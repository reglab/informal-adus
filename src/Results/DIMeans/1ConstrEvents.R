# 
# Generates a data.frame of formal and informal construction events along with
# Neyman weights, and Issuance Date for formal events. 
# Inputs: 
#   - parcel_database_file: Includes APN, median income, and true and Neyman weights
#   - batch3_file: APNs from the stratified sample's Neyman allocation
#   - batch3_constr_file: Construction events from the stratified sample
#   - final_permits_file: Complete set of permits

library(readr)
library(tidyverse)
library(ggplot2)
library(stringr)

# Parameters (construction event type, minimum square footage threshold, permit set)
cevent_type <- 'change_area1.2'
sqft_threshold <- 250
permit_set <- 'base' # expanded or base

# Prepare output file
out_file <- if (cevent_type == 'change_area1.2') 'ca12' else cevent_type
if (permit_set == 'expanded') out_file <- paste0(out_file, '_expanded')
out_file <- paste0(out_file, '_', sqft_threshold)

# 0. Data files ==========
# Parcel database
parcel_db_file <- 'data/raw/parcel_database.csv'

# Batch 3 sample
batch3_file <- 'data/raw/complete_sampled_APNs.csv'

# Batch 3 construction events (classified as permitted/unpermitted)
batch3_constr_file <- 
  paste0('data/processed/permitted_cevents_', 
         gsub('.', '', cevent_type, fixed=TRUE), '_', sqft_threshold, '.csv')

# Final permits
final_permits_file <- 'data/processed/Permits/Final_Permits.csv'

# 1. Unpermitted events --------------------
# * Load and prep data 
# Stratified sample parcels and construction events
parcel_db <- read.csv(parcel_db_file, colClasses=c("APN"="character"))
batch3 <- read.csv(batch3_file, colClasses=c("APN"="character"))
batch3_events <- read.csv2(batch3_constr_file, colClasses=c("APN"="character"))
n <- dim(batch3)[1]
stopifnot(n == 15006)

# Filter for parcels in the stratified sample
batch3 <- parcel_db %>% dplyr::filter(APN %in% unique(batch3$APN))

# Define WLS weights: inverse of the Prob of selection
batch3 <- batch3 %>% dplyr::mutate(w = true_weight / Neyman_weight) 

# Aggregate unpermitted construction events at the parcel level
batch3_agg_unperm <- batch3_events %>%
  dplyr::group_by(APN) %>% dplyr::summarize(
    g = sum(unpermitted))

# Merge and fill missings
batch3_WLS_unperm <- batch3 %>% dplyr::left_join(batch3_agg_unperm, by='APN')
stopifnot(dim(batch3_WLS_unperm)[1] == n)

batch3_WLS_unperm <- batch3_WLS_unperm %>% 
  tidyr::replace_na(list(g=0))

# Disaggregate
informal <- batch3_WLS_unperm %>% 
  dplyr::filter(g > 0) %>%
  uncount(g)

# 2. Permitted events -------------------
# Clean permit data
final_permits <- read.csv2(final_permits_file, colClasses=c("APN"="character"))

# Filter to desired permit set
if (permit_set == 'base') {
  # additions taking place 2016-2020 (true constructions)
  final_permits <- final_permits %>%
    dplyr::filter(event_type == 'Addition' & event_201620 == TRUE)
} else if (permit_set == 'expanded') {
  print('[INFO] Using EXPANDED permit set includding additions with
        Issue Date before May 1, 2020')
  # additions with Issue Date < 2020-05-01
  final_permits <- final_permits %>%
    dplyr::filter(
      event_type == 'Addition' & (
        IssueDate < as.Date('2020-05-01', format='%Y-%m-%d') | event_201620 == TRUE))
} else {
  stopifnot(FALSE)
}

num_permits <- dim(final_permits)[1]

# Filter to residential parcels
formal <- final_permits %>%
  dplyr::filter(APN %in% parcel_db$APN) %>%
  dplyr::mutate(w=1)

# 3. Concatenate and save =========

# Concatenate
total <- informal
total <- total %>% dplyr::mutate(type='informal', IssueDate=NA, FOLDERNUMBER=NA)
total <- rbind(
  total %>% 
    dplyr::select(APN, w, type, IssueDate, FOLDERNUMBER), 
  formal %>% 
    dplyr::mutate(type='formal') %>% 
    dplyr::select(APN, w, type, IssueDate, FOLDERNUMBER)
)

# Save
dir.create('data/processed/DIMeans/')
write.csv(
  total, 
  paste0('data/processed/DIMeans/b3_cevents_', out_file, 
         '_formal_informal.csv'), 
  row.names=FALSE)
