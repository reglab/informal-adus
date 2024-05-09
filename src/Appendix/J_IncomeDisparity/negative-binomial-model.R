# 
# Estimates the Weighted Negative Binomial model on the stratified sample 
# of annotated (informal) construction events, and
# on the cleaned permit set (formal construction events)
#

library(readr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(stargazer)
library(WeightIt)

# Data frame of SJ residential parcels
parcel_db_file <- 'data/raw/parcel_database.csv'

# Data frame of all 15,006 parcels sampled in our stratified sample
batch3_file <-'data/raw/complete_sampled_APNs.csv'

# Data frame of annotated construction events (classified as permitted/unpermitted)
# larger than 120 sqft
batch3_constr_file <- 
  'data/processed/permitted_cevents_change_area12_120.csv'

# Data frame of cleaned ADU permits
final_permits_file <-'data/processed/Permits/Final_Permits.csv'

# * Load and prep data 
# Batch 3 parcels and construction events
parcel_db <- read.csv(parcel_db_file, colClasses=c("APN"="character"))
batch3 <- read.csv(batch3_file, colClasses=c("APN"="character"))
batch3_events <- read.csv2(batch3_constr_file, colClasses=c("APN"="character"))
n <- dim(batch3)[1]
stopifnot(n == 15006)

# Divide median income per 10k
parcel_db <- parcel_db %>% dplyr::mutate(median_inc_10k = median_inc / 10000)

# Filter for parcels in our stratified sample
batch3 <- parcel_db %>% dplyr::filter(APN %in% unique(batch3$APN))

# Define WLS weights: inverse of the Prob of selection
batch3 <- batch3 %>% dplyr::mutate(w = true_weight / Neyman_weight) 

# 1. Total model on stratified sample ----------
batch3_agg <- batch3_events %>%
  dplyr::mutate(total=permitted+unpermitted) %>%
  dplyr::group_by(APN) %>% dplyr::summarize(
    g = sum(total))

batch3_agg <- batch3 %>% dplyr::left_join(batch3_agg, by='APN')
stopifnot(dim(batch3_agg)[1] == n)

batch3_agg <- batch3_agg %>% tidyr::replace_na(list(g=0))

# Check
print(paste0('[INFO] Number of construction events: ', sum(batch3_agg$g)))

s.model <- glm.nb(g ~ median_inc_10k, data = batch3_agg, weights=w)

# 2. Unpermitted model --------------------

# Aggregate unpermitted construction events at the parcel level
batch3_agg_unperm <- batch3_events %>%
  dplyr::group_by(APN) %>% dplyr::summarize(
    g = sum(unpermitted))

# Merge and fill missings
batch3_WLS_unperm <- batch3 %>% dplyr::left_join(batch3_agg_unperm, by='APN')
stopifnot(dim(batch3_WLS_unperm)[1] == n)

batch3_WLS_unperm <- batch3_WLS_unperm %>% tidyr::replace_na(list(g=0))

# Check
print(paste0('[INFO] Number of informal events: ', sum(batch3_WLS_unperm$g)))
colSums(is.na(batch3_WLS_unperm))

# * Unpermitted
u.model <- glm.nb(g ~ median_inc_10k, data = batch3_WLS_unperm, weights=w)

# 2. Permitted model -------------------
# Final permit data
final_permits <- read.csv2(final_permits_file, colClasses=c("APN"="character"))

# Filter to additions taking place 2016-2020 (true constructions) and aggregate
# at the parcel level
final_permits <- final_permits %>%
  dplyr::filter(event_type == 'Addition' & event_201620 == TRUE) %>%
  dplyr::group_by(APN) %>%
  dplyr::summarize(g = n())
num_permits <- sum(final_permits$g)

# Add to full set of residential parcels (note that 13 parcels do not
# have a match in the residential parcel dataset)
perm_parcel_db <- parcel_db %>% 
  dplyr::left_join(final_permits, by='APN') %>% 
  tidyr::replace_na(list(g = 0))
print(paste0(
  '[INFO] ', num_permits - sum(perm_parcel_db$g), 
  ' parcels unavailable in the residential parcels dataset'))
print(paste0('[INFO] Number of formal constructions: ', sum(perm_parcel_db$g)))

p.model <- glm.nb(g ~ median_inc_10k, data = perm_parcel_db)
