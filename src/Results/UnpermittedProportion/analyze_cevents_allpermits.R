#
# Classifies each of the construction events in our stratified sample
# as being permitted or unpermitted, using data from the detached SJ list
# and the SJ P&PI portal. 
# This script uses a more liberal notion of permit, not limited to just the 
# ADU permits in the portal or in SJ's detached ADU list.
#

library(tidyverse)
library(WeightIt)

# Parameters (construction type, minimum square footage threshold)
cevent_type <- 'change_area1.2'
sqft_threshold <- 250

# 0. Load data =======
# Data frame of residential parcels in SJ
parcel_db_file <- 'data/raw/parcel_database.csv'
parcel_db <- read.csv(parcel_db_file, colClasses=c("APN"="character"))
parcel_db <- parcel_db %>%
  dplyr::mutate(w = true_weight / Neyman_weight)

# Annotated construction events in our sample of 15,006 parcels
c_events <- read.csv(
  'data/raw/processed_buildings.csv', 
  colClasses=c('apn'='character'))

# Permits obtained from the SJ P&P portal: note that it's okay to load the 120
# sqft version here and not the specific sqft_threshold version since we do a
# left_join to the permits in the actual set of construction events
scraped_permits_file <- paste0(
  'data/processed/Permits/b3_apns_allp_', 
  gsub('.', '', cevent_type, fixed=TRUE), '_120.csv')
c_events_permits <- read.csv(scraped_permits_file, colClasses=c("APN"="character"))

# List of 556 detached ADUs provided by SJ
p_dADU_file <- 'data/raw/Permits/Adu_Detach.csv'
p_dADU <- read.csv(p_dADU_file, colClasses=c("APN"="character"))

# Step 1: Define and clean missing permits for lower bound ====================
c_events <- c_events %>%
  dplyr::mutate(APN=apn) %>%
  dplyr::filter(`X2016_a` >= 11.15 | `X2020_a` >= 11.15) %>%
  dplyr::filter_at(vars(cevent_type), all_vars(. > 0)) %>%
  dplyr::mutate(area_2020_sqft = X2020_a * 10.7639) %>% # sqm to sqft area conversion
  dplyr::filter(area_2020_sqft >= sqft_threshold)

# Aggregate at the APN level
c_events_apns <- c_events %>% 
  dplyr::group_by(APN) %>% 
  summarise(n = n()) %>%
  dplyr::left_join(parcel_db %>% dplyr::select(APN, w), by='APN')

# Get more liberal set of permits and filter to 1 & 2 Family Detached 
# Dwellings - New Constructions or Additions/Alterations
c_events_permits <- c_events_permits %>%
  dplyr::mutate(ExpirationDate = as.Date(Exp..Date, format="%m/%d/%Y"))

c_events_permits <- c_events_permits %>%
  dplyr::filter(
    Permit.Type == '1 & 2 Family Detached Dwellings' &
      Work.Type %in% c('New Construction', 'Additions/Alterations'))

# De-duplicate permits
c_events_permits <- c_events_permits %>% 
  dplyr::mutate(folderrsn = as.character(folderrsn)) %>%
  dplyr::mutate(Address = str_trim(Address)) %>%
  dplyr::distinct(Address, folderrsn, .keep_all=TRUE)

# Estimate the raw permit rate
c_events_permits_apns_raw <- c_events_permits %>%
  dplyr::group_by(APN) %>% 
  dplyr::summarise(permits = n())
raw_permits <- c_events_apns %>%
  dplyr::left_join(c_events_permits_apns_raw, by='APN') %>%
  tidyr::replace_na(list(permits=0)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(permitted_events = min(n, permits)) %>%
  dplyr::mutate(unpermitted_events = n - permitted_events)
permitted_rate <- sum(
  raw_permits$permitted_events * raw_permits$w) / sum(raw_permits$n * raw_permits$w) * 100

# Manually review the permits that are not already covered by SJ's detached ADU list or
# in the ADU permits we scraped
not_covered <- c_events_permits %>%
  dplyr::filter(!APN %in% p_dADU$APN) %>%
  dplyr::filter(!(Work.Type == 'New Construction' & 
                    (grepl('SECOND', toupper(Folder.Name)) | grepl(2, Folder.Name)))) %>%
  dplyr::filter(!Status %in% c('Expired', 'Cancelled', 'Withdrawn')) %>%
  dplyr::filter(is.na(ExpirationDate) | ExpirationDate > as.Date("12/31/2015", format="%m/%d/%Y"))

# Step 2: Add permits for lower bound ==============================
# Load baseline construction event permit-matching to add non_covered, cleaned permits
c_events_baseline <- read.csv2(
  paste0('data/processed/permitted_cevents_', 
         gsub('.', '', cevent_type, fixed=TRUE), '_', sqft_threshold, '.csv'), 
  colClasses = c('APN'='character'))

c_events_baseline <- c_events_baseline %>%
  dplyr::left_join(parcel_db %>% dplyr::select(APN, w), by='APN')

# Load the manually reviewed permits.
# We dropped permits related to fee estimates, permits clearly marked as
# alterations/additions to the main building on the property, and permits 
# having a folder start date prior to 2014. Out of 201 reviewed permits (defined
# by the not_covered table), we keep 62 after this review process. 
permits_to_add <- read.csv(
  'data/raw/Permits/GSheets-June12-201permits.csv', 
  colClasses = c('APN'='character'))
permits_to_add <- permits_to_add %>%
  dplyr::filter(`Main.residence...Drop` == 0) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(APN=case_when(
    nchar(APN) == 7 ~ paste0('0', APN), 
    TRUE ~ APN))

# Manually review the permits associated with APNs that had more than 
# 1 construction event to see which permits from SJ's database we still need to
# include
multiple_cevents_inDADU <- c_events_apns %>% dplyr::filter(n > 1 & APN %in% p_dADU$APN)
multiple_cevents_inDADU_permits <- c_events_permits %>% 
  dplyr::filter(APN %in% multiple_cevents_inDADU$APN) %>%
  dplyr::mutate(folderrsn = as.character(folderrsn)) %>%
  dplyr::distinct(folderrsn, .keep_all=TRUE) %>%
  dplyr::filter(`Permit..` %in% c('2016-135853-RS', '2016-120703-RS', '2016-115121-RS'))

permits_to_add <- rbind(
  permits_to_add %>% dplyr::select(APN, folderrsn), 
  multiple_cevents_inDADU_permits %>% dplyr::select(APN, folderrsn)
)

# Group number of permits by APN
permits_to_add_apns <- permits_to_add %>%
  dplyr::group_by(APN) %>%
  dplyr::summarise(added_permits=n())

# Compute number of permitted and unpermitted events at the APN level
c_events_lowerb <- c_events_baseline %>%
  dplyr::left_join(permits_to_add_apns, by='APN') %>%
  tidyr::replace_na(list(added_permits = 0)) %>%
  dplyr::mutate(total_adus = permitted+unpermitted) %>%
  dplyr::mutate(total_permits = permitted + added_permits) %>%
  dplyr::mutate(updated_permitted = pmin(total_permits, total_adus)) %>%
  dplyr::mutate(updated_unpermitted = total_adus - updated_permitted)
  
# Compute the unpermitted rate using this liberal set of permits
lowerb_estimate_permitted_rate <- sum(
  c_events_lowerb$updated_permitted * c_events_lowerb$w) / sum(c_events_lowerb$total_adus * c_events_lowerb$w) * 100
print(paste0('[INFO] Lower bound estimate for the (N-W) unpermitted rate: ',
             round(100 - lowerb_estimate_permitted_rate, 1), '%' ))

# Output CSV of lower bound permit-matched construction events 
write.csv2(
  c_events_lowerb %>% 
    dplyr::mutate(permitted=updated_permitted, unpermitted=updated_unpermitted) %>%
    dplyr::select(APN, permitted, unpermitted, total_adus),
  paste0('data/processed/Permits/b3_apns_lowerbound_',
         gsub('.', '', cevent_type, fixed=TRUE), '_', sqft_threshold, '.csv')
)
