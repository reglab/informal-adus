#
# Classifies each of the construction events in our stratified sample
# as being permitted or unpermitted, using data from the detached SJ list
# and the SJ P&PI portal. 
# Computes the Neyman-weighted proportion of unpermitted ADU constructions
# via the permit-matching method.
# Output:
#   - permitted_cevents_changearea12_250.csv including number of permitted and
#   unpermitted events for each of the APNs where we found construction events.
#

library(tidyverse)
library(WeightIt)

# Parameters: type of construction event and minimum square footage threshold
cevent_type <- 'change_area1.2'
sqft_threshold <- 250

# 0. Load data ================================

# Scraped permit-matching on our set of annotated construction events 
construction_file <- paste0(
  'data/processed/Permits/b3_apns_p_', 
  gsub('.', '', cevent_type, fixed=TRUE), '_', sqft_threshold, '.csv')

# Database of SJ residential parcels
parcel_db_file <- 'data/raw/parcel_database.csv'

# Cleaned permits (marked as additions and whether they took place during the period)
clean_permits_file <- 'data/processed/Permits/Adu_Detach_Cleaned_2020-06-01.csv'

# SJ Detached ADU list
p_dADU_file <- 'data/raw/Permits/Adu_Detach.csv'

c_events <- read.csv(construction_file, colClasses=c("APN"="character"))
clean_permits <- read.csv2(clean_permits_file, colClasses=c("Final.APN"="character")) %>%
  dplyr::mutate(APN = Final.APN) %>% dplyr::select(-Final.APN)
p_dADU <- read.csv(p_dADU_file, colClasses=c("APN"="character"))
parcel_db <- read.csv(parcel_db_file, colClasses=c("APN"="character"))

# Prepare data frames
# Add Neyman weights to construction events
c_events <- c_events %>% left_join(
  parcel_db %>% 
    plyr::mutate(w = true_weight / Neyman_weight) %>% dplyr::select(APN, w), by='APN')
# Rename specific construction event type to generic column
c_events <- c_events %>% dplyr::mutate(
  event = !!rlang::sym(cevent_type)
)

print(paste0(
  '[INFO] Number of ', cevent_type, ' construction events: ', 
  sum(c_events$event)))

# 1. SJ P&P Portal Permits ======================
# Note: these are matches to permits with the following description
# 'Permit Type' equals '1 & 2 Family Detached Dwellings' and
# 'Work Type' equals 'New Construction' and
# 'Folder Name' contains the string '2' or the string 'SECOND'
c_events <- c_events %>% 
  dplyr::mutate(SJPP_permits = num_permits) %>% 
  dplyr::select(-num_permits) %>%
  dplyr::mutate(SJPP_permitted = pmin(event, SJPP_permits)) %>%
  dplyr::mutate(SJPP_unpermitted = event - SJPP_permitted)

# 2. Detached ADU (DADU) list of 556 permits provided by SJ =======
# Filter permits issued 2015-2020
p_dADU <- p_dADU %>% dplyr::mutate(c_year = substr(DATE, 1, 4))
p_dADU <- p_dADU %>% dplyr::filter(c_year %in% c(2015:2020))
N_dADUs <- dim(p_dADU)[1]

# Aggregate permits at the parcel level
p_dADU <- p_dADU %>% dplyr::group_by(APN) %>% 
  dplyr::summarize(DADU_permits = n(), )

c_events <- c_events %>% 
  left_join(p_dADU, by = 'APN') %>%
  tidyr::replace_na(list(DADU_permits = 0))

c_events <- c_events %>% 
  dplyr::mutate(DADU_permitted = pmin(event, DADU_permits)) %>%
  dplyr::mutate(DADU_unpermitted = event - DADU_permitted)

# 3. Compare permit matches ==========================
print(paste0('Total construction events: ', sum(c_events$event)))

# SJ P&P
print('[INFO] Using the portal information')
print(paste0('Total permitted events: ', sum(c_events$SJPP_permitted)))

# DADU
print('[INFO] Using the DADU list')
print(paste0('Total permitted events: ', sum(c_events$DADU_permitted)))

# 4. Handle discrepancies ===============
# Check discrepancies between scraping and DADU list 
discrepancies <- c_events %>% 
  dplyr::filter(SJPP_permits != DADU_permits & DADU_permits == 0) %>% 
  dplyr::select(APN, event, SJPP_permits, DADU_permits)

# Note: These APNs were manually validated in the SJPP portal and include detached
# ADU permits that were either finaled or expired with an approved rough frame. 
# These all had a single construction event. 
discrepancy_APNs <- c(
  "24936002", # Foldernum: 2018-104414 Finaled
  "24957042", # Foldernum: 2018-143196 Finaled
  "26140090", # Foldernum: 2017-036636 Expired, Rough Frame Approved
  "26462005", # Foldernum: 2017-022929 Expired, Rough Frame Approved
  "43947013", # Foldernum: 2013-128719 Finaled
  "49724012", # Foldernum: 2017-011294 Expired, Rough Frame Approved
  "49902048", # Foldernum: 2017-011439 Expired, Rough Frame Approved
  "70637024", # Foldernum: 2014-031138 Finaled
  "59944031", # Foldernum: 2019-104826 Expired
  "26157028", # Foldernum: 2018-144691 Expired, Rough Frame Approved
  "26450057", # Foldernum: 2018-145574 Expired, Conversion
  "43927010", # Foldernum: 2019-119040 Finaled, 
  "70613024" # Foldernum: 2019-142112-RS "1000 SQFT ADU"
)

not_discrepancy_APNs <- c(
  "28251041", # Foldernum: 2016-123093 & 2016-137497; Guest house
  "59949012" # Foldernum: 2019-149759; Fee estimates
)

# Re-classify permitted status of APNs listed above
c_events <- c_events %>%
  dplyr::mutate(
    permitted = case_when(
      APN %in% discrepancy_APNs ~ as.integer(1), 
      TRUE ~ DADU_permits)) %>%
  dplyr::mutate(unpermitted = event - permitted)
stopifnot(c_events$event == c_events$permitted + c_events$unpermitted)

# 5. Final output and save ==========
print('[INFO] Final version')
print(paste0('Total permitted events: ', sum(c_events$permitted)))
permitted_rate <- sum(c_events$permitted * c_events$w) / sum(c_events$event * c_events$w) * 100
print(paste0('Neyman-weighted proportion: ',round(permitted_rate, 1), '%'))
print(paste0('[INFO] Added permits: ', sum(c_events$permitted) - sum(c_events$DADU_permits)))

write.csv2(
  c_events %>% dplyr::select(APN, permitted, unpermitted), 
  paste0('data/processed/permitted_cevents_', 
         gsub('.', '', cevent_type, fixed=TRUE), '_', sqft_threshold, '.csv')
)

# 6. Confidence intervals =========
# * Compute Effective Sample Size (ESS)

# Define WLS weights: inverse of the Prob of selection
parcel_db <- parcel_db %>% dplyr::mutate(w = true_weight / Neyman_weight) 

# Aggregate unpermitted construction events at the parcel level
batch3_perm <- c_events %>%
  dplyr::select(APN, permitted) %>%
  dplyr::filter(permitted > 0) %>%
  uncount(permitted) %>%
  dplyr::mutate(type = 'permitted') %>%
  dplyr::select(APN, type)

batch3_unperm <- c_events %>%
  dplyr::select(APN, unpermitted) %>%
  dplyr::filter(unpermitted > 0) %>%
  uncount(unpermitted) %>%
  dplyr::mutate(type = 'unpermitted') %>%
  dplyr::select(APN, type)

batch3 <- rbind(batch3_perm, batch3_unperm)

batch3 <- batch3 %>% 
  dplyr::left_join(parcel_db %>% dplyr::select(APN, w), by='APN')

# Compute CIs
p_hat <- 1 - permitted_rate / 100
n <- ESS(batch3$w)

margin <- 1.96 * sqrt(p_hat * (1- p_hat) / n)

CI <- p_hat + c(-margin, margin)
print(paste0('Unpermitted proportion: ', round(p_hat * 100, 2), '%'))
print(paste0('95% Confidence interval: ', round(CI[1] * 100, 2), '% - ', 
             round(CI[2] * 100, 2), '%'))
