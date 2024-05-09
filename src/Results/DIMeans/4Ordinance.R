#
# Calculates the implications of SJ's ADU ordinance on the subset of 
# construction events that could potentially be permitted, by looking at the
# minimum lot size requirement over time
#

library(tidyverse)
library(sf)

# Parameters (construction event type, minimum square footage threshold)
cevent_type <- 'change_area1.2'
sqft_threshold <- 250

# 0. Load data =======
# Formal events
final_permits_file <- 'data/processed/Permits/Final_Permits.csv'
final_permits <- read.csv2(final_permits_file, colClasses=c("APN"="character"))

# Construction events (formal and informal)
out_file <- if (cevent_type == 'change_area1.2') 'ca12' else cevent_type
out_file <- paste0(out_file, '_', sqft_threshold)
cevents_file <- paste0(
  'data/processed/DIMeans/b3_cevents_', out_file, '_formal_informal.csv')
cevents <- read.csv(cevents_file, colClasses=c("APN"="character"))

formal_adus <- final_permits %>% 
  dplyr::filter(event_type == 'Addition' & event_201620 == TRUE) %>%
  dplyr::mutate(IssueDate = as.Date(IssueDate, '%Y-%m-%d'))

informal_adus <- cevents %>%
  dplyr::filter(type == 'informal')

# SJ residential parcels shapefile
sj_parcels_res_file <- 'data/raw/sj-parcels-res-cbgs'
sj_parcels_res <- st_read(sj_parcels_res_file)

# 1. Compute lot size =======
sj_parcels_areas <- sj_parcels_res %>% st_transform(crs=26910)
sj_parcels_areas <- sj_parcels_areas %>%
  dplyr::mutate(lot_size = st_area(sj_parcels_areas))
sj_parcels_areas <- st_drop_geometry(sj_parcels_areas)
sj_parcels_areas <- sj_parcels_areas %>%
  dplyr::mutate(lot_size_sqft = as.numeric(lot_size * 10.7639))

# Get lot sizes for each construction event
formal_adus <- formal_adus %>% 
  dplyr::left_join(sj_parcels_areas %>% dplyr::select(APN, lot_size_sqft), by='APN') %>%
  dplyr::filter(!is.na(lot_size_sqft))

informal_adus <- informal_adus %>%
  dplyr::left_join(sj_parcels_areas %>% dplyr::select(APN, lot_size_sqft), by='APN')

# 2. Ordinance over time ======
# Number of ADU permits issued. 
# ADU ordinance timeline: 
# https://www.sanjoseca.gov/business/development-services-permit-center/accessory-dwelling-units-adus/adu-ordinance-updates

formal_adus <- formal_adus %>%
  dplyr::mutate(Ordinance = case_when(
    IssueDate < as.Date('2018-07-27', '%Y-%m-%d') ~ 0, # Prior to July 27, 2018
    IssueDate >= as.Date('2018-07-27', '%Y-%m-%d') & 
      IssueDate < as.Date('2019-12-17', '%Y-%m-%d') ~ 1, # July 27, 2018 - Dec 16, 2019
    IssueDate >= as.Date('2019-12-17', '%Y-%m-%d') ~ 2 # On or after Dec 17, 2019
  ))

table(formal_adus$Ordinance)

# Formal compliance
formal_adus <- formal_adus %>%
  dplyr::mutate(Compliant = case_when(
    Ordinance == 0 & lot_size_sqft >= 5445 ~ 1, 
    Ordinance == 1 & lot_size_sqft >= 3000 ~ 1, 
    Ordinance == 2 ~ 1, 
    TRUE ~ 0
  ))

table(formal_adus$Compliant)

# Note: for the one formal unit that is marked as not compliant the lot size on
# building plans is above the 5,445 threshold.
# (see http://csjpbce.sanjoseca.gov/ecmsviewer/614/469/16469614.pdf)

# Informal compliance
informal_adus <- informal_adus %>%
  dplyr::mutate(
    Compliant0 = ifelse(lot_size_sqft >= 5445, 1, 0),
    Compliant1 = ifelse(lot_size_sqft >= 3000, 1, 0),
  )

table(informal_adus$Compliant0)
1 - sum(informal_adus$w * informal_adus$Compliant0) / sum(informal_adus$w)

table(informal_adus$Compliant1)

