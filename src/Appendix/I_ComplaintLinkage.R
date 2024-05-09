#
# Estimates a general complaint rate per parcel for the parcels in which
# we found unpermitted ADU construction events, and computes the number of
# complaints related to informal dwelling unit additions.
#

library(esri2sf)
library(tidyverse)
library(sf)

# 0. Load data ========
# Load complaints data
complaints <- esri2sf(
  "https://geo.sanjoseca.gov/server/rest/services/PLN/PLN_PermitsAndComplaints/MapServer/1")

# Load annotated construction events
cevents_file <- 'data/processed/DIMeans/b3_cevents_ca12_250_DIMs.csv'
cevents <- read.csv(
  cevents_file, colClasses=c(
    "APN"="character", "GEOID"="character")) %>%
  dplyr::select(APN, w, type, GEOID)

# 1. Pre-process the complaints data =======
complaints <- complaints %>% 
  mutate(open_date = (OPENDATE/1000) %>% as.POSIXct(origin = "1970-01-01"), 
         last_update_date = (LASTUPDATE/1000) %>% as.POSIXct(origin = "1970-01-01")) %>%
  mutate(open_year = as.numeric(substr(open_date, 1, 4)), 
         last_update_year = as.numeric(substr(last_update_date, 1, 4)))

# We filter for complaints May 2016-onward as the construction events took place 2016-2020
# We limit to complaints by July 7, 2023 for reproducibility, as complaints continue
# to be added to this database regularly. Note that we don't use the last_update_date
# as this seems to be updated regularly for all complaints regardless of their open
# date.
complaints <- complaints %>% 
  dplyr::filter(open_date >= as.Date('01-05-2016', format='%d-%m-%Y') & 
                  open_date <= as.Date('07-07-2023', format='%d-%m-%Y'))
complaints_all <- complaints

# 2. Linkage to parcels with ADU constructions (APN-based linkage) =======
# (Note: Spatial linkage results in exactly the same linked complaints)
complaints <- complaints %>% dplyr::filter(APN %in% cevents$APN)

# 3. Complaint rate =======
informal <- cevents %>% dplyr::filter(type == 'informal') 

informal <- informal %>%
  left_join(complaints %>% dplyr::select(APN, DESCRIPTION) %>% st_drop_geometry(), 
            by = 'APN')

informal <- informal %>%
  dplyr::mutate(complaint = case_when(is.na(DESCRIPTION) ~ 0, TRUE ~ 1))

print('[INFO] Neyman-weighted complaint rate on informal constructions (construction-level)')
sum(informal$w * informal$complaint) / sum(informal$w)

informal_parcel <- informal %>% 
  dplyr::group_by(APN) %>% 
  dplyr::summarise(w=dplyr::first(w), complaint=max(complaint))

complaint_rate_informal_sample <- sum(informal_parcel$w * informal_parcel$complaint) / sum(informal_parcel$w)
print('[INFO] Neyman-weighted complaint rate on parcels with informal constructions (parcel-level)')
complaint_rate_informal_sample

# 4. Complaints related to informal dwelling unit additions =======
complaints_idu <- complaints_all %>%
  dplyr::filter(!(PROGRAM %in% c('Massage Parlor', 'Medical Marijuana', 'Shopping Carts', 
                          'Special  Council', 'Special', 'Tobacco', 'Vacant Building',
                          'Veh Abate/ Private Prop', 'Vehicle Abatement'))) %>%
  dplyr::mutate(DESC_up = toupper(DESCRIPTION)) %>%
  dplyr::mutate(idu_related = case_when(
    grepl('OCCUPIED|SECOND UNIT|DIVIDED|BUILDING ADDITION|BUILT ADDITION|CONVERSION|PEOPLE LIVING|ILLEGAL UNIT|
               ILLEGAL OCCUPANCY|OVERCROWDING|ILLEGAL STRUCTURE|ILLEGAL OCCUPANCY|
               CONVERTING|CONSTRUCTION W/O PERMITS|2ND UNIT| ADU |ADU |ACCESSORY|DWELLING UNIT|
               RENTING UNPERMITTED|BUILT A UNIT|ADDING ROOMS|UNPERMITTED STRUCTURE|SECOND UNIT|
               RENTAL UNIT|ILLEGAL STRUCTURE|LLEGALLY RENTING|DETACTED STRUCTURE|
               UNPERMITTED ROOM|RESIDENTIAL LIVING USE|USED FOR LIVING|LIVING IN|INDIVIDUALLY RENTED|
               BUILDING * UNITS|2ND HOUSE|PPL LIVING|BUILDING A DWELLING|A THIRD UNIT|A THRID UNIT|
               UNPERMITTED HOUSE|UNPERMITTED NEW ROOM|BUILT * ROOMS|BUILT * STRUCTURES|BUILT * STUDIO|
               UNPERMITTED MAKESHIFT|CONVERT|RENTED|TURNED INTO|SPLIT INTO|OCCUPANT LIVING|
               PEOPLE RENTING|PERSON LIVING|SUB LEASING|RENTAL UNIT|STRUCTURE BEING BUILT|
               STRUCTURE BUILT|TENANTS LIVING|MULTIPLE UNITS|UNPERMITTED DWELLING|UNPERMITTED DETACHED|
               UNPERMITTED STUDIO|GRANNY|COVERTED GARAGE|ILLEGAL ADDITIONS|ILLEGAL OCUPPANCY|
               DWELLING BEHIND THE HOUSE W/O PERMITS|ILLEGAL *OCCUPANCY|UNPERMITTED STUDIO|
               STUDIO WITHOUT PERMISSION|UNPERMITTED ATTACHED ADDITION BUILT|BUILDING UNPERMITTED HOUSE|
               ILLEGAL ADDITION OF STUDIO|ILLEGAL OCCUPANY|BUILDING A 2 STORY *|
               ADDED * BEDROOMS WITHOUT PERMITS|MULTIPLE ROOMS W/O PERMITS
          ', DESC_up) ~ 1,
    TRUE ~ 0
  ))

# Number of unique parcels with ADU-related complaints
parcels_idu_complaints <- complaints_idu %>% 
  dplyr::filter(idu_related == 1)
N_parcels_idu_comp <- length(unique(parcels_idu_complaints$APN))
print(paste0('Number of parcels with ADU-related complaints: ', N_parcels_idu_comp))

# Complaint rate on informal constructions
N_informal_sample <- dim(informal)[1]
no_complaint_sample <- (1 - complaint_rate_informal_sample) * N_informal_sample
complaint_rate_informal <- N_parcels_idu_comp / (N_parcels_idu_comp + no_complaint_sample)
print(paste0('Complaint rate on informal constructions: ', round(complaint_rate_informal * 100, 1), '%'))

