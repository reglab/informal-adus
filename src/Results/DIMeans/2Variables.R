# 
# Generates a data.frame of formal and informal construction events, Neyman
# weights and the neighborhood- and parcel-level variables to be used in the
# difference in means tests
# Inputs:
#   - parcel_database_file: Includes APN, median income, and true and Neyman weights
#   - Complete set of formal and informal construction events
#   - Corelogic variables (parcel-level information)
#

library(readr)
library(tidyverse)
library(ggplot2)
library(tigris)
library(gtools)
library(censusapi)
library(sf)

Sys.setenv(CENSUS_KEY="INSERT CENSUS KEY")

# Parameters (construction event type, minimum square footage threshold, permit set)
# We use 2016 for ACS to get the data at baseline.
ACS_year <- 2016
cevent_type <- 'change_area1.2'
sqft_threshold <- 250
permit_set <- 'base' # expanded or base

# Prepare output file
out_file <- if (cevent_type == 'change_area1.2') 'ca12' else cevent_type
if (permit_set == 'expanded') out_file <- paste0(out_file, '_expanded')
out_file <- paste0(out_file, '_', sqft_threshold)

# 0. Data files ----------------
# Data frame of SJ residential parcels (to get the census block groups)
parcel_db_file <- 'data/raw/parcel_database.csv'

# Annotated construction events (formal and informal)
cevents_file <- paste0('data/processed/DIMeans/b3_cevents_', 
                       out_file, '_formal_informal.csv')

# CoreLogic data for each of the parcels with annotated constructions or formal permits
cl_file <-  'data/raw/DIMeans/corelogic_may1223.csv'
if (permit_set == 'expanded') {
  cl_file <- 'data/raw/DIMeans/corelogic_expanded_permits.csv'
}

# 1. Load data ----------------
parcel_db <- read.csv(
  parcel_db_file, colClasses=c("APN"="character", "GEOID"="character"))
cevents <- read.csv(cevents_file, colClasses=c("APN"="character"))
cl_data <- read.csv(cl_file, colClasses=c("APN"="character"))

# 2. Add CBG-level variables -----------------
options(tigris_use_cache = TRUE)
counties <- counties("CA", year=ACS_year)

# API Descriptions: https://api.census.gov/data.html
# ACS Variables: https://api.census.gov/data/2016/acs/acs5/variables.html
acs_data <- counties$COUNTYFP %>% 
  map_dfr(function(county){
    
    print(county)
    
    age_income <- getCensus(
      name = "acs/acs5",
      vintage = ACS_year,
      region = "block group:*",
      regionin = paste0("state:06+county:", county),
      vars = c(
        # Income, rent, HHs
        "B19013_001E", # HH Median income 
        "B19001_001E", # Number of households
        "B25064_001E", # Median gross rent (dollars)
        "B25058_001E", # Median contract rent (dollars)
        
        # Av Housing Vacancy Rate
        "B25004_003E", # Vacancy status: Rented, not occupied
        "B25004_004E", # Vacancy status: For sale only
        "B25004_005E", # Vacancy status: Sold, not occupied
        "B25004_002E", # Vacancy status: For rent
        "B25002_002E", # Total Occupied
        
        # Prop of renter occupied
        "B25003_003E", # Total Renter Occupied
        
        # Population
        "B01003_001E", # Total Population
        "B02001_002E", # White alone (Hispanic + Not Hispanic)
        "B02001_003E", # Black or African American alone (H+NH)
        "B02001_004E", # American Indian and Alaska Native alone (H+NH)
        "B02001_005E", # Asian alone (H+NH)
        "B02001_006E", # Native Hawaiian OPI alone (H+NH)
        
        "B03002_012E", # Total Hispanic or Latino origin by race
        "B03002_003E", # Not Hispanic - White alone
        "B03002_004E", # Not Hispanic - Black / AA alone
        "B03002_006E", # Not Hispanic - Asian alone
        
        # Overcrowding
        "B25014_001E", # Total housing units: Tenure by occupants per room
        "B25014_005E", # Owner occupied: 1.01 to 1.50 occupants / room
        "B25014_006E", # Owner occupied: 1.51 to 2.00 occupants / room
        "B25014_007E", # Owner occupied: 2.01+
        "B25014_011E", # Renter occupied: 1.01 to 1.50 occupants / room
        "B25014_012E", # Renter occupied: 1.51 to 2.00 occupants / room
        "B25014_013E" # Renter occupied: 2.01+
      )
    ) %>% 
      transmute(
        # GEOIDs and income/rent/HHs
        GEOID = paste0(state,county,tract,block_group),
        hhmedinc_2016 = ifelse(B19013_001E < 0, NA, B19013_001E),
        hh_2016 = B19001_001E,
        med_gross_rent_2016 = ifelse(B25064_001E < 0, NA, B25064_001E),
        med_contract_rent_2016 = ifelse(B25058_001E < 0, NA, B25058_001E),
        
        # Available Housing Vacancy Rate 
        # def https://www2.census.gov/programs-surveys/acs/tech_docs/subject_definitions/2016_ACSSubjectDefinitions.pdf
        occupied = B25002_002E,
        vac_for_sale_only = B25004_004E,
        vac_sold_not_occu = B25004_005E,
        vac_for_rent = B25004_002E,
        vac_rented_not_occu = B25004_003E,
        housing_inv = occupied + vac_for_sale_only + vac_sold_not_occu + vac_for_rent + vac_rented_not_occu,
        av_housing_vac_rate_2016 = 100 * (vac_for_sale_only + vac_for_rent) / housing_inv, 
        
        # Proportion of renter-occupied units
        prop_renter_occu_2016 = B25003_003E / B25002_002E,
        
        # Population data
        latinx_perc_2016 = ifelse(B01003_001E == 0, NA, B03002_012E/B01003_001E * 100),
        white_perc_2016 = ifelse(B01003_001E == 0, NA, B02001_002E/B01003_001E * 100),
        black_perc_2016 = ifelse(B01003_001E == 0, NA, B02001_003E/B01003_001E * 100),
        asian_perc_2016 = ifelse(B01003_001E == 0, NA, B02001_005E/B01003_001E * 100),
        
        NH_white_perc_2016 = ifelse(B01003_001E == 0, NA, B03002_003E/B01003_001E * 100),
        NH_black_perc_2016 = ifelse(B01003_001E == 0, NA, B03002_004E/B01003_001E * 100),
        NH_asian_perc_2016 = ifelse(B01003_001E == 0, NA, B03002_006E/B01003_001E * 100),
        
        # Overcrowding
        # Percentage of owner and renter-occupied units > 1 occupants per room
        overcrowding_perc_2016 = 100 * (B25014_005E + B25014_006E + B25014_007E + B25014_011E + B25014_012E + B25014_013E) / B25014_001E,
        severe_overcrowding_perc_2016 = 100 * (B25014_006E + B25014_007E + B25014_012E + B25014_013E) / B25014_001E
      ) %>%
      dplyr::select(
        GEOID, hhmedinc_2016, hh_2016, med_gross_rent_2016, 
        med_contract_rent_2016, 
        av_housing_vac_rate_2016,
        prop_renter_occu_2016, 
        latinx_perc_2016, white_perc_2016, black_perc_2016, asian_perc_2016, 
        NH_white_perc_2016, NH_black_perc_2016, NH_asian_perc_2016,
        overcrowding_perc_2016, severe_overcrowding_perc_2016
        )
  })

# Add land area
cbgs_2016 <- block_groups("CA", year = 2016)
acs_data <- acs_data %>% left_join(
  cbgs_2016 %>% dplyr::select(GEOID, ALAND), by='GEOID') %>%
  dplyr::mutate(land_area_sqkm_2016 = ALAND / 1000000) %>%
  dplyr::mutate(hh_lasqkm_2016 = hh_2016 / land_area_sqkm_2016)

# 3. Consolidate ==========
final <- cevents %>%
  dplyr::left_join(parcel_db %>% dplyr::select(APN, GEOID), by='APN') %>%
  dplyr::left_join(acs_data %>% dplyr::select(-geometry), by='GEOID') %>%
  dplyr::left_join(cl_data, by='APN')

# Clean
final <- final %>%
  dplyr::select(-APN..PARCEL.NUMBER.UNFORMATTED., 
                -BUILDING.SQUARE.FEET.IND,  
                -ALAND, -geometry)

# Save
write.csv(
  final, 
  paste0('data/processed/DIMeans/b3_cevents_', out_file, '_DIMs.csv'),
  row.names=FALSE)
