#
# Computes the number of 2016-2020 ADU permits per County and CBG in California using
# APR data
# Output:
#     - data/processed/City-analysis/aprs_regression2016.csv
#

library(tidyverse)
library(sf)
library(tigris)
library(gtools)
library(censusapi)

Sys.setenv(CENSUS_KEY="INSERT KEY HERE")

# Params
ACS_year <- 2016

# 1. Geocode APR addresses ==============
aprs <- read_csv("http://data.ca.gov/dataset/81b0841f-2802-403e-b48e-2ef4b751f77c/resource/fe505d9b-8c36-42ba-ba30-08bc4f34e022/download/table-a2-annual-building-activity-report-summary-new-construction-entitled-permits-and-completed.csv")

aprs_clean <- aprs %>% 
  filter(UNIT_CAT_DESC == 'Accessory Dwelling Unit') %>% 
  filter(YEAR %in% c(2016:2020)) %>% 
  mutate(
    index = row_number(),
    address = ifelse(
      !grepl("[0-9]$", STREET_ADDRESS),
      paste(STREET_ADDRESS, JURS_NAME, "CA", sep = ", "),
      STREET_ADDRESS
    )
  )

aprs_to_geocode <- aprs_clean %>% dplyr::select(index, address)

source("https://raw.githubusercontent.com/cengel/ArcGIS_geocoding/master/SUL_gcFunctions.R")

geocode_addresses <- NULL

for(x in 1:nrow(aprs_clean)){
  if(x%%1000==0) print(x)
  
  address <- tryCatch(
    geocodeSL(aprs_to_geocode$address[x]),
    error = function(e){
      data.frame(status = "fail")
    }
  )
  
  geocode_addresses <-
    geocode_addresses %>%
    bind_rows(
      aprs_to_geocode[x,] %>% 
        cbind(address)
    )
  
}

aprs_geocoded <- aprs_clean %>% left_join(geocode_addresses, by = "index")

# 2. Add ACS data ============================
aprs_tojoin <- aprs_geocoded %>% 
  filter(!addressType %in% c("Locality","POI", "Postal", "PostalExt", "StreetInt", "StreetName")) %>% 
  filter(grepl("California,", matchAddr))

options(tigris_use_cache = TRUE)
cbgs <- block_groups("CA", year = ACS_year)

ca_places <- places("CA", year = ACS_year) %>% dplyr::select(NAME)
cbgs_places <- cbgs %>% 
  dplyr::select(CBG = GEOID) %>% 
  st_centroid() %>% 
  st_join(ca_places)

aprs_joined <- aprs_tojoin %>% 
  st_as_sf(coords=c("lon","lat"), crs = 4326) %>% 
  st_transform(4269) %>% 
  st_join(cbgs %>% dplyr::select(CBG = GEOID))

aprs_regression <- aprs_joined %>% 
  st_drop_geometry() %>% 
  left_join(
    cbgs_places %>% st_drop_geometry()
  ) %>% 
  group_by(NAME, CNTY_NAME, CBG) %>% 
  count() %>% 
  ungroup()
write_csv(aprs_regression, paste0("data/processed/City-analysis/aprs_regression", ACS_year, ".csv"))

