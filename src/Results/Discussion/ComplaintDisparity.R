#
# Computes the difference in mean CBG-level income between parcels that
# were and were not associated with complaints during 2016-2020
#

library(esri2sf)
library(sf)
library(tidyverse)
library(tableone)

# Load data on 159k SJ Residential parcels
parcel_db_file <- 'data/raw/parcel_database.csv'
parcel_db <- read.csv(parcel_db_file, colClasses=c("APN"="character"))

# Load complaint data
complaints <- esri2sf(
  "https://geo.sanjoseca.gov/server/rest/services/PLN/PLN_PermitsAndComplaints/MapServer/1")

# Parse dates
complaints <- complaints %>% 
  mutate(
    open_date = (OPENDATE/1000) %>% as.POSIXct(origin = "1970-01-01"),
    last_update_date = (LASTUPDATE/1000) %>% as.POSIXct(origin = "1970-01-01"))

# Filter for complaints during 2016-2020
complaints <- complaints %>% 
  dplyr::filter(
    open_date >= as.Date('05/01/2016', format="%m/%d/%Y") &
      open_date < as.Date('06/01/2020', format="%m/%d/%Y"))

# Compute a binary indicator at the parcel level of the presence of a complaint
complaints_apn <- complaints %>%
  dplyr::group_by(APN) %>%
  dplyr::summarize(num_complaints=n())

parcel_db <- parcel_db %>%
  dplyr::mutate(complaint_201620 = APN %in% complaints_apn$APN)

# Difference in means test
myVars <- c('median_inc')
tableOne <- CreateTableOne(
  vars = myVars, strata = "complaint_201620" , data = parcel_db,
  test=TRUE, testApprox='svyttest')

