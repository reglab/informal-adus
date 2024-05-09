#
# Generates a complete set of detached ADU permits (including the ones
# identified from SJ's detached list and the ones we find by scraping the APNs
# where we found construction events).
# Inputs:
#    - Cleaned permits file: Adu_Detach_Cleaned_2020-06-01.csv
#    - Annotated construction events file (with data on ADU permits): 
#      permitted_cevents_change_area12.csv

library(tidyverse)

# Load data
clean_permits_file <- 'data/processed/Permits/Adu_Detach_Cleaned_2020-06-01.csv'
construction_file <- 'data/processed/permitted_cevents_change_area12_250.csv'

c_events <- read.csv2(construction_file, colClasses=c("APN"="character"))
clean_permits <- read.csv2(clean_permits_file, colClasses=c("Final.APN"="character")) %>%
  dplyr::mutate(APN = Final.APN) %>% dplyr::select(-Final.APN)

final_permits <- clean_permits

# Find the 20 permits that are missing in the 270 subset. These are APNs
# that have construction events, and for which we found ADU permits through
# SJ's portal.
formal <- c_events %>% dplyr::filter(permitted > 0)

# Correct the 8 APNs found from construction events that we initially
# classified as not having a construction event during 2016-2020
final_permits <- final_permits %>%
  dplyr::mutate(event_201620 = case_when(
    APN %in% formal$APN ~ TRUE, 
    TRUE ~ event_201620
  ))

# The list of 556 permits provided by SJ gives us 42 permits that match to our 
# construction events. However, when looking up these permits in SJ's portal, 
# find an additional 13 missing permits that were not in the list of 556. 
# The code below adds these 13 permits, which we emphasize belong to APNs
# with permitted detached ADUs.
formal_missing <- formal %>% 
  dplyr::filter(!APN %in% final_permits$APN)

formal_missing <- formal_missing %>%
  dplyr::mutate(event_type='Addition', event_201620=TRUE, CompletionDate=NA, Description=NA, IssueDate=NA)

# Note that each APN has just 1 permit 
formal_missing <- formal_missing %>%
  dplyr::mutate(FOLDERNUMBER = case_when(
    APN == "24936002" ~ '18-104414',
    APN == "24957042" ~ '18-143196',
    APN == "26140090" ~ '17-036636',
    APN == "26462005" ~ '17-022929',
    APN == "43947013" ~ '13-128719',
    APN == "49724012" ~ '17-011294',
    APN == "49902048" ~ '17-011439',
    APN == "70637024" ~ '14-031138',
    APN == "59944031" ~ '19-104826',
    APN == "26157028" ~ '18-144691',
    APN == "26450057" ~ '18-145574',
    APN == "43927010" ~ '19-119040', 
    APN == "70613024" ~ '19-142112',
  ))

formal_missing <- formal_missing %>%
  dplyr::mutate(IssueDate = case_when(
    APN == "24936002" ~ '2018-11-29',
    APN == "24957042" ~ '2019-04-29',
    APN == "26140090" ~ '2018-06-21',
    APN == "26462005" ~ '2018-03-23',
    APN == "43947013" ~ '2013-12-04',
    APN == "49724012" ~ '2018-06-11',
    APN == "49902048" ~ '2017-11-03',
    APN == "70637024" ~ '2016-01-12',
    APN == "59944031" ~ '2019-02-07',
    APN == "26157028" ~ '2019-04-24',
    APN == "26450057" ~ '2018-12-06',
    APN == "43927010" ~ '2019-12-04',
    APN == "70613024" ~ '2019-12-05'
  ))

# Add these 13 missing permits to the final permit list 
final_permits <- rbind(
  final_permits %>% 
    dplyr::select(event_type, event_201620, FOLDERNUMBER, 
                  CompletionDate, Description, IssueDate, APN), 
  formal_missing %>% 
    dplyr::select(event_type, event_201620, FOLDERNUMBER, 
                  CompletionDate, Description, IssueDate, APN))

# Save
write.csv2(
  final_permits, 
  'data/processed/Permits/Final_Permits.csv', row.names=FALSE)
