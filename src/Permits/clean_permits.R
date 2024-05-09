#
# Classifies the 556 detached ADU permits issued 2015-2020 in San Jose into
# permit type (Addition, Legalization or Conversion) and whether or not
# the construction took place during our period of interest (2016 to the 
# end_date parameter specified in this script).
# Input:
#   - Adu_Detach_Scraped.csv : This is SJ's detached ADU permit list of 556 ADUs
# Output:
#   - Adu_Detach_Cleaned_{end_date}.csv including the 556 permits, their
#   permit type and whether the construction took place during the period.
#

library(tidyverse)


# Define permit end date
end_date <- as.Date('06/01/2020', format="%m/%d/%Y")

# 1. Step 1: Find Additions ==========================
# Load scraped data (using src/Permits/Detached.py) on SJ detached ADU permits
# from SJ's Permit & Property Information Portal
permits_file <- 'data/processed/Permits/Adu_Detach_Scraped.csv'
permits <- read.csv(
  permits_file, 
  colClasses=c("Geocoded.APN"="character", "Permit.APN"="character"))

# Simple regex to try to capture Additions vs Conversions vs Legalizations
permits <- permits %>%
  dplyr::mutate(
    event_Legalization = grepl('legal', Description, ignore.case=TRUE)) %>%
  dplyr::mutate(
    event_conversion = grepl('conver', Description, ignore.case=TRUE)) %>%
  dplyr::mutate(
    event_addition = grepl('new|add|constr|build', Description, ignore.case=TRUE))

permits <- permits %>%
  dplyr::rowwise() %>%
  dplyr::mutate(event_type = case_when(
    event_Legalization == TRUE & as.numeric(event_conversion) + as.numeric(event_addition) == 0 ~ 'Legalization', 
    event_conversion == TRUE & as.numeric(event_Legalization) + as.numeric(event_addition) == 0 ~ 'Conversion', 
    event_addition == TRUE & as.numeric(event_Legalization) + as.numeric(event_conversion) == 0 ~ 'Addition', 
    as.numeric(event_Legalization) + as.numeric(event_conversion) + as.numeric(event_addition) == 0 ~ 'Addition',
    TRUE ~ 'NA'
  ))

# We manually review the classification generated above, and save the
# reviewed file to data/raw/Permits/Adu_Detach_Scraped_Reviewed.csv, with
# the final classification (Addition, Legalization, Conversion) listed in
# the event_type column.

permits_reviewed <- read.csv(
  'data/raw/Permits/Adu_Detach_Scraped_Reviewed.csv', 
  colClasses=c("Geocoded.APN"="character", "Permit.APN"="character"))

# Fix APNs (we have to append initial 0 as these get corrupted in the Google 
# Sheets doc)
permits_reviewed <- permits_reviewed %>%
  dplyr::mutate(
    Geocoded.APN = as.character(Geocoded.APN), 
    Permit.APN = as.character(Permit.APN)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Geocoded.APN = case_when(
    nchar(Geocoded.APN) == 7 ~ as.character(paste0('0', Geocoded.APN)), 
    TRUE ~ as.character(Geocoded.APN))) %>%
  dplyr::mutate(Permit.APN = case_when(
    nchar(Permit.APN) == 7 ~ as.character(paste0('0', Permit.APN)), 
    TRUE ~ as.character(Permit.APN)))

print('[INFO] Classification of the 556 permits:')
permits_reviewed %>% dplyr::group_by(event_type) %>% dplyr::summarize(n = n())

# 2. Step 2: Time Period ==========================
permits_clean <- permits_reviewed

# Identify permits that had a rough frame completed by the selected end date. 
# We find the earliest date included in the rough frame approval date list
parse_datelist <- function(dlist) {
  if (dlist == '' || dlist == '[]') return(NA)
  
  dlist <- c(
    strsplit(gsub("\\]", "", gsub("\\[", "" ,dlist)), ", ")[[1]])
  
  min_date <- end_date
  for (d in dlist) {
    d <- gsub("'", "", d)
    d <- as.Date(d, format="%m/%d/%Y")
    if (d < min_date) min_date <- d
  }
  return(min_date)
}

permits_clean <- permits_clean %>%
  dplyr::rowwise() %>%
  dplyr::mutate(RoughFrameDate = parse_datelist(Rough.Frame.Approval.Date))

# If rough frame approval date is missing, we use the finaled date.
permits_clean <- permits_clean %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Final.Date = as.Date(Final.Date, format="%m/%d/%Y")) %>%
  dplyr::mutate(CompletionDate = case_when(
    is.na(RoughFrameDate) ~ Final.Date,
    TRUE ~ RoughFrameDate
  ))

# There are 16 permits that still have a missing completion date after this
# process. These permits seem to be mostly from 2020.

# Of the 16, 7 permits have an issue date prior to our end date:
# permits_clean %>%
#  dplyr::mutate(Issue.Date = as.Date(Issue.Date, format="%m/%d/%Y")) %>%
#  dplyr::filter(Issue.Date < end_date & is.na(CompletionDate))

# Looking at these on Google Earth, only the following 2 seem to actually have 
# been built before June 2020. 

permits_clean <- permits_clean %>%
  dplyr::mutate(CompletionDate = case_when(
    FOLDERNUMBER %in% c('19-110169', '19-110861') ~ end_date - 1, 
    is.na(CompletionDate) ~ end_date,
    TRUE ~ CompletionDate
))

print(paste0(
  '[INFO] Dropping ',
  sum(permits_clean$CompletionDate >= end_date),
  ' permits with completion dates post ', end_date -1))

permits_clean <- permits_clean %>%
  dplyr::mutate(event_201620 = CompletionDate < end_date)

# Final classification of the 556 permits
start_num_permits <- dim(permits_reviewed)[1]
print(paste0('[INFO] Number of 2016-2020 permits: ', start_num_permits))
print('[INFO] Final classification: ')
table(permits_clean %>% dplyr::select(event_type, event_201620))

# 3. Final processing ======================

# Final APN associated with the permit
permits_clean <- permits_clean %>% 
  dplyr::mutate(Final.APN = case_when(
    Geocoded.APN == Permit.APN ~ Geocoded.APN, 
    is.na(Geocoded.APN) ~ Permit.APN, 
    Permit.APN == '' ~ Geocoded.APN,
    grepl('more', Permit.APN) ~ Geocoded.APN,
    TRUE ~ Permit.APN
  ))

permits_clean <- permits_clean %>%
  dplyr::select(
    Final.APN, event_type, event_201620, FOLDERNUMBER, Issue.Date, CompletionDate, Description) %>%
  dplyr::mutate(IssueDate = as.Date(Issue.Date, format="%m/%d/%Y")) %>%
  dplyr::select(-Issue.Date)

# 4. Output =================
write.csv2(
  permits_clean,
  paste0('data/processed/Permits/Adu_Detach_Cleaned_', end_date, '.csv'))
