# 
# Computes the difference in means tests for census block group and parcel-level 
# CoreLogic variables on the set of formal and informal construction events from
# the annotated stratified sample
#

library(readr)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(weights)
library(tableone)
library(survey)
library(xtable)

# Parameters (construction type, minimum square footage threshold, permit set)
cevent_type <- 'change_area1.2'
sqft_threshold <- 250
permit_set <- 'base' # expanded or base
decimal_alignment <- TRUE

stopifnot(sqft_threshold == 250) # Otherwise upper bounds need to be updated

# 0. Load data and define upper bounds ========
# Construction events (formal and informal)
out_file <- if (cevent_type == 'change_area1.2') 'ca12' else cevent_type
if (permit_set == 'expanded') out_file <- paste0(out_file, '_expanded')
out_file <- paste0(out_file, '_', sqft_threshold)
cevents_file <- paste0('data/processed/DIMeans/b3_cevents_', out_file, '_DIMs.csv')

# * Load data.frame of f/i construction events and variables 
cevents <- read.csv(
  cevents_file, colClasses=c(
    "APN"="character", "GEOID"="character"
))

informal <- cevents %>% dplyr::filter(type=='informal')
formal <- cevents %>% dplyr::filter(type=='formal')

# * Upper bound for informal population of construction events
if (cevent_type == 'change_area1.2') {
  # This is the construction type used in the main text
  informal_upperb <- 1336 + 226 - dim(formal)[1] # See Main text (Results)

} else if (cevent_type == 'construction') {
  # This is the construction type used in the robustness check in Appendix H.1
  informal_upperb <- 883 + 193 - dim(formal)[1] # See Appendix H.1
} else {
  stopifnot(FALSE)
}

# 1. Difference in means tests ================

# Define variables
factorVars <- c()
toDrop <- c(
  "OWNERSHIP.CHANGE", "OWNERSHIP.CHANGE.YEAR", "OWNERSHIP.CHANGE.MONTH",
  "SALE.AMOUNT", "SALE.CODE", "MORTGAGE.AMOUNT")
myVars <- colnames(
  cevents %>% dplyr::select(-APN, -w, -type, -GEOID, -IssueDate, -FOLDERNUMBER, -one_of(toDrop)))

# Handle categorical variables
cevents <- cevents %>%
  dplyr::mutate(
    prop_renter_occu_2016 = prop_renter_occu_2016 * 100, 
    HOMESTEAD.EXEMPT = HOMESTEAD.EXEMPT * 100)

# Add population size of each group
formal_pop <- as.numeric(dim(formal)[1])

cevents <- cevents %>%
  dplyr::mutate(
    fpc=case_when(
      type == 'formal' ~ formal_pop, # Population of formal construction events
      type == 'informal' ~ informal_upperb)
    )

# Tests
cevents_svy <- svydesign(
  id=~1, # No clusters 
  strata=~type, 
  weights=~w, # Sampling weights
  fpc=~fpc, # Population size for the stratum
  data=cevents)
tableOne <- svyCreateTableOne(
  vars = myVars, strata = "type" , data = cevents_svy,
  test=TRUE, testApprox='svyttest')

# 2. Formatting ================
tab <- data.frame(print(tableOne))

# Variable names
tab <- tab %>%
  dplyr::mutate(Variable=rownames(tab)) %>%
  dplyr::mutate(Variable=case_when(
    Variable == "n" ~ "Construction events",
    Variable == "hhmedinc_2016 (mean (SD))" ~ "Household median income ($)",             
    Variable == "hh_2016 (mean (SD))" ~ "Number of households",
    Variable == "med_gross_rent_2016 (mean (SD))" ~ "Median gross rent ($)",           
    Variable == "med_contract_rent_2016 (mean (SD))" ~ "Median contract rent ($)",
    Variable == "av_housing_vac_rate_2016 (mean (SD))" ~ "Available housing vacancy rate (%)",      
    Variable == "prop_renter_occu_2016 (mean (SD))" ~ "Percentage of renter occupied housing (%)",
    Variable == "latinx_perc_2016 (mean (SD))" ~ "Hispanic or Latino population (%)",              
    Variable == "white_perc_2016 (mean (SD))" ~ "White population (%)",
    Variable == "black_perc_2016 (mean (SD))" ~ "Black population (%)",              
    Variable == "asian_perc_2016 (mean (SD))" ~ "Asian population (%)",
    
    Variable == "NH_white_perc_2016 (mean (SD))" ~ "Non-Hispanic White population (%)",
    Variable == "NH_black_perc_2016 (mean (SD))" ~ "Non-Hispanic Black population (%)",              
    Variable == "NH_asian_perc_2016 (mean (SD))" ~ "Non-Hispanic Asian population (%)",
    
    Variable == 'overcrowding_perc_2016 (mean (SD))' ~ "Overcrowding (%)",
    Variable == 'severe_overcrowding_perc_2016 (mean (SD))' ~ "Severe Overcrowding (%)",
    Variable == "TOTAL.VALUE.CALCULATED (mean (SD))" ~ "Total value calculated ($)",
    Variable == "LAND.VALUE.CALCULATED (mean (SD))" ~ "Land value calculated ($)",
    Variable == "IMPROVEMENT.VALUE.CALCULATED (mean (SD))" ~ "Improvement value calculated ($)", 
    Variable == "ASSD.TOTAL.VALUE (mean (SD))" ~ "Assessed total value ($)",
    Variable == "ASSD.LAND.VALUE (mean (SD))" ~ "Assessed land value ($)",              
    Variable == "ASSD.IMPROVEMENT.VALUE (mean (SD))" ~ "Assessed improvement value ($)",
    Variable == "ACRES (mean (SD))" ~ "Acres",                         
    Variable == "LAND.SQUARE.FOOTAGE (mean (SD))" ~ "Land square footage",
    Variable == "UNIVERSAL.BUILDING.SQUARE.FEET (mean (SD))" ~ "Universal building square feet",
    Variable == "BUILDING..SQUARE.FEET (mean (SD))" ~ "Building square feet",
    Variable == "LIVING.SQUARE.FEET (mean (SD))" ~ "Living square feet",
    Variable == "GROUND.FLOOR.SQUARE.FEET (mean (SD))" ~ "Ground floor square feet",
    Variable == "GARAGE.PARKING.SQUARE.FEET (mean (SD))" ~ "Garage parking square feet",    
    Variable == "BEDROOMS (mean (SD))" ~ "Bedrooms",  
    Variable == "TOTAL.ROOMS (mean (SD))" ~ "Total rooms",                
    Variable == "TOTAL.BATHS.CALCULATED (mean (SD))" ~ "Total baths calculated",
    Variable == "TOTAL.BATHS (mean (SD))" ~ "Total baths",                   
    Variable == "NUMBER.OF.UNITS (mean (SD))" ~ "Number of units",
    Variable == 'hh_lasqkm_2016 (mean (SD))' ~ "Households per sq km",
    Variable == 'HOMESTEAD.EXEMPT (mean (SD))' ~ "Homeowner Exemption (%)",
    Variable == 'YEARS.SINCE.LAST.OWNERSHIP.CHANGE (mean (SD))' ~ 'Years since last ownership change',
    Variable == 'MONTHS.SINCE.LAST.OWNERSHIP.CHANGE (mean (SD))' ~ 'Months since last ownership change',
    TRUE ~ "NA"
  ))

# Get N for formal and informal
N_formal <- as.numeric((tab %>% dplyr::filter(Variable == 'Construction events'))$formal)
N_informal <- as.numeric((tab %>% dplyr::filter(Variable == 'Construction events'))$informal)

# Select variables to include
tab <- tab %>% dplyr::filter(
  Variable %in% c(
    # 
    'Household median income ($)', 'Number of households', 'Median gross rent ($)', 
    'Available housing vacancy rate (%)', 'Percentage of renter occupied housing (%)', 
    'Hispanic or Latino population (%)', 
    'Non-Hispanic White population (%)', 
    'Non-Hispanic Black population (%)', 
    'Non-Hispanic Asian population (%)',
    'Overcrowding (%)', 'Severe Overcrowding (%)', 'Households per sq km',
    'Assessed total value ($)', 'Assessed land value ($)', 
    'Assessed improvement value ($)', 
    'Land square footage', 'Building square feet', 'Bedrooms', 
    'Homeowner Exemption (%)', 'Years since last ownership change'
    ))

tab <- tab %>% dplyr::select(Variable, formal, informal, p)

# Separate SD from Means column and compute SE
tab <- tab %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    `SE (Formal)` = as.numeric(gsub(')', '', strsplit(formal, " (", fixed=TRUE)[[1]][2])) / sqrt(N_formal), 
    `SE (Informal)` = as.numeric(gsub(')', '', strsplit(informal, " (", fixed=TRUE)[[1]][2])) / sqrt(N_informal), 
    `Mean (Formal)` = as.numeric(strsplit(formal, " (", fixed=TRUE)[[1]][1]), 
    `Mean (Informal)` = as.numeric(strsplit(informal, " (", fixed=TRUE)[[1]][1])) 

# Select columns
tab <- tab %>% 
  dplyr::select(Variable, `Mean (Formal)`, `SE (Formal)`, `Mean (Informal)`, `SE (Informal)`, p) %>%
  dplyr::mutate(`p-value` = p) %>% dplyr::select(-p)

rounding_vars <- c()
if (!decimal_alignment) {
  # Specify number of digits
  rounding_vars <- c(
    'Household median income ($)', 'Number of households', 'Median gross rent ($)', 
    'Assessed total value ($)', 'Assessed land value ($)', 'Assessed improvement value ($)', 
    'Land square footage', 'Building square feet', 'Households per sq km')
}

# Format commas
columns <- c('Mean (Formal)', 'Mean (Informal)', 'SE (Formal)', 'SE (Informal)')
for (col in columns) {
  tab <- tab %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!col := case_when(
        Variable %in% rounding_vars ~ formatC(get(col), format="d", big.mark=","), 
        TRUE ~ formatC(get(col), format="f", big.mark=",", digits=1))
    )
}

# Format p-value
pval_format <- function(pval) {
  if (pval == '<0.001') {
    return('<0.01')
  } else {
    num_pval <- as.numeric(pval)
    if (num_pval < 0.01) return('<0.01')
    num_pval <- sprintf("%.2f", round(num_pval, 2))
    return(as.character(num_pval))
  }
}

tab <- tab %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`p-value` = pval_format(`p-value`))


# Export
xtable(tab, label='tab:DIMs', include.rownames=FALSE, )

