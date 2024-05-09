#
# Power Analysis used to define the sample size of our stratified sample (per
# the Neyman allocation): 15,006 samples 
# This power analysis uses an initial simple random sample of 5,000 parcels
# to obtain priors on our question of interest: whether there are differential 
# detached ADU construction rates for different levels of income
# See Appendix D.3 for a full description.
#

library(readr)
library(tidyverse)
library(patchwork)
library(stargazer)
library(scales)
library(broom)
library(sf)
library(pbapply)
library(Hmisc)
library(MASS)

source('src/Sampling/PowerAnalysis/simulation_functions.R')

# Parameters and Inputs ------------------------------------
# Study design parameters
# * Sampling design: SimpleRS (simple random sample), 
# Proportional (proportional allocation), Neyman_B1 (Neyman allocation based on
# our initial simple random sample)
design <- 'SimpleRS' 
generation_type <- 'negativebinomial' 
test_type <- 'negativebinomial' # For regression outcome

# Study design constants
sample_sizes <- seq(2000, 30000, by = 2000)
alpha <- 0.05

# Simulation constants
power_thresh <- 0.80
num_sims_p_step <- 100

# * Residential parcels in San Jose and household income for census block groups
# in San Jose 
sj_parcels_res_file <- 'data/Processed/PopEstimates/sj-parcels-res-cbgs'
cbg_strata_file <- 'data/Processed/PopEstimates/cbg-income-strata'

# * Neyman allocation (based on estimates from the initial random sample)
neyman_file <- paste0(
  'data/Processed/PopEstimates/Outcomes/B1-100/PA/PA-neyman_alloc-CF-min11-GT1.csv')
# * Initial sample of 5,000 parcels
CF_parcels_file <- paste0(
  'data/Processed/PopEstimates/Outcomes/B1-100/PA/PA-CFparcels-CF-min11-GT1.csv')
# * Computer vision predictions for the confidence of the construction of a detached
# structure between 2016 and 2020
parcel_confidence_file <- 'data/processed/PopEstimates/parcel-confidence.csv'
# * Annotated Simple Random Sample of 5,000 parcels
parcel_density_file <- 'data/processed/PopEstimates/Outcomes/B1-100/PA/parcel_density-min11.csv'

# 0. Load data ----------------------------------------------
# 0.1 Number of residential parcels in each income strata -------
sj_parcels_res <- st_read(sj_parcels_res_file)
sj_parcels_res <- sj_parcels_res %>% st_drop_geometry()

cbg_strata <- st_read(cbg_strata_file)
cbg_strata <- cbg_strata %>% st_drop_geometry() %>%
  dplyr::select(GEOID, strata_inc, median_inc)

# Add median income to sj_parcels_res
sj_parcels_res <- left_join(sj_parcels_res, cbg_strata, by='GEOID')

# Drop parcels from missing income strata
sj_parcels_res <- filter(sj_parcels_res, strata_inc != 'IS_MISSING')

# Number of parcels in each income strata
num_parcels <- sj_parcels_res %>% 
  group_by(strata_inc) %>% dplyr::summarise(n= n())

# 0.2 Neyman allocation and new buildings per parcel for CF area ----------
neyman <- read.csv(neyman_file)
neyman <- neyman %>% 
  dplyr::mutate(true_weight = N_j / sum(neyman$N_j)) %>%
  dplyr::mutate(sampling_prop = weight / true_weight) %>%
  dplyr::mutate(w = 1 / sampling_prop)

CF_parcels <- readr::read_csv(
  CF_parcels_file, col_types = cols(.default = "c"))
CF_parcels$new_small_build <- as.integer(CF_parcels$count_new_sbuild)

# Add median income to CF parcels
CF_parcels <- CF_parcels %>%
  left_join(dplyr::select(cbg_strata, GEOID, median_inc), by=c('GEOID') )
stopifnot(dim(CF_parcels)[1] == 5541)

# Add weights belonging to the strata
CF_parcels <- CF_parcels %>% 
  left_join(dplyr::select(neyman, Bin, w), by='Bin')

# Parcel confidences and Bins
parcel_confidence <- readr::read_csv(
  parcel_confidence_file, col_types = cols(.default = "c"))
# * Generate bins, add median income
parcel_confidence <- parcel_confidence %>%
  dplyr::mutate(Bin = paste0(strata_inc, '-', confidence)) %>%
  left_join(dplyr::select(sj_parcels_res, APN, median_inc), by='APN')

# 0.3 Model for the relationship between num construction events and median income ----
parcel_density <- read.csv(parcel_density_file)

# Add Bins and weights belonging to the strata
parcel_density <- parcel_density %>% 
  left_join(dplyr::select(parcel_confidence, APN, Bin), by='APN') %>%
  left_join(dplyr::select(neyman, Bin, w), by='Bin')

# Linear model
model <- NA
lm.model <- lm(count_new_sbuild ~ median_inc, data=parcel_density)

# Negative binomial
neg.model <- glm.nb(count_new_sbuild ~ median_inc, data=parcel_density)

if (generation_type == 'negativebinomial') model <- neg.model
stopifnot(!is.na(model))

# 1. Data-generating function -------------------------------

# Data generation function to generate samples of parcels with associated
# construction events and median income
data_gen_func <- function(
    sampling_type, model, sample_size,neyman, CF_parcels, sj_parcels_res, 
    num_parcels, parcel_confidence) {
  # Output: List including
  #   - building_df: data.frame of construction events, including median income
  
  if (grepl('Neyman', sampling_type, fixed=TRUE) == FALSE) {
    if (sampling_type == 'SimpleRS') {
      parcel_df <- generate_SimpleRS(sample_size, sj_parcels_res)
    } else if (sampling_type == 'Proportional') {
      parcel_df <- generate_proportional(sample_size, num_parcels, sj_parcels_res)
    }
    
    building_df <- generate_CEvents_from_model(model, generation_type, parcel_df)
    
  } else if (grepl('Neyman', sampling_type, fixed=TRUE)) {
    # Neyman allocation
    building_df <- generate_CEvents_Neyman(
      sampling_type, neyman, CF_parcels, sample_size)
  }
  
  return(list(df=building_df))
  
}

# 2. Estimator ---------------------------------------------
estimator_func <- function(output) {
  df.model <- NA
  
  if (design == 'Neyman_B1') {
    # Here we estimate the model on a Stratified Random Sample, so we need
    # to use the sampling weights. 
    if (test_type == 'linear') {
      df.model <- lm(n_sbuild ~ median_inc, data = output$df, weights=w)
    } else if (test_type == 'negativebinomial') {
      df.model <- glm.nb(n_sbuild ~ median_inc, data = output$df, weights=w)
    }
    
  } else {
    # For a Simple Random Sample we don't need to use WLS.
    if (test_type == 'negativebinomial') {
      df.model <- glm.nb(n_sbuild ~ median_inc, data=output$df)
    } else if (test_type == 'linear') {
      df.model <- lm(n_sbuild ~ median_inc, data = output$df)
    }
  }
  
  stopifnot(!is.na(df.model))
  model.coefs <- summary(df.model)$coefficients
  
  slope_pvalue <- model.coefs[2, colnames(model.coefs)[4]]
  slope_estimate <- model.coefs[2, 'Estimate']
  
  return(list(`p.value`=slope_pvalue, estimate=slope_estimate))
}

# 3. Discriminator -----------------------------------------

# We test whether we can claim at an alpha level of significance that
# there is a negatie relationship between ADU construction rates and income
discriminator_func <- function(test) test$p.value <= alpha & test$estimate < 0 

# 4. Combining 1, 2 and 3 -----
calc_power <- function(data_generator,estimator,discriminator,num_sims=500) {
  sig_results <- c()
  for (i in 1:num_sims) {
    # Re-create the data
    mock_data <- data_generator()
    # Run the analysis
    model <- estimator(mock_data)
    # Answer our original question
    sig_results[i] <- discriminator(model)
  }
  # Power
  sig_results %>%
    mean() %>%
    return()
}

# 5. Power curves -----------
# Create a dataframe with each effect size (es) and sample size (ss) pair
params <- expand.grid(list(ss=sample_sizes))

# Create parameterized data generators for each effect/sample size pair
create_generator <- function(ss) function() data_gen_func(
  design, model, ss, neyman, CF_parcels, sj_parcels_res, num_parcels, 
  parcel_confidence)
data_generators <- mapply(create_generator, params$ss)

power_res <- pbmapply(
  calc_power,
  data_generators,
  MoreArgs=list(
    estimator = estimator_func,
    discriminator = discriminator_func,
    num_sims = num_sims_p_step)
)

results <- params  %>% mutate(power = power_res)

ggplot(results, aes(x = ss, y = power)) +
  geom_line(size=1) +
  # add a horizontal line at 80%
  geom_hline(aes(yintercept = power_thresh), linetype = 'dashed') +
  xlim(0, 30000) +
  scale_y_continuous(labels=percent, limits=c(0, 1)) +
  labs(x = 'Sample Size', y = 'Power') +
  theme(legend.position="bottom")
