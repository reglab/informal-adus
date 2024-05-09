#
# Power analysis for the level of the unpermitted proportion of 
# construction events to measure the statistical gains of using the computer
# vision model (see Appendix D.4)
#

library(reshape2)
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
library(WeightIt)

source('src/Sampling/PowerAnalysis/UR_simulation_functions.R')

# Parameters and Inputs ------------------------------------

# Study design parameters
# * Sampling design: SimpleRS (simple random sample), 
# Proportional (proportional allocation), Neyman (Neyman allocation)
design <- 'Neyman' 
# * Stratification: confidence (Model confidence alone), strata_inc (income
# strata alone), Bin (IS-CL: model confidence and income strata)
selected_group <- 'Bin'
# * Unpermitted rate and lower bound assumption
unpermitted_rate <- 0.60
lowerb_threshold <- 0.5

# Study design constants
sample_sizes <- seq(2000, 26000, by = 1000)
alpha <- 0.05

# Simulation constants
power_thresh <- 0.80
num_sims_p_step <- 500

# SJ Residential parcels
parcel_db_file <- 'data/raw/parcel_database.csv'

# Annotated simple random sample
CF_parcels_file <- paste0(
  'data/raw/PowerAnalysis/PA-CFparcels-CF-min11-GT1.csv')

# Neyman allocations under each sampling scenario
neyman_IS_file <- 'data/raw/PowerAnalysis/Neyman_alloc_IS.csv'
neyman_CB_file <- 'data/raw/PowerAnalysis/Neyman_alloc_CB.csv'

# 0. Load data ----------------------------------------------
# Parcel database (APN, GEOID, IS, CBin, Stratum, income, Neyman weight)
parcel_db <- read.csv(parcel_db_file, colClasses=c("APN"="character"))
parcel_db <- filter(parcel_db, strata_inc != 'IS_MISSING')
parcel_db <- parcel_db %>% 
  dplyr::mutate(Bin=gsub('.0', '', Bin, fixed=TRUE))

# Annotations for Batch 1 (the simple random sample of 5,000 parcels)
CF_parcels <- readr::read_csv(
  CF_parcels_file, col_types = cols(.default = "c"))
CF_parcels$new_small_build <- as.integer(CF_parcels$count_new_sbuild)
CF_parcels$permitted_ratio <- as.integer(CF_parcels$permitted_ratio)
CF_parcels <- CF_parcels %>%
  dplyr::mutate(confidence=as.integer(CBin))

# 1. Neyman allocations -------------------------------------
# Neyman allocation for IS-CL (150 bins)
neyman_ISCB <- parcel_db %>%
  dplyr::group_by(Bin) %>%
  dplyr::summarise(
    true_weight = dplyr::first(true_weight), 
    Neyman_weight = dplyr::first(Neyman_weight)) %>%
  dplyr::mutate(sampling_prop = Neyman_weight / true_weight) %>%
  dplyr::mutate(w = 1 / sampling_prop)

# Neyman allocation for IS
neyman_IS <- read.csv(neyman_IS_file)
neyman_IS <- neyman_IS %>% 
  dplyr::mutate(sampling_prop = Neyman_weight / true_weight) %>%
  dplyr::mutate(w = 1 / sampling_prop)

# Neyman allocation for CL
neyman_CB <- read.csv(neyman_CB_file)
neyman_CB <- neyman_CB %>%
  dplyr::mutate(sampling_prop = Neyman_weight / true_weight) %>%
  dplyr::mutate(w = 1 / sampling_prop)

neyman_list <- list('strata_inc'=neyman_IS, 'confidence'=neyman_CB, 'Bin'=neyman_ISCB)

# 2. Multinomial probabilities ---------
# for number of ADU constructions in a parcel (depending on the stratum)
stopifnot(max(CF_parcels$new_small_build) == 2)

prob_IS <- CF_parcels %>% 
  dplyr::group_by(strata_inc, new_small_build) %>% 
  dplyr::summarise(count_APNs=n()) %>%
  reshape2::dcast(strata_inc ~ new_small_build, value.var='count_APNs') %>%
  tidyr::replace_na(list(`0`=0, `1`=0, `2`=0)) %>%
  dplyr::mutate(total_APNs = `0` + `1` + `2`) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`0` = `0` / total_APNs, `1` = `1` / total_APNs, `2` = `2` / total_APNs) %>%
  dplyr::select(-total_APNs)
  
prob_CBin <- CF_parcels %>% 
  dplyr::group_by(confidence, new_small_build) %>% 
  dplyr::summarise(count_APNs=n()) %>%
  reshape2::dcast(confidence ~ new_small_build, value.var='count_APNs') %>%
  tidyr::replace_na(list(`0`=0, `1`=0, `2`=0)) %>%
  dplyr::mutate(total_APNs = `0` + `1` + `2`) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`0` = `0` / total_APNs, `1` = `1` / total_APNs, `2` = `2` / total_APNs) %>%
  dplyr::select(-total_APNs)

prob_stratum <- CF_parcels %>% 
  dplyr::group_by(Bin, new_small_build) %>% 
  dplyr::summarise(count_APNs=n()) %>%
  reshape2::dcast(Bin ~ new_small_build, value.var='count_APNs') %>%
  tidyr::replace_na(list(`0`=0, `1`=0, `2`=0)) %>%
  dplyr::mutate(total_APNs = `0` + `1` + `2`) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`0` = `0` / total_APNs, `1` = `1` / total_APNs, `2` = `2` / total_APNs) %>%
  dplyr::select(-total_APNs)

prob_SRS <- CF_parcels %>%
  dplyr::group_by(new_small_build) %>% 
  dplyr::summarise(count_APNs=n()) %>%
  dplyr::mutate(batch1=1) %>%
  reshape2::dcast(batch1 ~ new_small_build, value.var='count_APNs') %>%
  tidyr::replace_na(list(`0`=0, `1`=0, `2`=0)) %>%
  dplyr::mutate(total_APNs = `0` + `1` + `2`) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(`0` = `0` / total_APNs, `1` = `1` / total_APNs, `2` = `2` / total_APNs) %>%
  dplyr::select(-total_APNs)

prob_list <- list(
  'strata_inc'=prob_IS, 'confidence'=prob_CBin, 'Bin'=prob_stratum,
  'SimpleRS'=prob_SRS)

# 3. Power analysis functions --------------------

# * 3.1 Data-generating function -------------------------------

data_gen_func <- function(
    sampling_type, sample_size, 
    neyman_list, prob_list, parcel_db, group, permitted_rate) {
  # Output: df of construction events including APN, group and permit status
  
  # Get Neyman table, Multinomial table according to selected group
  neyman_group <- neyman_list[[group]]
  cons_prob_group <- prob_list[[group]]
  # If SRS, we just use probabilities across all strata
  if (sampling_type == 'SimpleRS') {
    cons_prob_group_SRS <- prob_list[['SimpleRS']]
    cons_prob_group <- cons_prob_group %>%
      dplyr::mutate(`0` = cons_prob_group_SRS$`0`,
                    `1` = cons_prob_group_SRS$`1`,
                    `2` = cons_prob_group_SRS$`2`)
  }

  # Parcel generation
  if (sampling_type == 'SimpleRS') {
    parcel_df <- generate_SimpleRS(sample_size=sample_size, df=parcel_db)
    parcel_df <- parcel_df %>% dplyr::mutate(w=1)
  }
  
  if (sampling_type == 'Proportional') {
    parcel_df <- generate_proportional(
      sample_size=sample_size, df=parcel_db, group_col=group)
    stop('not implemented')
  }
  
  if (sampling_type == 'Neyman') {
    parcel_df <- generate_Neyman(
      sample_size=sample_size, df=parcel_db, neyman_df=neyman_group, group_col=group)
    parcel_df <- parcel_df %>% 
      dplyr::left_join(neyman_group %>% dplyr::select_at(c(group, 'w'), by=group))
  }
  
  # Construction generation function
  constructions_df <- generate_constructions(
    parcel_df=parcel_df, prob_df=cons_prob_group, group_col=group, 
    permit_p=permitted_rate)
  
  return(list(df=constructions_df))
  
}

# * 2.2 Estimator ---------------------------------------------
estimator_func <- function(output) {
  
  df <- output$df
  
  permitted_rate <- sum(df$permitted * df$w) / sum(df$w) * 100
  p_hat <- 1 - permitted_rate / 100
  n <- ESS(df$w)
  
  margin <- 1.96 * sqrt(p_hat * (1- p_hat) / n)
  
  CI <- p_hat + c(-margin, margin)
  
  return(list(point_estimate=p_hat, lowerb=CI[1], upperb=CI[2]))
}

# * 2.3 Discriminator -----------------------------------------
# Test whether the estimated lower bound on the unpermitted rate is above
# the threshold we chose
discriminator_func <- function(test) test$lowerb >= lowerb_threshold

# * 2.4 Combining data generator, estimator and discriminator -----
calc_power <- function(data_generator,estimator,discriminator,num_sims=500) {
  sig_results <- c()
  for (i in 1:num_sims) {
    mock_data <- data_generator()
    model <- estimator(mock_data)
    sig_results[i] <- discriminator(model)
  }
  # Power
  sig_results %>%
    mean() %>%
    return()
}

# 3. Power curves -----------
# Create a dataframe with each effect size (es) and sample size (ss) pair
params <- expand.grid(list(ss=sample_sizes))

# Create parameterized data generators for each effect/sample size pair
create_generator <- function(ss, group) function() data_gen_func(
  sampling_type=design, sample_size=ss, 
  neyman_list=neyman_list, prob_list=prob_list, parcel_db=parcel_db, 
  group=selected_group, permitted_rate=1-unpermitted_rate)
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
  geom_line(linewidth=1) +
  # add a horizontal line at 80%
  geom_hline(aes(yintercept = power_thresh), linetype = 'dashed') +
  xlim(0, 26000) +
  scale_y_continuous(labels=percent, limits=c(0, 1)) +
  labs(x = 'Sample Size', y = 'Power') +
  theme(legend.position="bottom") +
  theme_light()
