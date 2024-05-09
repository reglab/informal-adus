#
# Computes the unpermitted proportion of ADU constructions (and associated
# confidence intervals) on a subset of construction events defined by 
# minimum square footage thresholds. Appendix H.1.2
#

library(tidyverse)
library(WeightIt)
library(ggplot2)

# Parameters: type of construction event
cevent_type <- 'change_area1.2'

# 0. Helper functions ========
compute_rates <- function(c_events_df) {
  # Convert from APN to construction-event level
  events_perm <- c_events_df %>%
    dplyr::select(APN, w, permitted) %>%
    dplyr::filter(permitted > 0) %>%
    uncount(permitted) %>%
    dplyr::mutate(type = 'permitted') %>%
    dplyr::select(APN, w, type)
  
  events_unperm <- c_events_df %>%
    dplyr::select(APN, w, unpermitted) %>%
    dplyr::filter(unpermitted > 0) %>%
    uncount(unpermitted) %>%
    dplyr::mutate(type = 'unpermitted') %>%
    dplyr::select(APN, w, type)
  
  events_df <- rbind(events_perm, events_unperm) %>%
    dplyr::mutate(event=1)
  
  # Proportion
  permitted_rate <- sum(events_perm$w) / sum(events_df$w) * 100
  p_hat <- 1 - permitted_rate / 100
  
  # CIs
  n <- ESS(events_df$w)
  margin <- 1.96 * sqrt(p_hat * (1- p_hat) / n)
  
  CI <- p_hat + c(-margin, margin)
  return(list(u_prop=p_hat, u_lower=CI[1], u_upper=CI[2]))
}

subset_cevents <- function(construction_df, c_events_df) {
  # Subset c_events_df
  construction_df <- construction_df %>%
    dplyr::group_by(APN) %>% dplyr::summarise(total=n())
  
  c_events_df <- c_events_df %>%
    dplyr::inner_join(construction_df, by='APN')
  
  c_events_df <- c_events_df %>%
    dplyr::mutate(permitted_updated = pmin(total, permitted)) %>%
    dplyr::mutate(unpermitted_updated = total - permitted_updated)
  
  c_events_df <- c_events_df %>% 
    dplyr::select(APN, total, permitted_updated, unpermitted_updated, w) %>%
    dplyr::mutate(permitted=permitted_updated, unpermitted=unpermitted_updated) %>%
    dplyr::select(-permitted_updated, -unpermitted_updated)
  
  return(c_events_df)
}

compute_rates_permit_set <- function(cevent_type, permit_set) {
  # Load data ===============
  # Permitted/unpermitted construction events
  # Note that we load results at 120 as these contain all construction
  # events to be filtered down at increasing sqft thresholds
  c_events_base <- read.csv2(
    paste0('data/processed/permitted_cevents_', 
           gsub('.', '', cevent_type, fixed=TRUE), '_120.csv'), 
    colClasses=c("APN"="character")
  )
  
  # These are the construction events classified as permitted/unpermitted
  # according to the more liberal permit set (detached structures, not just
  # detached ADUs)
  c_events_lowerb <- read.csv2(
    paste0('data/processed/Permits/b3_apns_lowerbound_',
           gsub('.', '', cevent_type, fixed=TRUE), '_120.csv'), 
    colClasses=c("APN"="character")
  )
  
  if (permit_set == 'base') {
    c_events <- c_events_base
  } else if (permit_set == 'lower_bound') {
    c_events <- c_events_lowerb
  } else {
    stopifnot(FALSE)
  }
  
  parcel_db <- read.csv('data/raw/parcel_database.csv',
                        colClasses=c("APN"="character"))
  
  # Add Neyman weights to construction events
  c_events <- c_events %>% left_join(
    parcel_db %>% 
      dplyr::mutate(w = true_weight / Neyman_weight) %>% 
      dplyr::select(APN, w), by='APN')
  
  # Annotated construction events (so we can obtain the square footage)
  processed_buildings <- read.csv(
    'data/raw/processed_buildings.csv', 
    colClasses=c("apn"="character")) %>%
    dplyr::mutate(APN=apn) %>% 
    dplyr::select(-X2016, -X2020, -apn)
    
  # 1. Alter the set of construction events based on sqft threshold ======
  base_constructions <- processed_buildings %>%
    dplyr::mutate(event = !! rlang::sym(cevent_type)) %>%
    dplyr::filter(X2016_a >= 11.15 | X2020_a >= 11.15) %>%
    dplyr::filter(event == 1) %>%
    dplyr::mutate(change_sqm = case_when(
      is.na(X2016_a) ~ X2020_a, 
      TRUE ~ X2020_a - X2016_a)) %>%
    dplyr::mutate(change_sqft = change_sqm * 10.7639) %>%
    dplyr::select(APN, X2016_a, X2020_a, change_sqft) %>%
    dplyr::mutate(area_2020_sqft = X2020_a * 10.7639)
  
  # 2. Compute rates across sqft thresholds =========
  sqft_changes <- seq(120, 300, 10)
  rate_df <- data.frame()
  
  for (sqft_change in c(sqft_changes)) {
    
    # Subset construction events: by 2020 area
    subset_constructions <- base_constructions %>%
      dplyr::filter(area_2020_sqft >= sqft_change)
    
    c_events_subset <- subset_cevents(
      construction_df=subset_constructions, c_events_df=c_events)
    
    rate_list <- compute_rates(c_events_df=c_events_subset)
    
    rate_df <- rbind(
      rate_df, 
      data.frame(permit_set=permit_set, sqft_change=sqft_change, 
                 cevents=sum(c_events_subset$total), 
                 u_prop=rate_list$u_prop * 100, u_lower=rate_list$u_lower * 100, 
                 u_upper=rate_list$u_upper * 100
                 ))
  }
  
  return(rate_df)
}

# 1. Generate plot ========
# Run for each type of permit subset
rate_df_base <- compute_rates_permit_set(
  cevent_type=cevent_type, permit_set='base')
rate_df_lb <- compute_rates_permit_set(
  cevent_type=cevent_type, permit_set='lower_bound')
rate_df <- rbind(rate_df_base, rate_df_lb)

# Re-scale to percentage and clean variable names
rate_df <- rate_df %>%
  dplyr::mutate(
    permit_set = case_when(
      permit_set == 'base' ~ "Base set", 
      permit_set == 'lower_bound' ~ 'Expanded set'
  ), 
    u_prop = u_prop / 100,
    u_lower = u_lower / 100, 
    u_upper = u_upper / 100
  )

# Plot
ggplot(rate_df, 
       aes(x=sqft_change, group=permit_set)) + 
  geom_ribbon(aes(ymax=u_upper, ymin=u_lower, fill=permit_set), alpha=0.3) + 
  geom_line(aes(y=u_prop, color=permit_set)) + 
  theme_light() + 
  geom_point(aes(y=u_prop, size=cevents, color=permit_set)) + 
  labs(
    x='Minimum threshold for ADU square footage', 
    y='Unpermitted percentage of\nADU constructions (2016-2020)',
    size='Number of\nconstructions\nin the sample', 
    color='Set of\npermits', 
    fill='Set of\npermits') +
  scale_y_continuous(labels = scales::percent, limits=c(0, 1))
ggsave(paste0('output/Visualizations/sqft_robustness.png'), 
       width = 8, height = 4, dpi=300, units = "in", device='png')
