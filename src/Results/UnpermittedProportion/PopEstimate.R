#
# Computes the population estimate of ADU constructions.
#


# Parameters (construction event type and minimum square footagre threshold)
cevent_type <- 'change_area1.2'
sqft_threshold <- 250
# * Number of formal constructions: We use 291 for the main results (per nF 
# in the main text. For the robustness check in Appendix H.2.1 we use 377 permits)
formal_constructions <- 291

# 0. Load data ========
# * Permitted/unpermitted annotated construction events
construction_file <- paste0(
  'data/processed/Permits/b3_apns_p_', 
  gsub('.', '', cevent_type, fixed=TRUE), '_', sqft_threshold, '.csv')
c_events <- read.csv(construction_file, colClasses=c("APN"="character"))
# * Our sample of 15,006 parcels
complete_sample <- read.csv('data/raw/complete_sampled_APNs.csv', 
                            colClasses=c("APN"="character"))
# * Data frame of 159k residential parcels in SJ
parcel_db <- read.csv('data/Final/parcel_database.csv',
                      colClasses=c("APN"="character"))

# 1. Pre-processing =======
# Merge datasets: add sampled binary indicator and number of constructions to parcel_db
complete_sample$sampled <- 1
parcel_db <- parcel_db %>% left_join(c_events, by='APN') %>%
  dplyr::mutate(event = !! rlang::sym(cevent_type)) %>%
  left_join(complete_sample %>% dplyr::select(APN, sampled), by='APN') %>%
  tidyr::replace_na(list(sampled=0))

# We need to specify that for sampled parcels with no event data, we observed
# zero (not NA) construction events here
parcel_db$event <- as.numeric(parcel_db$event)
parcel_db <- parcel_db %>%
  dplyr::mutate(event = case_when(
    (sampled == 1 & is.na(event)) ~ 0, 
    TRUE ~ event
  ))

# Compute stratum-level metrics: note that we ignore NAs for event since 
# NAs represent parcels that were not sampled
strata_db <- parcel_db %>% dplyr::group_by(Bin) %>%
  dplyr::summarise(
    N = n(), 
    n=sum(sampled), 
    sum_events=sum(event, na.rm=T), 
    var_events=var(event, na.rm=T)) %>%
  dplyr::mutate(wj = N / n) %>%
  dplyr::filter(n> 0) %>%
  dplyr::mutate(y_bar = wj * sum_events) %>%
  dplyr::mutate(var_j_component = (1 - n/N) * N^2 * var_events / n)

# 2. Population estimate and variance ======
y_hat <- sum(strata_db$y_bar, na.rm=T)
y_var <- sum(strata_db$var_j_component, na.rm = T)
y_sd <- sqrt(y_var)

# Confidence intervals 
margin <- 1.96 * y_sd
CI <- y_hat + c(-margin, margin)

# Unpermitted proportion
u_hat <- (1 - formal_constructions / y_hat) * 100
CI_u <- c((1 - formal_constructions / CI[1]) * 100, (1 - formal_constructions / CI[2]) * 100)

print(paste0('Population: ', round(y_hat, 0)))
print(paste0('Population margin: ', round(margin, 0)))
print(paste0('Unpermitted: ', round(y_hat - formal_constructions, 0)))
print(paste0('Unpermitted proportion: ', round(u_hat, 1)))
print(paste0('Unpermitted CI: ', round(CI_u[1], 1), ' , ', round(CI_u[2], 1)))
