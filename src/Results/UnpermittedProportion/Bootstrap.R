#
# Computes the bootstrap uncertainty measures of ADU constructions.
#

library(splitstackshape)
library(tidyverse)
library(ggplot2)
library(scales)

# Parameters (construction event type, minimum square footage threshold)
cevent_type <- 'change_area1.2'
sqft_threshold <- 250
K <- 10000 # number of bootstrap iterations
alpha = 0.025 # alpha level

# Number of formal constructions and population estimate
# Note: these figures can be found in the main text
formal_constructions <- 291 # We have 291 formal events
pop_point <- 1336
pop_margin <- 226

# 0. Load data ========
# * Permitted/unpermitted annotated construction events
construction_file <- paste0(
  'data/processed/Permits/b3_apns_p_', 
  gsub('.', '', cevent_type, fixed=TRUE), '_', sqft_threshold, '.csv')
c_events <- read.csv(construction_file, colClasses=c("APN"="character"))
# * Our sample of 15,0006 parcels
complete_sample <- read.csv('data/raw/complete_sampled_APNs.csv', 
                            colClasses=c("APN"="character"))
# * Data frame of 159k residential parcels in SJ
parcel_db <- read.csv('data/raw/parcel_database.csv',
                      colClasses=c("APN"="character"))

# 1. Compute number of sampled parcels in each strata and number of parcels in each strata ======
# Neyman allocation for IS-CB (150 bins)
neyman <- complete_sample %>%
  dplyr::group_by(Bin) %>%
  dplyr::summarise(n_j = n()) %>%
  dplyr::left_join(
    parcel_db %>% dplyr::group_by(Bin) %>% dplyr::summarise(N_j = n()), by='Bin')

# Merge datasets: add sampled binary indicator and number of constructions to parcel_db
merged <- complete_sample %>%
  left_join(c_events, by='APN') %>%
  dplyr::mutate(event = !! rlang::sym(cevent_type)) %>%
  tidyr::replace_na(list(event=0)) %>%
  left_join(neyman, by='Bin')

# 2. Run bootstrap ========
# Bootstrap stratified sample
run_iter <- function(data, neyman_df) {
  # Get a sample following the Neyman allocation
  bin_sizes <- neyman_df$n_j
  names(bin_sizes) <- neyman_df$Bin
  
  sample_df <- stratified(
    data, 'Bin', size=bin_sizes, replace=TRUE) %>%
    dplyr::select_at(c('APN', 'Bin', 'event'))
  
  # Compute estimate on sample
  estimate <- sample_df %>% dplyr::group_by(Bin) %>%
    dplyr::summarise(
      sum_events=sum(event, na.rm=T)) %>%
    left_join(neyman_df, by='Bin') %>%
    dplyr::mutate(y_bar = N_j * sum_events / n_j)
  
  # Population estimate and variance
  y_hat <- sum(estimate$y_bar, na.rm=T)
  
  return(y_hat)
}

y_hat_vec <- c()
for (k in 1:K) {
  y_hat_vec <- c(y_hat_vec, run_iter(data=merged, neyman_df=neyman))
}
y_hat <- mean(y_hat_vec)

# 3. Uncertainty measures and figure ========
# Confidence intervals 
y_hat_vec <- sort(y_hat_vec, decreasing = FALSE)
CI <- c(y_hat_vec[as.integer(alpha * K)], y_hat_vec[as.integer((1 - alpha) * K)])

# Unpermitted proportion
u_hat <- (1 - formal_constructions / y_hat) * 100
CI_u <- c((1 - formal_constructions / CI[1]) * 100, (1 - formal_constructions / CI[2]) * 100)

print(paste0('Population: ', round(y_hat, 0)))
print(paste0('Population margin: ', round(y_hat - CI[1], 0)))
print(paste0('Unpermitted: ', round(y_hat - formal_constructions, 0)))
print(paste0('Unpermitted proportion: ', round(u_hat, 1)))
print(paste0('Unpermitted CI: ', round(CI_u[1], 1), ' , ', round(CI_u[2], 1)))

# Generate p-value
print(paste0('p-value: ', mean(y_hat_vec < formal_constructions)))

# Plot
plot_df <- data.frame(y_hat=y_hat_vec)
ggplot() + 
  geom_histogram(data=plot_df, aes(x=y_hat), fill='steelblue', bins=50) +
  geom_vline(xintercept=formal_constructions, color='indianred') + 
  theme_light() + 
  labs(
    x='Number of construction events (2016-2020)', 
    y='Count') +
  scale_y_continuous(label = comma) + 
  scale_x_continuous(label = comma) + 
  geom_point(data=NULL, aes(x=pop_point, y=500), color='orange') + 
  geom_errorbar(data=NULL,
    aes(xmin=pop_point - pop_margin, 
        x=pop_point, xmax=pop_point + pop_margin, y=500), 
    color='orange', width=30) + 
  annotate('text', x=1700, y=520, color='orange', label='Population\nestimate') + 
  annotate('text', x=1520, y=1000, color='steelblue', label='Bootstrap\nsamples') + 
  annotate('text', x=320, y=1000, color='indianred', 
           label='Number of\npermitted constructions', hjust = 0)
ggsave(paste0('output/Visualizations/bootstrap.png'), 
       width = 8, height = 4, dpi=300, units = "in", device='png')
