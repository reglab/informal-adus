# Helper functions to simulate samples (construction events) under different
# sampling schemes (simple random sample, proportional allocation, Neyman allocation).
###############################################################################

library(splitstackshape)

# *** Parcel sampling *** ----------------------------------
# * Proportional allocation
generate_proportional <- function(sample_size, df, group_col) {
  # Sample a fixed percentage at the level of each group
  sample_size_perc <- sample_size / dim(df)[1]
  
  # Sample
  sample_df <- df %>%
    group_by_at(group_col) %>%
    slice_sample(prop=sample_size_perc, replace=FALSE) %>% 
    ungroup()
  
  sample_df
}

# * Simple Random Sample
generate_SimpleRS <- function(sample_size, df) {
  sample_df <- df %>%
    slice_sample(n=sample_size, replace=FALSE)
  
  sample_df
}

# * Stratified Sample per Neyman Allocation
generate_Neyman <- function(sample_size, df, neyman_df, group_col) {
  # Parcel database
  df <- df %>% dplyr::select_at(c('APN', 'Bin', group_col))
  
  # Sample parcels from strata according to Neyman weights
  neyman_df$n_j <- ceiling(neyman_df$Neyman_weight * sample_size)
  df <- df %>% left_join(
    neyman_df %>% dplyr::select_at(c(group_col, 'n_j')), by=group_col)
  
  # Sample parcels
  bin_sizes <- neyman_df$n_j
  names(bin_sizes) <- neyman_df[[group_col]]
  
  sample_df <- stratified(
    df, group_col, size=bin_sizes, replace=TRUE) %>%
    dplyr::select_at(c('APN', 'Bin', group_col))
  
  return(sample_df)
  
}

# Sample construction events
generate_constructions <- function(
    parcel_df, prob_df, group_col, permit_p, dgp) {
  # parcel_df is a df of APNs and the group they belong to
  
  # For each parcel, we sample the number of ADU constructions. 
  prob_df <- prob_df %>% dplyr::rename(p0 = `0`, p1 = `1`, p2 = `2`)
  
  merge_col <- if (dgp == 'gt') 'Bin' else group_col
  constructions_df <- parcel_df %>%
    dplyr::left_join(prob_df, by=merge_col) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      new_ADUs = sample(c(0, 1, 2), 1, prob=c(p0, p1, p2))
    )
  
  # We keep only parcels with constructions, and uncount so that our 
  # df is now at the construction level. This way, we keep the APN and
  # associated weight that we'll need when computing the point estimate
  # and CIs later. 
  num_new_ADUs <- sum(constructions_df$new_ADUs)
  constructions_df <- constructions_df %>%
    dplyr::filter(new_ADUs > 0) %>%
    uncount(new_ADUs) %>%
    dplyr::select_at(c('APN', group_col, 'w'))
  stopifnot(num_new_ADUs == dim(constructions_df)[1])
  
  # We determine the permit status of each building using a Ber(p)
  constructions_df <- constructions_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(permitted = rbinom(1, 1, permit_p))
  
  return(constructions_df)
}
