# Helper functions to simulate samples (construction events) under different
# sampling schemes (simple random sample, proportional allocation, Neyman allocation). 
###############################################################################

library(splitstackshape)

# *** Parcel sampling *** ----------------------------------
# * Proportional allocation
generate_proportional <- function(sample_size, num_parcels, sj_parcels_res) {
  # Sample a fixed percentage at the level of each income strata
  sample_size_perc <- sample_size / sum(num_parcels$n)
  
  # Sample
  building_df <- sj_parcels_res %>%
    group_by(strata_inc) %>%
    slice_sample(prop=sample_size_perc, replace=FALSE) %>% 
    ungroup()
  
  building_df
}

# * Simple Random Sample
generate_SimpleRS <- function(sample_size, sj_parcels_res) {
  building_df <- sj_parcels_res %>%
    slice_sample(n=sample_size, replace=FALSE)
  
  building_df
}

# *** Income Power Analysis (Appendix D.3) *** ----------------------------------------------
generate_CEvents_from_model <- function(model, generation_type, parcel_df) { 
  if (generation_type == 'negativebinomial') {
    building_df <- parcel_df %>% 
      dplyr::mutate(pred_n_sbuild = predict(
        model, newdata=dplyr::select(parcel_df, median_inc), type='response')) %>% # Note: response already gives exp(XB) = mu
      dplyr::rowwise() %>%
      dplyr::mutate(n_sbuild = rnbinom(n=1, mu=pred_n_sbuild, size=model$theta)) %>%
      dplyr::ungroup()
  } else {
    stop('[ERROR] Check generation_type')
  }
  return(building_df)
}

generate_CEvents_Neyman <- function(
  sampling_type, neyman, CF_parcels, sample_size) {
  
  # Sample parcels from strata according to Neyman weights
  neyman$n_j <- ceiling(neyman$weight * sample_size)
  CF_parcels <- CF_parcels %>% left_join(dplyr::select(neyman, Bin, n_j), by='Bin')
  
  # Sample parcels
  if (sampling_type == 'Neyman_B1') {
    bin_sizes <- neyman$n_j
    names(bin_sizes) <- neyman$Bin
    
    building_df <- stratified(
      CF_parcels, "Bin", size=bin_sizes, replace=TRUE) %>%
      dplyr::mutate(n_sbuild = new_small_build) %>%
      dplyr::select(APN, median_inc, n_sbuild, Bin, w)
  } else {
    stop('[ERROR] Not implemented.')
  }
  
  return(building_df)
  
}

