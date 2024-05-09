#
# Computes household median income by census block group in the City of San Jose
# at baseline
#

library(tidyverse)
library(sf)
library(tigris)
library(censusapi)
library(mapview)

Sys.setenv(CENSUS_KEY="INSERT CENSUS KEY")

# ACS 2019 variables
acs_vars_2019_5yr <-
  listCensusMetadata(
    name = "2019/acs/acs5",
    type = "variables"
  )

# 0. Santa Clara County 1,075 census block groups and household income ======
scc_cbg <- block_groups("06","085", year = 2016)

scc_acs <- getCensus(
  name = "acs/acs5",
  vintage = 2016,
  region = "block group:*", 
  regionin = "state:06+county:085",
  vars = "B19013_001E"
) %>% 
  transmute(
    GEOID = paste0(state, county, tract, block_group),
    hh_median_inc = B19013_001E
  )

scc_join <- scc_cbg %>% 
  left_join(
    scc_acs, by = "GEOID"
  )

# Missing household income values values
sum(is.na(scc_join$hh_median_inc))

map_missing <- scc_join %>% 
  filter(hh_median_inc < 0)

mapview(map_missing)

# 1. Interpolation approach to handle CBGs with missing data ====
scc_income <- getCensus(
  name = "acs/acs5",
  vintage = 2016,
  region = "block group:*", 
  regionin = "state:06+county:085",
  vars = "group(B19001)"
) %>% 
  mutate(
    GEOID = paste0(state, county, tract, block_group)
  ) %>% 
  select(!c(GEO_ID,state,NAME) & !ends_with(c("EA","MA","M"))) %>%
  pivot_longer(
    ends_with("E"),
    names_to = "name",
    values_to = "estimate"
  ) %>% 
  group_by(GEOID, name) %>% 
  summarize(estimate = sum(estimate)) %>% 
  left_join(
    acs_vars_2019_5yr %>% 
      select(name, label)
  ) %>% 
  select(-name)

income_tiers <- 
  data.frame(
    lower_end = c(NA, 0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000, 100000, 125000, 150000, 200000),
    width = c(NA, 10000, rep(5000, 8), 10000, 15000, rep(25000, 3), 50000, NA)
  )

median_income <-
  unique(scc_income$GEOID) %>% 
  map_dfr(function(cbg){
    
    print(cbg)
    
    income <- scc_income %>% 
      filter(GEOID == cbg) %>% 
      pull(estimate)
    
    total <- income[1]
    
    if(!total > 0){
      
      data.frame(
        GEOID = cbg,
        hh_median_inc = NA
      )
      
    } else {
      
      row <- 2 
      cumulative <- income[row]
      proportion <- cumulative/total
      
      while (proportion < 0.5) {
        cumulative_lag <- cumulative
        row <- row + 1
        cumulative <- cumulative + income[row]
        proportion <- cumulative/total
      }
      
      median <- 
        income_tiers$lower_end[row] + 
        (total/2 - cumulative_lag) /
        income[row] *
        income_tiers$width[row]
      
      data.frame(
        GEOID = cbg,
        hh_median_inc = median
      )
      
    }
    
  })

retrieved <- map_missing %>% 
  left_join(
    median_income, by = "GEOID"
  )

# 2. Compare this interpolation technique with actual median incomes ====
compare <- scc_acs %>% 
  left_join(median_income, by = "GEOID")

compare %>% 
  filter(
    hh_median_inc.x > 0, 
    hh_median_inc.y > 0
  ) %>% 
  ggplot(
    aes(
      x = hh_median_inc.x,
      y = hh_median_inc.y
    )
  ) +
  geom_point()


# 3. Save output ========
scc_join_final <- scc_cbg %>% 
  left_join(
    median_income, by = "GEOID"
  )

st_write(select(scc_join_final, GEOID, hh_median_inc, geometry), 
         paste0('data/processed/PopEstimates/cbg_income_2016/cbg_income_2016.shp'))

