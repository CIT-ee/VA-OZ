add_comm_banks_data <- function(source.dat, tract.dat){
  source.dat$fips <- as.character(source.dat$fips)
  
  tract.dat %>%
    left_join(source.dat, by=c('county' = 'fips')) %>%
    rename(num_of_commercial_banks=number_of_establishments)
}

add_incentives_mask <- function(source.dat, tract.dat, srcField){
  tract.dat %>% 
    select(geoid) %>% 
    left_join(source.dat, by = c('geoid' = 'geoid_census_tract')) %>%
    group_by(geoid) %>% 
    summarize(is_inzone = n_distinct(!!sym(srcField), na.rm = TRUE)) %>%
    select(-geoid)
}