add_county_data <- function(source_dat, tract_dat){
  source_dat$fips <- as.character(source_dat$fips)
  tract_dat$geoid <- as.character(tract_dat$geoid)
  
  tract_dat %>%
    select(geoid) %>%
    mutate(county_id = substr(geoid, 1, 5)) %>%
    left_join(source_dat, by=c('county_id' = 'fips')) %>%
    select(-one_of(c('geoid', 'county_id')))
}

aggr_tract_data <- function(source_dat, tract_dat, aggr_field, aggr_fn){
  group_tract_data(source_dat, tract_dat) %>%
    summarize(count = aggr_fn(!!sym(aggr_field)))
}

get_incentives_mask <- function(source_dat, tract_dat){
  group_tract_data(source_dat, tract_dat) %>% 
    summarize(is_inzone = sum(!is.na(!!sym(names(.)[2])))) %>%
    select(-geoid)
}