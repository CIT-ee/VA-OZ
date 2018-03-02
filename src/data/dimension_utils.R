add_county_data <- function(source.dat, tract.dat){
  source.dat$fips <- as.character(source.dat$fips)
  tract.dat$geoid <- as.character(tract.dat$geoid)
  
  tract.dat %>%
    select(geoid) %>%
    mutate(county_id = substr(geoid, 1, 5)) %>%
    left_join(source.dat, by=c('county_id' = 'fips')) %>%
    select(-one_of(c('geoid', 'county_id')))
}

aggr_tract_data <- function(source.dat, tract.dat){
  # this counts NA rows as 1. Find better way to do this
  group.tract.data(source.dat, tract.dat) %>%
    summarize(count = n()) %>%
    select(-geoid)
}

get_incentives_mask <- function(source.dat, tract.dat, srcField){
  group.tract.data(source.dat, tract.dat) %>% 
    summarize(is_inzone = n_distinct(!!sym(srcField), na.rm = TRUE)) %>%
    select(-geoid)
}