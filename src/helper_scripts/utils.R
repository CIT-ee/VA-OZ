add_tract_data <- function(source_dat, tract_dat){
  source_dat$fips <- as.character(source_dat$fips)
  left_join(source_dat, tract_dat, by=c('fips' = 'county'))  %>%
    mutate('geoid_census_tract'=paste(state, substring(fips, nchar(fips) -2, nchar(fips)), tract)) %>%
    select(c(colnames(source_dat), 'geoid_census_tract'))
}

group_tract_data <- function(source_dat, tract_dat){
  tract_dat %>% 
    select(geoid) %>% 
    left_join(source_dat, by = c('geoid' = 'geoid_census_tract')) %>%
    group_by(geoid)
}