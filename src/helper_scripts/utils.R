library(dplyr)

add.tract.data <- function(source.dat, tract.dat){
  source.dat$fips <- as.character(source.dat$fips)
  left_join(source.dat, tract.dat, by=c('fips' = 'county'))  %>%
    mutate('geoid_census_tract'=paste(state, substring(fips, nchar(fips) -2, nchar(fips)), tract)) %>%
    select(c(colnames(source.dat), 'geoid_census_tract'))
}

group.tract.data <- function(source.dat, tract.dat){
  return(tract.dat %>% 
    select(geoid) %>% 
    left_join(source.dat, by = c('geoid' = 'geoid_census_tract')) %>%
    group_by(geoid))
}