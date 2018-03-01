library(dplyr)

add.tract.data <- function(source_df, tract_df){
  source_df$fips <- as.character(source_df$fips)
  left_join(source_df, tract_df, by=c('fips' = 'county'))  %>%
    mutate('geoid_census_tract'=paste(state, substring(fips, nchar(fips) -2, nchar(fips)), tract)) %>%
    select(c(colnames(source_df), 'geoid_census_tract'))
}