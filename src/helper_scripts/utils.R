save_dataframe <- function(dat, path_to_table){
  path_to_data_dir <- dirname(path_to_table)
  assert_that(is.dir(path_to_data_dir))
  write.csv(dat, path_to_table, row.names = FALSE)
}

load_dataframe <- function(table_name, dim_name){
  path_to_data_frame <- file.path(path_to[['raw']], dim_name, 
                                  paste0(table_name, '.csv'))
  assert_that(file.exists(path_to_data_frame))
  read.csv(path_to_data_frame, check.names = FALSE)
}

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