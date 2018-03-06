#' Utility to save dataframe to disk locally given specified path
#'
#' @param dat dataframe to save locally
#' @param path_to_table path on disk to save to
#'
#' @return None
#' @export
#'
#' @examples 
#' save_dataframe(hubzone_with_tract, file.path(getwd(), 'data', 'raw',
#' 'support_programs', 'hubzones_with_tract.csv') )
save_dataframe <- function(dat, path_to_table){
  path_to_data_dir <- dirname(path_to_table)
  assert_that(is.dir(path_to_data_dir))
  write.csv(dat, path_to_table, row.names = FALSE)
}

#' Load dataframe from disk locally, given the associated table name andn dimension 
#'
#' @param table_name name of the table associated with the dataframe to load
#' @param dim_name dimension name associated with the dataframe to load
#'
#' @return dataframe loaded from disk
#' @export
#'
#' @examples load_dataframe('hubzone_with_tract', 'support_programs')
load_dataframe <- function(table_name, dim_name){
  path_to_data_frame <- file.path(path_to[['raw']], dim_name, 
                                  paste0(table_name, '.csv'))
  assert_that(file.exists(path_to_data_frame))
  read.csv(path_to_data_frame, check.names = FALSE)
}

#' Add tract identifier to dataframe records
#'
#' @param source_dat source dataframe to be tagged with tract identifier
#' @param tract_dat dataframe with tract information
#'
#' @return updated dataframe with tract identifier attached to each record
#' @export
#'
#' @examples add_tract_data(hubzone_dat, tract_va_dat)
add_tract_data <- function(source_dat, tract_dat){
  source_dat$fips <- as.character(source_dat$fips)
  left_join(source_dat, tract_dat, by=c('fips' = 'county'))  %>%
    mutate('geoid_census_tract'=paste(state, substring(fips, nchar(fips) -2, nchar(fips)), tract)) %>%
    select(c(colnames(source_dat), 'geoid_census_tract'))
}

#' Group dataframe records based on the tract identifier
#'
#' @param source_dat source dataframe with records to be grouped
#' @param tract_dat dataframe with tract information
#'
#' @return dataframe with records grouped by tract identifier
#' @export
#'
#' @examples group_tract_data(available_properties_dat, tract_va_dat)
group_tract_data <- function(source_dat, tract_dat){
  tract_dat %>% 
    select(geoid) %>% 
    left_join(source_dat, by = c('geoid' = 'geoid_census_tract')) %>%
    group_by(geoid)
}