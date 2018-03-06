#' Downloads any and all datasets associated with a Dimension to be saved locally
#'
#' @param table_names list (can be nested) of data.world table names to be downloaded
#'
#' @return None
#' @export
#'
#' @examples 
#' download_dimension_data('vacensustracts_geodata_censusbureau')
#' 
#' download_dimension_data(list(research_dev = c('federal_federal_count', 
#' 'research_parks_count', 'r1r2_universiti_mean_distance')))
download_dimension_data <- function(table_names){
  dim_names <- c(names(table_names))
  for(dim_idx in seq(length(table_names))){
    dim_name = dim_names[dim_idx]
    for(table_name in table_names[[dim_idx]]){
      path_to_data_frame <- file.path(path_to[['raw']], dim_name, 
                                      paste0(table_name, '.csv'))
      if(!file.exists(path_to_data_frame)){
        sprintf('Saving %s to %s', table_name, path_to_data_frame)
        save_dataframe(get_dataworld_dat(table_name), path_to_data_frame)
      }
    }
  }
}

#' Map county level data to tract level for records in source dataset
#'
#' @param source_dat source dataframe with data on the county level
#' @param tract_dat dataframe with tract information
#'
#' @return dataframe with county data mapped to tracts
#' @export
#'
#' @examples add_county_data(commercial_banks_dat, tract_va_dat)
add_county_data <- function(source_dat, tract_dat){
  source_dat$fips <- as.character(source_dat$fips)

  tract_dat %>%
    select(geoid) %>%
    mutate(county_id = substr(as.character(geoid), 1, 5)) %>%
    left_join(source_dat, by=c('county_id' = 'fips')) %>%
    mutate(geoid = as.numeric(geoid)) %>%
    select(-county_id)
}

#' Aggregrate data on the tract level based on specified variable
#'
#' @param source_dat source dataframe with data to be aggregated
#' @param tract_dat dataframe with tract information
#' @param aggr_field field/variable to base aggregation on
#' @param aggr_fn aggregation function to apply on the data
#'
#' @return dataframe with records aggregated on the tract level w.r.t specified field
#' @export
#'
#' @examples aggr_tract_data(available_properties_dat, tract_va_dat, 
#' 'spacetotalavailable', function(x) sum(!is.na(x)))
aggr_tract_data <- function(source_dat, tract_dat, aggr_field, aggr_fn){
  group_tract_data(source_dat, tract_dat) %>%
    summarize(count = aggr_fn(!!sym(aggr_field)))
}

#' Boolean mask indicating whether a tract is in zone specified in 
#' source dataframe or not
#'
#' @param source_dat source dataframe with incentive zone information
#' @param tract_dat dataframe with tract information
#'
#' @return dataframe with variable denoting if tract is in specified zone or not
#' @export
#'
#' @examples get_incentives_mask(hubzone_with_tract, tract_va_dat)
get_incentives_mask <- function(source_dat, tract_dat){
  group_tract_data(source_dat, tract_dat) %>% 
    summarize(is_inzone = sum(!is.na(!!sym(names(.)[2])))) %>%
    select(-geoid)
}