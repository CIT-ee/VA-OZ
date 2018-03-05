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

add_county_data <- function(source_dat, tract_dat){
  source_dat$fips <- as.character(source_dat$fips)

  tract_dat %>%
    select(geoid) %>%
    mutate(county_id = substr(as.character(geoid), 1, 5)) %>%
    left_join(source_dat, by=c('county_id' = 'fips')) %>%
    mutate(geoid = as.numeric(geoid)) %>%
    select(-county_id)
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