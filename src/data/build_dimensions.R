build_support_programs_dim <- function(tract_dat){
  dim_name <- 'support_programs'
  incentive_zone_names <- c('tobacco_zones', 'hubzones', 'technology_zones', 
                         'ez', 'go_virginia_zones')
  zone_mask_ls <- list()
  for (zone in incentive_zone_names){
    col_name <- paste0('in_', zone)
    table_name <- paste0(zone, '_with_tract')
    zone_mask_ls[[col_name]] <- load_dataframe(table_name, dim_name) %>%
      get_incentives_mask(tract_dat) %>%
      rename(!!col_name := is_inzone)
  }
  
  sbdc_count_in_tract <- tract_dat %>%
    select(geoid) %>%
    left_join(load_dataframe('small_business_count', dim_name), by = 'geoid') %>%
    select(-column_a)
                              
  cbind(data.frame(zone_mask_ls), sbdc_count_in_tract)
}

build_infrastructure_dim <- function(tract_dat){
  dim_name <- 'infrastructure'
  aggregator <- function(x) sum(x, na.rm = TRUE)
  
  dist2DC_in_tract <- load_dataframe('tract_to_dc_distance_2', dim_name) %>%
    select(geoid, duration_hours)
  dist2port_in_tract <- load_dataframe('tract_to_port_distance', dim_name) %>%
    select(geoid, duration_hours)
  total_space_in_tract <- load_dataframe('available_properties_with_tract', dim_name) %>%
    select(geoid_census_tract, spacetotalavailable) %>%
    aggr_tract_data(tract_dat, 'spacetotalavailable', aggregator) %>%
    rename(total_space_available = count)
  
  tract_dat %>%
    select(geoid) %>%
    left_join(load_dataframe('commercial_airp_count', dim_name), by = 'geoid') %>%
    left_join(load_dataframe('general_airport_count', dim_name), by = 'geoid') %>%
    left_join(dist2DC_in_tract, by = 'geoid') %>%
    left_join(dist2port_in_tract, by = 'geoid', suffix = c('_dc', '_port')) %>%
    left_join(total_space_in_tract, by = 'geoid') %>%
    select(-starts_with('column_a'))
}

build_quality_of_life_dim <- function(tract_dat){
  dim_name <- 'quality_of_life'
  aggregator <- function(x) sum(!is.na(x))
  
  col_index_in_tract <- load_dataframe('cost_of_living', dim_name) %>%
    select(fips, col_index) %>% 
    add_county_data(tract_dat)
  num_pub_schools_in_tract <- load_dataframe('public_schools_with_tract', dim_name) %>%
    aggr_tract_data(tract_dat, 'school_address', aggregator) %>%
    rename(num_public_schools = count)
  
  tract_dat %>%
    select(geoid) %>%
    left_join(load_dataframe('sports_venues_count', dim_name), by = 'geoid') %>%
    left_join(load_dataframe('hospitals_with_count', dim_name), by = 'geoid') %>%
    left_join(load_dataframe('higher_educatio_count', dim_name), by = 'geoid') %>%
    left_join(col_index_in_tract, by = 'geoid') %>%
    left_join(num_pub_schools_in_tract, by = 'geoid') %>%
    select(-starts_with('column_a'))
}

build_industrial_base_dim <- function(tract_dat){
  dim_name <- 'industrial_base'
  location_quotient_in_tract <- load_dataframe('cost_of_living', 'quality_of_life') %>%
    select(fips, `2017_location_quotient`) %>%
    add_county_data(tract_dat)
  competitive_effect_in_tract <- load_dataframe('cost_of_living', 'quality_of_life') %>%
    select(fips, competitive_effect) %>%
    add_county_data(tract_dat)
  net_resilience_in_tract <- load_dataframe('net_change_establishments', dim_name) %>%
    select(geo_county, net_change_6years) %>%
    rename(fips = geo_county) %>%
    add_county_data(tract_dat)
  
  tract_dat %>%
    select(geoid) %>%
    left_join(load_dataframe('fortune1000_va_count', dim_name), by = 'geoid') %>%
    left_join(location_quotient_in_tract, by = 'geoid') %>%
    left_join(competitive_effect_in_tract, by = 'geoid') %>%
    left_join(net_resilience_in_tract, by = 'geoid') %>%
    select(-starts_with('column_a'))
}

build_financial_capital_dim <- function(tract_dat){
  dim_name <- 'financial_capital'
  comm_banks_count_in_tract <- load_dataframe('commercial_banks', dim_name) %>%
    select(fips, number_of_establishments) %>%
    add_county_data(tract_dat) %>%
    rename(num_commercial_banks = number_of_establishments)
  
  tract_dat %>%
    select(geoid) %>%
    left_join(load_dataframe('cdfis_with_trac_count', dim_name), by = 'geoid') %>%
    left_join(load_dataframe('vc_with_tract_count', dim_name), by = 'geoid') %>%
    left_join(comm_banks_count_in_tract, by = 'geoid') %>%
    select(-starts_with('column_a'))
}

build_research_dev_dim <- function(tract_dat){
  dim_name <- 'research_dev'
  tract_dat %>%
    select(geoid) %>%
    left_join(load_dataframe('federal_federal_count', dim_name), by = 'geoid') %>%
    left_join(load_dataframe('research_parks_count', dim_name), by = 'geoid') %>%
    left_join(load_dataframe('r1r2_universiti_mean_distance', dim_name), by = 'geoid') %>%
    select(-starts_with('column_a'))
}
