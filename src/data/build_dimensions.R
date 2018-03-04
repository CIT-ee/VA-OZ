build_support_programs_dim <- function(tract_dat){
  incentive_zone_names <- c('tobacco_zones', 'hubzones', 'technology_zones', 
                         'ez', 'go_virginia_zones')
  zone_mask_ls <- list()
  for (zone in incentive_zone_names){
    col_name <- paste0('in_', zone)
    table_name <- paste0(zone, '_with_tract')
    zone_mask_ls[[col_name]] <- get_dataworld_dat(table_name) %>%
      get_incentives_mask(tract_dat) %>%
      rename(!!col_name := is_inzone)
  }
  sbdc_count_in_tract <- get_dataworld_dat('small_business_count') %>%
    right_join(tract_dat, by = 'geoid') %>%
    select(small_business_count)
                              
  cbind(tract_dat$geoid, data.frame(zone_mask_ls), sbdc_count_in_tract) %>%
    rename(tract_geoid = `tract_dat$geoid`)
}

build_infrastructure_dim <- function(tract_dat){
  aggregator <- function(x) sum(x, na.rm = TRUE)
  
  dist2DC_in_tract <- get_dataworld_dat('tract_to_dc_distance_2') %>%
    select(geoid, duration_hours)
  dist2port_in_tract <- get_dataworld_dat('tract_to_port_distance') %>%
    select(geoid, duration_hours)
  total_space_in_tract <- get_dataworld_dat('available_properties_with_tract') %>%
    select(geoid_census_tract, spacetotalavailable) %>%
    aggr_tract_data(tract_dat, 'spacetotalavailable', aggregator) %>%
    rename(total_space_available = count)
  
  tract_dat %>%
    select(geoid) %>%
    left_join(get_dataworld_dat('commercial_airp_count'), by = 'geoid') %>%
    left_join(get_dataworld_dat('general_airport_count'), by = 'geoid') %>%
    left_join(dist2DC_in_tract, by = 'geoid') %>%
    left_join(dist2port_in_tract, by = 'geoid', suffix = c('_dc', '_port')) %>%
    left_join(total_space_in_tract, by = 'geoid') %>%
    select(-starts_with('column_a'))
}

build_quality_of_life_dim <- function(tract_dat){
  aggregator <- function(x) sum(!is.na(x))
  
  col_index_in_tract <- get_dataworld_dat('cost_of_living') %>%
    select(fips, col_index) %>% 
    add_county_data(tract_dat)
  num_pub_schools_in_tract <- get_dataworld_dat('public_schools_with_tract') %>%
    aggr_tract_data(tract_dat, 'school_address', aggregator) %>%
    rename(num_public_schools = count)
  
  tract_dat %>%
    select(geoid) %>%
    left_join(get_dataworld_dat('sports_venues_count'), by = 'geoid') %>%
    left_join(get_dataworld_dat('hospitals_with_count'), by = 'geoid') %>%
    left_join(get_dataworld_dat('higher_educatio_count'), by = 'geoid') %>%
    left_join(col_index_in_tract, by = 'geoid') %>%
    left_join(num_pub_schools_in_tract, by = 'geoid') %>%
    select(-starts_with('column_a'))
}

build_industrial_base_dim <- function(tract_dat){
  location_quotient_in_tract <- get_dataworld_dat('cost_of_living') %>%
    select(fips, `2017_location_quotient`) %>%
    add_county_data(tract_dat)
  competitive_effect_in_tract <- get_dataworld_dat('cost_of_living') %>%
    select(fips, competitive_effect) %>%
    add_county_data(tract_dat)
  net_resilience_in_tract <- get_dataworld_dat('net_change_establishments') %>%
    select(geo_county, net_change_6years) %>%
    rename(fips = geo_county) %>%
    add_county_data(tract_dat)
  
  tract_dat %>%
    select(geoid) %>%
    left_join(get_dataworld_dat('fortune1000_va_count'), by = 'geoid') %>%
    left_join(location_quotient_in_tract, by = 'geoid') %>%
    left_join(competitive_effect_in_tract, by = 'geoid') %>%
    left_join(net_resilience_in_tract, by = 'geoid') %>%
    select(-starts_with('column_a'))
}

build_financial_capital_dim <- function(tract_dat){
  comm_banks_count_in_tract <- get_dataworld_dat('commercial_banks') %>%
    select(fips, number_of_establishments) %>%
    add_county_data(tract_dat) %>%
    rename(num_commercial_banks = number_of_establishments)
  
  tract_dat %>%
    select(geoid) %>%
    left_join(get_dataworld_dat('cdfis_with_trac_count'), by = 'geoid') %>%
    left_join(get_dataworld_dat('vc_with_tract_count'), by = 'geoid') %>%
    left_join(comm_banks_count_in_tract, by = 'geoid') %>%
    select(-starts_with('column_a'))
}

build_research_dev_dim <- function(tract_dat){
  tract_dat %>%
    select(geoid) %>%
    left_join(get_dataworld_dat('federal_federal_count'), by = 'geoid') %>%
    left_join(get_dataworld_dat('research_parks_count'), by = 'geoid') %>%
    left_join(get_dataworld_dat('r1r2_universiti_mean_distance'), by = 'geoid') %>%
    select(-starts_with('column_a'))
}
