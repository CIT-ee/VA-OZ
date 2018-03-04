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
  
  comm_airport_count_dat <- get_dataworld_dat('commercial_airp_count')
  gen_airport_count_dat <- get_dataworld_dat('general_airport_count') 
  tract2DC_dist_dat <- get_dataworld_dat('tract_to_dc_distance_2') %>%
    select(geoid, duration_hours)
  tract2port_dist_dat <- get_dataworld_dat('tract_to_port_distance') %>%
    select(geoid, duration_hours)
  available_properties_dat <- get_dataworld_dat('available_properties_with_tract') %>%
    select(geoid_census_tract, spacetotalavailable)
  
  total_space_in_tract <- aggr_tract_data(available_properties_dat, tract_dat, 
                                          'spacetotalavailable', aggregator)
  tract_dat %>%
    select(geoid) %>%
    left_join(comm_airport_count_dat, by = 'geoid') %>%
    left_join(gen_airport_count_dat, by = 'geoid') %>%
    left_join(tract2DC_dist_dat, by = 'geoid') %>%
    left_join(tract2port_dist_dat, by = 'geoid', suffix = c('_dc', '_port')) %>%
    left_join(total_space_in_tract, by = 'geoid') %>%
    rename(total_space_available = count) %>%
    select(-starts_with('column_a'))
}

build_quality_of_life_dim <- function(tract_dat){
  aggregator <- function(x) sum(!is.na(x))
  
  costOfLiving.dat <- get_dataworld_dat('cost_of_living') %>%
                      select(fips, col_index)
  publicSchools.dat <- get_dataworld_dat('public_schools_with_tract')
  sportsVenuesCount.dat <- get_dataworld_dat('sports_venues_count')
  hospitalsCount.dat <- get_dataworld_dat('hospitals_with_count')
  higherEducation.dat <- get_dataworld_dat('higher_educatio_count')
  
  col_index_in_tract <- add_county_data(costOfLiving.dat, tract_dat)
  num_pub_schools_in_tract <- aggr_tract_data(publicSchools.dat, tract_dat, 
                                              'school_address', aggregator) %>%
                              rename(num_public_schools = count)
  num_sport_venues_in_tract <- sportsVenuesCount.dat %>%
                                right_join(tract_dat, by = 'geoid') %>%
                                select(sports_venues_count)
  num_hospitals_in_tract <- hospitalsCount.dat %>%
                            right_join(tract_dat, by = 'geoid') %>%
                            select(hospitals_with_count)
  num_higher_ed_in_tract <- higherEducation.dat %>%
                            right_join(tract_dat, by = 'geoid') %>%
                            select(higher_educatio_count)
  
  tract_geoid <- tract_dat$geoid
  cbind(tract_geoid, col_index_in_tract, num_pub_schools_in_tract,
        num_sport_venues_in_tract, num_hospitals_in_tract, num_higher_ed_in_tract)
  
}

build_industrial_base_dim <- function(tract_dat){
  locationQuotient.dat <- get_dataworld_dat('cost_of_living') %>%
                          select(fips, `2017_location_quotient`)
  competitiveEffect.dat <- get_dataworld_dat('cost_of_living') %>%
                            select(fips, competitive_effect)
  fortune1000Count.dat <- get_dataworld_dat('fortune1000_va_count')
  netResilience.dat <- get_dataworld_dat('net_change_establishments') %>%
                        select(geo_county, net_change_6years) %>%
                        rename(fips = geo_county)
  
  location_quotient_in_tract <- add_county_data(locationQuotient.dat, tract_dat)
  competitive_effect_in_tract <- add_county_data(competitiveEffect.dat, tract_dat)
  fortune_1000_count_in_tract <- fortune1000Count.dat %>%
                                  right_join(tract_dat, by='geoid') %>%
                                  select(fortune1000_va_count)
  net_resilience_in_tract <- add_county_data(netResilience.dat, tract_dat)
  
  tract_geoid <- tract_dat$geoid
  cbind(tract_geoid, location_quotient_in_tract, competitive_effect_in_tract,
        fortune_1000_count_in_tract, net_resilience_in_tract)
}

build_financial_capital_dim <- function(tract_dat){
  commercialBanks.dat <- get_dataworld_dat('commercial_banks') %>%
                          select(fips, number_of_establishments)
  cdfis.dat <- get_dataworld_dat('cdfis_with_trac_count')
  ventureCapitalFirms.dat <- get_dataworld_dat('vc_with_tract_count')
  
  comm_banks_count_in_tract <- add_county_data(commercialBanks.dat, tract_dat) %>%
                                rename(num_commercial_banks = number_of_establishments)
  cdfis_count_in_tract <- cdfis.dat %>%
                          right_join(tract_dat, by='geoid') %>%
                          select(cdfis_with_trac_count)
  vc_count_in_tract <- ventureCapitalFirms.dat %>%
                        right_join(tract_dat, by='geoid') %>%
                        select(vc_with_tract_count)
  
  tract_geoid = tract_dat$geoid
  cbind(tract_geoid, comm_banks_count_in_tract, 
        cdfis_count_in_tract, vc_count_in_tract)
}

build_researchNDev_dim <- function(tract_dat){
  federalLabsCount.dat <- get_dataworld_dat('federal_federal_count')
  researchParksCount.dat <- get_dataworld_dat('research_parks_count')
  researchUniDist.dat <- get_dataworld_dat('r1r2_universiti_mean_distance')
  
  federalLabs_count_in_tract <- federalLabsCount.dat %>%
                                  right_join(tract_dat, by='geoid') %>%
                                  select(federal_federal_count)
  researchParks_count_in_tract <- researchParksCount.dat %>%
                                  right_join(tract_dat, by='geoid') %>%
                                  select(research_parks_count)
  researchUni_dist_from_tract <- researchUniDist.dat %>%
                                  right_join(tract_dat, by='geoid') %>%
                                  select(r1r2_universiti_mean_distance)
  
  tract_geoid = tract_dat$geoid
  cbind(tract_geoid, federalLabs_count_in_tract, 
        researchParks_count_in_tract, researchUni_dist_from_tract)
}
