build_support_programs_dim <- function(tract.dat){
  tobaccoZones.dat <- get.dataworld.df('tobacco_zones_with_tract')
  hubZones.dat <- get.dataworld.df('hubzones_with_tract')
  techZones.dat <- get.dataworld.df('technology_zones_with_tract')
  eZones.dat <- get.dataworld.df('ez_with_tract')
  goZones.dat <- get.dataworld.df('go_virginia_zones_with_tract')
  
  tzone_mask <- get_incentives_mask(tobaccoZones.dat, tract.dat, 'shape_starea') %>%
                rename_(in_tobacco_zone = names(.)[1])
  hubzone_mask <- get_incentives_mask(hubZones.dat, tract.dat, 'status_as_of_jan_2018') %>%
                  rename_(in_hub_zone = names(.)[1])
  techzone_mask <- get_incentives_mask(techZones.dat, tract.dat, 'shapestarea') %>%
                    rename_(in_tech_zone = names(.)[1])
  ezone_mask <- get_incentives_mask(eZones.dat, tract.dat, 'shapestarea') %>%
                rename_(in_enterprise_zone = names(.)[1])
  gozone_mask <- get_incentives_mask(goZones.dat, tract.dat, 'shapestarea') %>%
                  rename_(in_go_virginia_zone = names(.)[1])
  
  tract_geoid <- tract.dat$geoid
  cbind(tract_geoid, tzone_mask, hubzone_mask, techzone_mask, ezone_mask, gozone_mask)
}

build_infrastructure_dim <- function(tract.dat){
  aggregator <- function(x) sum(x, na.rm = TRUE)
  
  availableProperties.dat <- get.dataworld.df('available_properties_with_tract') %>%
                              select(geoid_census_tract, spacetotalavailable)
  commercialAirportCount.dat <- get.dataworld.df('commercial_airp_count')
  generalAirportCount.dat <- get.dataworld.df('general_airport_count')
  tractDCDist.dat <- get.dataworld.df('tract_to_dc_distance_2')
  tractPortDist.dat <- get.dataworld.df('tract_to_port_distance') 
  
  total_spaces_in_tract <- aggr_tract_data(availableProperties.dat, tract.dat, 
                                           'spacetotalavailable', aggregator) %>%
                            rename(total_space_available = count)
  comm_airports_count_in_tract <- commercialAirportCount.dat %>%
                                    right_join(tract.dat, by="geoid") %>%
                                    select(commercial_airp_count)
  gen_airports_count_in_tract <- generalAirportCount.dat %>%
                                  right_join(tract.dat, by="geoid") %>%
                                  select(general_airport_count)
  mean_tract_to_dc_dist <- tractDCDist.dat %>%
                            right_join(tract.dat, by="geoid") %>%
                            select(duration_hours)
  mean_tract_to_port_dist <- tractPortDist.dat %>%
                              right_join(tract.dat, by="geoid") %>%
                              select(duration_hours)
  
  
  tract_geoid <- tract.dat$geoid
  cbind(tract_geoid, total_spaces_in_tract , comm_airports_count_in_tract, 
        gen_airports_count_in_tract, mean_tract_to_dc_dist, 
        mean_tract_to_port_dist)
}

build_quality_of_life_dim <- function(tract.dat){
  aggregator <- function(x) sum(!is.na(x))
  
  costOfLiving.dat <- get.dataworld.df('cost_of_living') %>%
                      select(fips, col_index)
  publicSchools.dat <- get.dataworld.df('public_schools_with_tract')
  
  col_index_in_tract <- add_county_data(costOfLiving.dat, tract.dat)
  num_pub_schools_in_tract <- aggr_tract_data(publicSchools.dat, tract.dat, 
                                              'school_address', aggregator) %>%
                              rename(num_public_schools = count)
  
  tract_geoid <- tract.dat$geoid
  cbind(tract_geoid, col_index_in_tract, num_pub_schools_in_tract)
  
}

build_industrial_base_dim <- function(tract.dat){
  locationQuotient.dat <- get.dataworld.df('cost_of_living') %>%
                          select(fips, `2017_location_quotient`)
  competitiveEffect.dat <- get.dataworld.df('cost_of_living') %>%
                            select(fips, competitive_effect)
  fortune1000Count.dat <- get.dataworld.df('fortune1000_va_count')
  netResilience.dat <- get.dataworld.df('net_change_establishments') %>%
                        select(geo_county, net_change_6years) %>%
                        rename(fips = geo_county)
  
  location_quotient_in_tract <- add_county_data(locationQuotient.dat, tract.dat)
  competitive_effect_in_tract <- add_county_data(competitiveEffect.dat, tract.dat)
  fortune_1000_count_in_tract <- fortune1000Count.dat %>%
                                  right_join(tract.dat, by='geoid') %>%
                                  select(fortune1000_va_count)
  net_resilience_in_tract <- add_county_data(netResilience.dat, tract.dat)
  
  tract_geoid <- tract.dat$geoid
  cbind(tract_geoid, location_quotient_in_tract, competitive_effect_in_tract,
        fortune_1000_count_in_tract, net_resilience_in_tract)
}

build_financial_capital_dim <- function(tract.dat){
  commercialBanks.dat <- get.dataworld.df('commercial_banks') %>%
                          select(fips, number_of_establishments)
  cdfis.dat <- get.dataworld.df('cdfis_with_trac_count')
  ventureCapitalFirms.dat <- get.dataworld.df('vc_with_tract_count')
  
  comm_banks_count_in_tract <- add_county_data(commercialBanks.dat, tract.dat) %>%
                                rename(num_commercial_banks = number_of_establishments)
  cdfis_count_in_tract <- cdfis.dat %>%
                          right_join(tract.dat, by='geoid') %>%
                          select(cdfis_with_trac_count)
  vc_count_in_tract <- ventureCapitalFirms.dat %>%
                        right_join(tract.dat, by='geoid') %>%
                        select(vc_with_tract_count)
  
  tract_geoid = tract.dat$geoid
  cbind(tract_geoid, comm_banks_count_in_tract, 
        cdfis_count_in_tract, vc_count_in_tract)
}

build_researchNDev_dim <- function(tract.dat){
  federalLabsCount.dat <- get.dataworld.df('federal_federal_count')
  researchParksCount.dat <- get.dataworld.df('research_parks_count')
  researchUniDist.dat <- get.dataworld.df('r1r2_universiti_mean_distance')
  
  federalLabs_count_in_tract <- federalLabsCount.dat %>%
                                  right_join(tract.dat, by='geoid') %>%
                                  select(federal_federal_count)
  researchParks_count_in_tract <- researchParksCount.dat %>%
                                  right_join(tract.dat, by='geoid') %>%
                                  select(research_parks_count)
  researchUni_dist_from_tract <- researchUniDist.dat %>%
                                  right_join(tract.dat, by='geoid') %>%
                                  select(r1r2_universiti_mean_distance)
  
  tract_geoid = tract.dat$geoid
  cbind(tract_geoid, federalLabs_count_in_tract, 
        researchParks_count_in_tract, researchUni_dist_from_tract)
}
