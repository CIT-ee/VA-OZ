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