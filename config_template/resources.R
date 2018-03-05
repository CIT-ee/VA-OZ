path_to <- list(
  raw = file.path(getwd(), 'data', 'raw'),
  interim = file.path(getwd(), 'data', 'interim'),
  processed = file.path(getwd(), 'data', 'processed')
)

table_names <- list(support_programs = c('tobacco_zones_with_tract', 'hubzones_with_tract', 
                                         'technology_zones_with_tract', 'ez_with_tract', 
                                         'go_virginia_zones_with_tract', 'small_business_count'), 
                    infrastructure = c('tract_to_dc_distance_2', 'tract_to_port_distance', 
                                       'available_properties_with_tract', 'commercial_airp_count', 
                                       'general_airport_count'), 
                    quality_of_life = c('cost_of_living', 'public_schools_with_tract', 
                                        'sports_venues_count', 'hospitals_with_count', 
                                        'higher_educatio_count'),
                    industrial_base = c('net_change_establishments', 'fortune1000_va_count'),
                    financial_capital = c('commercial_banks', 'cdfis_with_trac_count', 
                                          'vc_with_tract_count'), 
                    research_dev = c('federal_federal_count', 'research_parks_count', 
                                     'r1r2_universiti_mean_distance'), 
                    'vacensustracts_geodata_censusbureau')
