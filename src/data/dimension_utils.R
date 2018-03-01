add_comm_banks_data <- function(source.dat, tract.dat){
  source.dat$fips <- as.character(source.dat$fips)
  
  tract.dat %>%
    left_join(source.dat, by=c('county' = 'fips')) %>%
    rename(num_of_commercial_banks=number_of_establishments)
}