library(dplyr)
library(rlang)

source(file.path(getwd(), 'config', 'resources.R'))
source(file.path(getwd(), 'src', 'helper_scripts', 'utils.R'))
source(file.path(getwd(), 'src', 'helper_scripts', 'datadotworld_interface.R'))
source(file.path(getwd(), 'src', 'data', 'dimension_utils.R'))
source(file.path(getwd(), 'src', 'data', 'build_dimensions.R'))

download_dimension_data(table_names)

tract_va_dat <- load_dataframe('vacensustracts_geodata_censusbureau', '')

build_research_dev_dim(tract_va_dat) %>%
  write.csv(file.path(path_to[['interim']], 'research_dev_dim.csv'))

build_financial_capital_dim(tract_va_dat) %>%
  write.csv(file.path(path_to[['interim']], 'financial_capital_dim.csv'))

build_industrial_base_dim(tract_va_dat) %>%
  write.csv(file.path(path_to[['interim']], 'industrial_base_dim.csv'))

build_quality_of_life_dim(tract_va_dat) %>%
  write.csv(file.path(path_to[['interim']], 'quality_of_life_dim.csv'))

build_infrastructure_dim(tract_va_dat) %>%
  write.csv(file.path(path_to[['interim']], 'insfrastructure_dim.csv'))

build_support_programs_dim(tract_va_dat) %>%
  write.csv(file.path(path_to[['interim']], 'support_programs_dim.csv'))