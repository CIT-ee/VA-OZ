library(dwapi)

# give dwapi access to user dataset
# dwapi::configure(auth_token = Sys.getenv("DW_AUTH_TOKEN"))
dataset_name <- 'batten/va-oz'

get_dataworld_dat <- function(table_name){
  dwapi::download_table_as_data_frame(dataset_name, table_name)
}

set_dataworld_dat <- function(dat, fname){
  dwapi::upload_data_frame(dataset_name, dat, fname)
}