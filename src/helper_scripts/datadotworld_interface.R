library(dwapi)

# give dwapi access to user dataset
# dwapi::configure(auth_token = Sys.getenv("DW_AUTH_TOKEN"))
dataset.name <- 'batten/va-oz'

get.dataworld.df <- function(table.name){
  dwapi::download_table_as_data_frame(dataset.name, table.name)
}

set.dataworld.df <- function(df, fname){
  dwapi::upload_data_frame(dataset.name, df, fname)
}