library(dwapi)

dataset_name <- 'batten/va-oz'

#' Fetch table from data.world
#'
#' @param table_name name of table to fetch
#'
#' @return dataframe fetched from data.world
#' @export
#'
#' @examples get_dataworld('hubzone_with_tract')
get_dataworld_dat <- function(table_name){
  dwapi::download_table_as_data_frame(dataset_name, table_name)
}

#' Upload (new/modified) dataframe as file/table to data.world
#'
#' @param dat dataframe to upload
#' @param fname name of file/table on data.world
#'
#' @return None
#' @export
#'
#' @examples set_dataworld_dat(hubzone_dat, 'hubzone_with_tract')
set_dataworld_dat <- function(dat, fname){
  dwapi::upload_data_frame(dataset_name, dat, fname)
}