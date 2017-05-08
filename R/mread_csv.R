#' @title mread_csv function
#'
#' ver: 0.1.0
#'
#' Author: Dien Giau Bui (aka Richard)
#'
#' In package `rifin`
#'
#' @description
#' This load all .csv file in a folder, filter based on conditions, then rbind (i.e., rows combine) together
#'
#' @param location an address of .csv files
#' @param condition the condition of the filter
#' @return a data.frame
#' @export
#' @examples
#' mread_csv("home/", x > 0)
#'
mread_csv <- function(location, condition ="") {
  require(data.table)
  require(plyr)
  #List of files
  files <- list.files(location, pattern = ".csv", full.names = T)
  #Read all the file
  fread_filter <- function(csv_file){
    fread(csv_file)[condition]
  }
  #rbind together
  df <- rbind.fill(lapply(files, fread_filter))

  #return output
  return(df)
}
