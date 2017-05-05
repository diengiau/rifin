#' mread_csv function
#' ver: 0.1.0
#' Author: Dien Giau Bui (aka Richard)
#' In package `rifin`
#' Description: This load all .csv file in a folder, filter based on conditions, then rbind (i.e., rows combine) together
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
