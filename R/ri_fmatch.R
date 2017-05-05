#' ri_fmatch function
#'
#' ver: 0.1.0
#'
#' Author: Dien Giau Bui (aka Richard)
#'
#' In package `rifin`
#'
#' Description: This function fuzzy-match 2 vector of strings and return the output vector. In this version, I allow only one key.
#'
#' We need two objects and the method to match:
#'
#' 1. name_old: is the string vector of name (that we need to match)
#'
#' 2. key: a data.frame with at least two variables: name and key (e.g, gvkey or cusip)
#'
#' 3. Method: default is "dl". Other methods:  "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex". Detail: ?stringdist::stringdist
#'
#' Example:
#'
#' data("key")
#'
#' old <- name_old$name_old
#'
#' match <- ri_fmatch(old, key)
#'
ri_fmatch <- function(name_old, key, method_type = "dl") {
  #step 1: match
  library(parallel)
  require(stringdist)
  require(tidyverse)

  # Calculate the number of cores
  no_cores <- detectCores() - 1

  # Initiate cluster
  cl_par <- makeCluster(no_cores)
  clusterExport(cl = cl_par, varlist = c("key"))


  dl_match <- parLapply(cl_par, name_old,
                        function(x) {
                          key[stringdist::amatch(x, key$conm,
                                                 matchNA = FALSE, method = method_type,
                                                 maxDist = stringr::str_length(x)/3, nthread = getOption("sd_num_thread")),]
                        }
  )
  stopCluster(cl_par)

  #step 2: return the output
  dl_match %>% unlist() %>% matrix(., ncol = 2, byrow = T) -> A
  A <- as_tibble(A)
  A <- as_tibble(data.frame(name_old, A))
  A <- rename(A, name_old = name_old, name_matched = V1, key = V2)
  return(A)
}
