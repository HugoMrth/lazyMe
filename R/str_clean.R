str_clean <- function(x) {

  #### Check Params ####

  if(is.null(x)){
    stop("x missing")
  }

  #### Code Fonction ####
  res <- stri_trans_general(str_to_lower(pander::trim.spaces(x)), id = "Latin-ASCII")
  return(res)
}
