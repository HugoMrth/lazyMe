str_clean <- function(x) {
  if(is.null(x)) stop("x missing")

  stri_trans_general(str_to_lower(pander::trim.spaces(x)), id = "Latin-ASCII")
}
