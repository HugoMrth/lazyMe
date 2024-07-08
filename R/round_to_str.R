round_to_str <- function(x, digits = 3, n_decimal = 3) {

  #### Check Params ####

  if(is.null(x)){
    stop("x missing")
  }

  #### Code Fonction ####

  format(arrondi(x, digits), nsmall = n_decimal)
}
