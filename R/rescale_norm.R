rescale_norm <- function(x) {
  #### Check Params ####

  if(is.null(x)){
    stop("x manquant")
  }

  #### Code Fonction ####

  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
