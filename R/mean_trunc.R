mean_trunc <- function(X, q = 0.95, sides = c("two.sided", "left", "right")) {

  #### Check Params ####
  if(is.null(X)) stop("X missing")
  if(!is.vector(X)) X <- as.numeric(as.character(X))

  #### Code Fonction ####
  if (sides == "right") {
    return(mean(X[X < quantile(X, q)], na.rm=TRUE))
  } else if (sides == "left") {
    return(mean(X[X > quantile(X, 1-q)], na.rm=TRUE))
  } else {
    alpha <- 1 - q
    return(mean(X[(X > quantile(X, alpha/2)) & (X < quantile(X, 1-alpha/2))], na.rm=TRUE))
  }
}
