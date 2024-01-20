arrondi <- function(x, digits = 0) {
  .local <- function(x, digits = 0) {
    x <- x * (10^digits)
    ifelse(abs(x%%1 - 0.5) < .Machine$double.eps^0.5, ceiling(x)/(10^digits),
           round(x)/(10^digits))
  }
  if (is.data.frame(x)) {
    return(data.frame(lapply(x, .local, digits)))
  }
  .local(x, digits)
}
