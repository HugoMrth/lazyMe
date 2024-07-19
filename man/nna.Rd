\title{
NA count
}
\name{nna}
\alias{nna}
\alias{nnonna}
\description{
  Counts the number of missing or non-missing value in a adequate object.
}\usage{
  nna(x)
  nnonna(x)
}\arguments{
  \item{x}{
A object that may contain missing value. Most likely a data vector.
}
}\value{
  An integer
}\author{
  Hugo Marthinet
}\examples{
  x <- c(NA, NA, NA, "", NA, NA, NA, NA, "", NA, NA, NA, NA, "", NA, NA, NA, NA, "BATMAAAAAN")
  nna(x)
  nnonna(x)
}
