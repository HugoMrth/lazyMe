\title{
Mathematical rounding
}\name{arrondi}
\alias{arrondi}
\description{
Matheamtical rounding of numbers, 0.5 is rounded down to 0 instead of up to 1.
}\usage{
arrondi(x, digits = 0)
}\arguments{
  \item{x}{
Any kind of numerical object.
}
  \item{digits}{
Number of decimal digits, default is 0. 
}
}\value{

}\author{
  Joffrey Marchi
}\examples{
## Comparison with round()
arrondi(0.5)
round(0.5)

## Matrix example
arrondi(matrix(1:10/7, nrow = 5), digits = 3)
}
