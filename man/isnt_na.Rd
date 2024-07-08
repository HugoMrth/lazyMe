\title{
Opposite of is.na()
}\name{isnt_na}\alias{
  isnt.na
}\description{

}\usage{
  isnt.na(x)
}\arguments{
  \item{x}{
An object to be tested.
}
}\value{
  A boolean
}\author{
  Hugo Marthinet
}\examples{
x <- c(NA, NA, NA, "", NA, NA, NA, NA, "", NA, NA, NA, NA, "", NA, NA, NA, NA, "BATMAAAAAN")
is.na(x)
isnt.na(x)
}
