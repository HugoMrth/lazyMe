\title{
  Inverse de is.na()
}\name{isnt_na}\alias{
  isnt.na
}\description{
  Compile \code{!is.na(x)}. \cr
}\usage{
  isnt.na(x)
}\arguments{
  \item{x}{
    Un vecteur de valeurs.
}
}\value{
  Un vecteur de booléens de la même longueur que x.
}\author{
  Hugo Marthinet
}\examples{
x <- c(NA, NA, NA, "", NA, NA, NA, NA, "", NA, NA, NA, NA, "", NA, NA, NA, NA, "BATMAAAAAN")
is.na(x)
isnt.na(x)
}
