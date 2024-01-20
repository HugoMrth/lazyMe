\title{
  Inverse de is.null()
}
\name{is.defined}
\alias{is.defined}
\description{
  Retourne l'inverse de \code{is.null()}
}\usage{
  is.defined(x)
}\arguments{
  \item{x}{
    Un objet (qui peut être \code{null})
}
}\value{
  Un booléen
}\author{
  Joffrey Marchi
}\examples{
a <- 1
b <- NULL

is.defined(a)
is.defined(b)
}
