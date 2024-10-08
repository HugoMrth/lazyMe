\title{
  FAIRification of a dataset
}
\name{FAIR}
\alias{FAIR}
\description{
  Following the FAIR (findability, accessibility, interoperability, and reusability) guidelines, 
extract metadatas of a data base, create a codebook and save a timestamped version in a .Rdata file.
}\usage{
  FAIR(data)
}\arguments{
  \item{data}{
    Data
}
  \item{id}{
    Id column, if present in data.
}
  \item{save.Rdata}{
    Whether or not to save a .Rdata file of the output. Default is \code{save.Rdata = FALSE}.
}
  \item{print.metadata}{
Should the output be automatically open in the Viewer. Default is \code{print.metadata = TRUE}.
}
}\value{
  A list of metadatas and other informations over the data.
}\author{
  Hugo Marthinet
}\examples{
  FAIR(iris)
}
