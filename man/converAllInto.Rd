\title{
  Conversion of columns type in a data.frame
}\name{convertAllInto}\alias{convertAllInto}\description{
Convert every columns of a given type in a data frame to another one.
}\usage{
convertAllInto(data, type, converInto)
}\arguments{
  \item{data}{
    The data frame
}
  \item{type}{
Type of the columns to convert. See \code{Details} for supported types.
}
  \item{converInto}{
Type to convert the columns into.
}
}\details{
  Supported type are :
    \code{integer}, \code{character}, \code{factor} et \code{numeric}.
}\value{
}\author{
  Hugo Marthinet
}\examples{
head(iris)
convertAllInto(iris, "factor", "numeric")
convertAllInto(iris, "numeric", "character")
}
