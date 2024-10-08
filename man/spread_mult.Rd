\title{
  Improved \code{spread()} from \code{tidyr}
}\name{spread_mult}
\alias{spread_mult}
\description{
Allows for \code{spread()} over mutliple columns at once.
}\usage{
  spread_mult(df, key, value)
}\arguments{
  \item{data}{
    Data
}
  \item{id}{
Name or index of the key column.
}
  \item{value}{
Vector of names or indexes of the columns on which to apply the \code{spread}
}
}\detail{
  This is equivalent (and only calls) to using \code{pivotwider()}, but was created before
  I knew of it and now only serves as a placeholder.
}\author{
  Hugo Marthinet
}\seealso{
   \code{\link{spread}}
}\examples{
  Ind <- rep(1:4, 2)
  id <- c("A", "A", "A", "A", "B", "B", "B", "B")
  x1 <- c(1, 2, 3, 4, 5, 6, 7, 8)
  x2 <- rev(x1)

  (data <- cbind(Ind, id, x1, x2))

  spread_mult(df = data, key = "id", value = c("x1", "x2"))
}
