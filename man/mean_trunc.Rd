\name{mean_trunc}
\alias{mean_trunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Truncated mean
}
\description{
Compute the truncated mean of the data serie, omitting the extreme values.
}
\usage{
  mean_trunc(X, q = 0.95, direction = "both")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  A numeric vector.
}
  \item{q}{
  Quantile corresponding to the propotion of data on which to compute the mean.
}
  \item{sides}{
Default is \code{"two.sided"} and 1-q percent of the data will be ignored on either side of the data serie (both high and low values).
Other options are \code{sides = "right"} to onyl exclude high values and \code{sides = "left"} to exclude low values.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The mean as a numeric value
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Two sided
x <- c(rep(5, 5), rep(10, 90), rep(15, 5))
mean_trunc(x, q = 0.90)

## One sided
mean_trunc(x, sides = "right")
mean_trunc(x, sides = "left")
}
