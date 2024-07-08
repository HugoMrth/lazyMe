\name{nb.decimal}
\alias{nb.decimal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Number of decimal digits of a numeric
}
\description{
Compute the number of decimal digits of a numeric, exlucding the excess zeroes.
}
\usage{
  nb.decimal(double)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{double}{
    A numeric.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An integer
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
  \code{\link{gregexpr}}
}
\examples{
nb.decimal(3.5)
nb.decimal(3.5500)
}
