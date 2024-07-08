\name{rescale_norm}
\alias{rescale_norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Numeric normalization
}
\description{
Normalize a numerical vector so that all values are between 0 and 1.
}
\usage{
  rescale_norm(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A numeric vector.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Hugo Marthinet (via Internet)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- 1:10
rescale_norm(x)

## Negative values are supported
y <- -5:5
rescale_norm(y)

## NA are supported
z <- c(1:4, NA, 6:10)
rescale_norm(z)
}
