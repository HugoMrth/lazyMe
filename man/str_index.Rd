\name{str_index}
\alias{str_index}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Index of a pattern in a string
}
\description{
Detects the appearance of a pattern in a string and returns the indexes of those appearances.
}
\usage{
  str_clean(string, pattern,
            mode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{
    A \code{string}.
}
  \item{pattern}{
A character string of the pattern.
}
  \item{mode}{
Which appearance should be displayed. Default is \code{mode = "first"} and only the first one will be displayed in the output.
Other options are \code{mode = "all"} to display every appearances, and \code{mode = "last"} to only display the last one.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Either a list if \code{mode = "all"} or a vector if \code{mode = "first"} or \code{mode = "last"}.
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

A <- "Une fois trois egal trois"
B <- "Deux fois trois egal six"

str_index(A, "trois")
str_index(A, "trois", mode = "premiere")

str_index(c(A, B), "trois")
}
