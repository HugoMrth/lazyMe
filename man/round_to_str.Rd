\name{round_to_str}
\alias{round_to_str}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scientific notation of a numeric
}
\description{
Rounds and converts the numeric input into a character string. Fills up ht eright side with zeroes if needed.
}
\usage{
  round_to_str(x, digits = 3, n_decimal = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A numeric
}
  \item{digits}{
Number of decimal digits to consider for the roudning.
}
  \item{n_decimal}{
Number of decimal digits to display.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A character string
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
  \code{\link{arrondi}}
}
\examples{
round_to_str(0.5)
round_to_str(0.037, digits = 2, n_decimal = 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
