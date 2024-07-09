\name{str_as_function}
\alias{str_as_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Execute a character string as a function
}
\description{
Allows for a character string to be compute as a function name and applied using the arguments of said function.
Can be sued to apply several functions over an object in the same call.
}
\usage{
  str_as_function(func, arg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
    Vector of functions names as character strings.
}
  \item{arg}{
Argument inherited.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Function output
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
## No point doing it this way
str_as_function("mean", 1:15)

## Two calls in one
fonctions <- c("mean", "sd")
for (i in 1:2) {
  print(str_as_function(fonctions[i], 1:15))
}
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
