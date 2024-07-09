\name{str_clean}
\alias{str_clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Character string normalisation
}
\description{
Apply succesivly \code{strini::stri_trans_general()}, \code{stringr::str_to_lower} and \code{pander::trim.spaces()}
to fully normalize a character string to prepare for textual analysing.
}
\usage{
  str_clean(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A character string.
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
  Néant
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
str_clean("    bONjour & au Revoir    é     ")
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
