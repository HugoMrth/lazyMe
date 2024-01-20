\name{str_clean}
\alias{str_clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Normalise une chaine de caracteres
}
\description{
  Applique successivement les fonctions \code{strini::stri_trans_general()}, \code{stringr::str_to_lower}
  et \code{pander::trim.spaces()} à la chaîne de caractères afin de supprimer les caractères non
  ASCII, les passer en minuscule et supprimer les espaces en début et fin de chaïne.
}
\usage{
  str_clean(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Une châine de caractères.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Une chaîne de caractères normalisée.
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
