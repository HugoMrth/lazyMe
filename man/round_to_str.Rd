\name{round_to_str}
\alias{round_to_str}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Ecriture scientifique de l'arrondi d'un nombre
}
\description{
  Retourne le numérique en entrée arrondi et sous forme de chaîne de caractère.
  Ajoute des zéros en fin de chaîne afin qu'elles soient toutes de longueurs égales. \cr
  Peut être très utile pour l'écriture graphique.
}
\usage{
  round_to_str(x, digits = 3, n_decimal = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Un objet numérique à arrondir.
}
  \item{digits}{
    Un chiffre correspondant nombre de décimal voulues pour l'arrondi.
}
  \item{n_decimal}{
    Un chiffre correspondant nombre de caractères voulus après la virgule.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Une chaîne de caractères.
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
