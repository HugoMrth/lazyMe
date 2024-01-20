\name{mean_trunc}
\alias{mean_trunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Moyenne tronquee
}
\description{
  Calcule la moyenne tronquée. Omet les valeurs les plus élevées et/ou les plus faibles.
}
\usage{
  mean_trunc(X, q = 0.95, direction = "both")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  Un vecteur numérique.
}
  \item{q}{
  Le quantile correspondant au pourcentage des données à considérer.
}
  \item{sides}{
  Par défaut \code{"two.sided"}, omet les \code{1-q} valeurs les plus élevées et les plus faibles.
  Si \code{sides = "right"}, n'exclu que les valeurs hautes.
  Si \code{sides = "left"}, n'exclu que les valeurs basses.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Retourne la moyenne en numérique.
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
## Utilisation classique
x <- c(rep(5, 5), rep(10, 90), rep(15, 5))
mean_trunc(x, q = 0.90)

## En excluant les valeurs extrêmes à droite ou à gauche
mean_trunc(x, sides = "right")
mean_trunc(x, sides = "left")
}
