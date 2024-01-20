\name{rescale_norm}
\alias{rescale_norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Normalisation d'un vecteur
}
\description{
  Normalise un vecteur de données, toutes les valeurs sont recalculées pour être entre 0 et 1.
}
\usage{
  rescale_norm(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Un vecteur de données numériques.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Un vecteur numérique compris entre 0 et 1.
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

## Fonctionne avec des valeurs négatives
y <- -5:5
rescale_norm(y)

## Et aussi avec des NA
z <- c(1:4, NA, 6:10)
rescale_norm(z)
}
