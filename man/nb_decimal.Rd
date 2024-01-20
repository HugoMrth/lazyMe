\name{nb.decimal}
\alias{nb.decimal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Retourne le nombre de chiffre après la virgule d'un réel.
}
\description{
  Retourne le nombre de chiffre après la virgule d'un réel.
  Exclu les 0 en trop.
}
\usage{
  nb.decimal(double)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{double}{
    Un réel.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Un entier.
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
