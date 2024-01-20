\name{str_index}
\alias{str_index}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Retourne l'indice d'apparition d'un patterne.
}
\description{
  Detecte l'apparition d'un patterne dans une chaine de caracteres ou un vecteur de chaines de caracteres.
  Retourne les emplacements - en terme de numero de caractere - d'apparition des patternes detectes.
}
\usage{
  str_clean(string, pattern,
            mode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{string}{
    Une châine de caractères ou vecteur de class \code{string}.
}
  \item{pattern}{
    La châine de caractères donc on veut detecter la presence.
}
  \item{pattern}{
    Choix de retourner le/les indices de la premiere (\code{mode = "premiere"}) occurence du patterne,
    toutes les occurences (\code{mode = "toutes"}), ou uniquement la derniere  (\code{mode = "derniere"}).
    Par defaut, \code{mode = "toutes"}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Si \code{mode = "toutes"}, une liste de vecteurs contenant les positions, sinon, un vecteur contenant les positions.
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
