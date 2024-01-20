\name{str_as_function}
\alias{str_as_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Execution d'une fonction sous forme de chaine de caracteres.
}
\description{
  Prend une chaîne de caractères et l'éxecute en tant que fonction avec son argument associé.
  Peut être utile dans certaines situation (liste de noms de fonctions par exemple).
}
\usage{
  str_as_function(func, arg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
    Un nom de fonction sous forme de chaîne de caractères.
}
  \item{arg}{
    L'argument associé.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
    Retourne l'évaluation de la fonction.
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
## Exemple sans interêt
str_as_function("mean", 1:15)

## Celui ci peut être plus utile
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
