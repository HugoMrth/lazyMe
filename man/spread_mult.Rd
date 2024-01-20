\title{
  Fonction \code{spread()} du package \code{tidyr} am?lior?e
}\name{spread_mult}
\alias{spread_mult}
\description{
  Permet de compiler la fonction \code{spread()} du package \code{tidyr} sur plusieurs colonnes ? la fois.
}\usage{
  spread_mult(df, key, value)
}\arguments{
  \item{data}{
    Le jeu de donn?es.
}
  \item{id}{
    Le nom ou l'indice de la variable cl? sur laquelle effectuer le \code{spread()}.
}
  \item{value}{
    Vecteur contenant les noms ou indices des colonnes sur lesquelles appliquer le \code{spread}.
}
}\value{
  Un data frame.
}\author{
  Hugo Marthinet
}\seealso{
   \code{\link{spread}}
}\examples{
  Ind <- rep(1:4, 2)
  id <- c("A", "A", "A", "A", "B", "B", "B", "B")
  x1 <- c(1, 2, 3, 4, 5, 6, 7, 8)
  x2 <- rev(x1)

  (data <- cbind(Ind, id, x1, x2))

  data %>% spread_mult(key = id, value = c("x1", "x2"))
}
