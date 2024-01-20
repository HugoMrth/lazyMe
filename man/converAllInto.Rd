\title{
  Conversion de variable dans un data.frame
}\name{convertAllInto}\alias{convertAllInto}\description{
  Converti toutes les colonnes d'un type donne dans un data frame dans un autre type.
}\usage{
convertAllInto(data, type, converInto)
}\arguments{
  \item{data}{
    Le jeu de donnees.
}
  \item{type}{
    Le type des colonnes a convertir. Voir les \code{Details} pour les types de variables supportes.
}
  \item{converInto}{
    Le type dans lequel la conversion doit etre faite.
}
}\details{
  Les types supportés sont :
    \code{integer}, \code{character}, \code{factor} et \code{numeric}.
}\value{
  Retourne le meme jeu de donnees avec les colonnes converties.
}\author{
  Hugo Marthinet
}\examples{
head(iris)
convertAllInto(iris, "factor", "numeric")
convertAllInto(iris, "numeric", "character")
}
