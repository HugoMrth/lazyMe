\title{
  Inverse de in
}\name{not_in}\alias{not.in}\description{
  Compile \code{!(x in y)}. \cr
  Détecte les éléments de x qui ne sont pas dans y.
}\usage{
  not.in(x, y)
  x %ni% y
}\arguments{
  \item{x}{
    Un vecteur de valeurs.
}
  \item{y}{
    Un vecteur de valeurs.
}
}\value{
  Un vecteur de booléens de la même longueur que x.
}\author{
  Joffrey Marchi
}\examples{
x <- c("RiRi", "Fifi", "Loulou")
y <- c("Fifi", "Brindacier")

not.in(x, y)
x %ni% y
}
