\title{
  Arrondi au sens mathematique
}\name{arrondi}\alias{arrondi}\description{
  Fourni l'arrondi mathematique : 0.5 n'est pas arrondi a 0 mais bien a 1.
}\usage{
arrondi(x, digits = 0)
}\arguments{
  \item{x}{
    Un nombre, vecteur ou matrice numerique.
}
  \item{digits}{
    Le nombre de chiffres apres la virgule voulu.
}
}\value{
  Le x en entrée arrondi.
}\author{
  Joffrey Marchi
}\examples{
## Difference avec le round()
arrondi(0.5)
round(0.5)

## Application à une matrice
arrondi(matrix(1:10/7, nrow = 5), digits = 3)
}
