\title{
  Gestion simple des facteurs
}\name{relevel_factor}\alias{relevel_factor}\description{
  Combine la fonction \code{factor()} avec une approche d'association des anciennes et nouvelles modalités
  deux par deux par une liste, ainsi que la fonction \code{relevel} afin de définir une fonction de référence. \cr

  La modalité de référence est fixée par défaut à la modalité la plus fréquente, et non pas la première à apparaître,
  ou la première par ordre alphabétique.
}\usage{
  relevel_factor(fac, new.levels = list(), ref = NULL)
}\arguments{
  \item{fac}{
    Un vecteur de chaînes de caractères ou de facteurs.
}
  \item{new.levels}{
    Une liste dont les noms des éléments les valeurs des anciennes modalités, et les valeurs, les noms des nouvelles modalités.
    Ou un datframe/une matrice dont la premiere colonne correspond aux ancienne modalites, et la deuxieme aux nouvelles.
    Par défaut, les modalités sont inchangées.
}
  \item{ref}{
    La (nouvelle) modalité de référence. Par défaut \code{ref = NULL} et la modalité
    de référence sera la plus fréquente.
}
}\value{
  Un objet de type facteur.
}\author{
  Hugo Marthinet
}\seealso{
  \code{\link{factor}}, \code{\link{relevel}}
}\examples{
## Dataset tabdata du package tab
library(tab)

# Par défaut, "Blanc" sera le facteur de référence
# Car c'est le plus fréquent
table(tabdata$Race)
relevel_factor(fac = tabdata$Race,
               new.levels = list("White" = "Blanc", "Black" = "Noir", "Mexican American" = "Hispanique", "Other" = "Autre"))

# Mais on peut le changer
relevel_factor(fac = tabdata$Race,
               new.levels = list("White" = "Blanc", "Black" = "Noir", "Mexican American" = "Hispanique", "Other" = "Autre"),
               ref = "Autre")

# Ou utiliser un dataframe ou une matrice
relevel_factor(fac = tabdata$Race,
               new.levels = cbind(c("White", "Black", "Mexican American", "Other"),
                                  c("Blanc", "Noir", "Hispanique", "Autre"))
}
