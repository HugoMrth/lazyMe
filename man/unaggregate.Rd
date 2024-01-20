\title{
  Désagrège un jeu de donnees
}
\name{unaggregate}
\alias{unaggregate.na}
\description{
  Désagrège un jeu de données agrégé par groupe selon une varible renseignant les effectifs de chaque groupe.
  Retourne un data frame contenant la ligne du groupe répétée autant de fois que d'invidus dans ce groupe.
}\usage{
  unaggregate(data, N)
}\arguments{
  \item{data}{
    Le jeu de données.
}
  \item{N}{
    La colonne renseignant les effectifs de chaque groupe.
}
}\value{
  Un data frame de dimensions sum(N)*ncol(data)-1
}\author{
  Hugo Marthinet
}\examples{
data <- data.frame(cbind(Sexe = c("H", "H", "H", "H", "F", "F", "F", "F"),
                         Age = c(18, 18, 25, 25, 18, 18, 25, 25),
                         Armee = c("T", "A", "T", "A", "T", "A", "T", "A"),
                         N = c(10, 9, 2, 5, 1, 4, 6, 3)))

data
unaggregate(data, "N")
}
