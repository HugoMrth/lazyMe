\title{
  Recode une variable depuis un dictionnaire de donnees
}\name{recode_from_dictionnary}
\alias{recode_from_dictionnary}
\description{
  Recode une variable en se basant sur le codage present dans un dictionnaire de donnees.
}\usage{
  recode_from_dictionnary(data, dictionnary, varname,
                          id.var, id.old, id.new,
                          type = c("QCU", "QCM"),
                          multi.label = c("oui", "label"))
}\arguments{
  \item{data}{
    Le jeu de donnees
}
  \item{dictionnary}{
    Le dictionnaire de donnees
}
  \item{varname}{
    Le nom de la variable a recoder. Doit etre le meme dans la dataset et le dictionnaire.
}
  \item{id.var}{
    Le nom de la colonne correspondant aux noms des variables dans le dictionnaire.
}
  \item{id.old}{
    Le nom de la colonne correspondant aux codage des modalites dans le dictionnaire.
}
  \item{id.new}{
    Le nom de la colonne correspondant aux libelles des codes dans le dictionnaire.
}
  \item{type}{
    Choix du type de question, QCU ou QCM. Par defaut \code{type = "QCU"}.
}
  \item{multi.label}{
    Choix de transformer les variables QCM en Oui/Non (\code{multi.label = "oui"}, ou en libelle/non  (\code{multi.label = "label"}).
    Par defaut (\code{multi.label = "oui"}.
}
}\value{
  Un vecteur sous forme de facteur.
}\author{
  Hugo Marthinet
}\examples{
head(iris)
(dic_iris <- cbind(Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "Species", "Species"),
                   Code = c("Numeric", "Numeric", "Numeric", "Numeric", "setosa", "versicolor", "virginica"),
                   Label = c(NA, NA, NA, NA, "1", "2", "3")))


recode_from_dictionnary(iris, dic_iris, "Species",
                        "Variable", "Code", "Label",
                        type = "QCU")
}
