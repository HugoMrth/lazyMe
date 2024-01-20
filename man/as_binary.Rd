\title{
  Binarisation
}\name{as.binary}\alias{as.binary}\description{
  Converti une variable a deux modalites en variable binaire.
}\usage{
  as.binary(x,
            true.val = 1,
            na.ignore = TRUE,
            values = "logical",
            ref = "neg",
            lang = "fr")
}\arguments{
  \item{x}{
    Vecteur de donnees.
}
  \item{true.val}{
    Modalite de la valeur positive.
}
\item{na.ignore}{
    Choix de considerer ou les \code{NA} comme des valeurs negatives. Pas defaut \code{na.ignore = TRUE}
    et les valeurs manquantes seront remplacees.
}
\item{values}{
    Type de variable en sortie. Les trois options sont :
    \code{"logical"} (defaut) pour une sortie en format logique booleen,
    \code{"factor"} pour une variable facteur Oui-Non,
    \code{"numeric"} pour une variable numerique 1-0.
}
\item{ref}{
    Utilise seulement si \code{values = "factor"}. Choix de la modalite de reference du facteur.
    Peut prendre les valeurs \code{"neg"} (defaut) pour prendre la modalite negtaive comme reference,
    ou \code{"pos"} pour la modalite positive.
}
\item{lang}{
    Utilise seulement si \code{values = "factor"}. Choix du language des modalites.
    Peut prendre les valeurs \code{"fr"} (defaut) ou \code{"en"}.
}
}
\author{
  Hugo Marthinet
}\examples{
(x1 <- sample(c("Yes", "No"), 25, replace = TRUE))
(x2 <- sample(c("Yes", "No", "Maybe"), 25, replace = TRUE))
(x3 <- sample(c("Yes", "No", NA), 25, replace = TRUE))

# Errors : not runing
as.binary(x1, true.val = "Maybe")
as.binary(x1, true.val = "Yes", lang = "por")
as.binary(x1, true.val = "Yes", values = "int")
as.binary(x2, true.val = "Yes")

# Run
as.binary(x1, true.val = "Yes")
as.binary(x1, true.val = "No")
as.binary(x1, true.val = "Yes", values = "numeric")
as.binary(x1, true.val = "Yes", values = "factor")
as.binary(x1, true.val = "Yes", values = "factor", ref = "pos")
as.binary(x1, true.val = "Yes", values = "factor", lang = "en")
as.binary(x3, true.val = "Yes")
as.binary(x3, true.val = "Yes", na.ignore = FALSE)
}
