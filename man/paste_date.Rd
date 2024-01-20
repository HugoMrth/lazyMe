\name{paste_date}
\alias{paste_date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Construit un vecteur de dates
}
\description{
  Construit un vecteur de dates à partir de colonnes éclatées dans un data frame.
  Concatène les colonnes renseignées et les compile sous format date/dateTime. \cr
}
\usage{
  paste_date(data,
            year = NULL, month = NULL, day = NULL,
            hour = NULL, minute = NULL, second = NULL,
            replace.na = TRUE, replace.day = c("15", "01"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    Un data frame de données.
}
  \item{year}{
    Colonne Année : un entier correspondant au numéro de colonne ou le nom de la colonne. Obligatoire.
}
  \item{month}{
    Colonne Mois. Les valeurs manquantes sont gérées mais induisent des valeurs manquantes dans le vecteur de sortie.
}
  \item{day}{
    Colonne Jour. Les valeurs manquantes sont gérées et peuvent être remplacé.
}
  \item{hour}{
    Colonne Heure.
}
  \item{minute}{
    Colonne Minute.
}
  \item{second}{
    Colonne Seconde.
}
  \item{replace.na}{
    Choix de remplacer ou non les valeurs manquantes. Ne fonctionne pas sur les colonnes d'année ou de mois.
    Les heures, minutes et seconde sont remplacées par la valeur 1. Par défaut \code{replace.na = TRUE}.
}
  \item{replace.day}{
    Choix de remplacer les données de jour manquantes par le 1er ou le 15 du mois. Inutile si \code{replace.na = FALSE}.
    Par défaut \code{replace.day = "15"}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Un vecteur de classe Date si les heures, minutes et secondes ne sont pas renseignées. Au format dateTime sinon.
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
# Création d'un dataframe
data <- cbind(c(rep(2012, 5), NA, rep(2013, 4)),
              c(NA, 2:10),
              c(10:18, NA),
              c(3:6, NA, 8:12))
colnames(data) <- c("Annee", "Mois", "Jour", "Heure")
data

# Cet appel ne fonctionne pas car l'argument année est nécessaire
paste_date(data, month = 2)
# Celui ci oui
paste_date(data, year = 1, month = "Mois")

# Avec ou sans replace.na
# Le dernier élément du vecteur jour est NA et remplacé par 15
# Le 5eme élément du vecteur heure est NA et remplacé par 1
paste_date(data, year = 1, month = 2, day = 3, hour = 4)
paste_date(data, year = 1, month = 2, day = 3, hour = 4, replace.na = FALSE)
}
