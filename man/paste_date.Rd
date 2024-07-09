\name{paste_date}
\alias{paste_date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dates construction from spread up columns of a data.frame
}
\description{
Construct a date vector from spread up date element in a date frame.
Pastes the columns together and convert the character string in a Date object.
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
    Data frame to work on.
}
  \item{year}{
Year column : must be an integer.
}
  \item{month}{
Month column. Missing values are supported, but will be missing in the output vector.
}
  \item{day}{
Day column. Missing values are supported and can be replaced.
}
  \item{hour}{
    Hours
}
  \item{minute}{
    Minutes
}
  \item{second}{
    Seconds
}
  \item{replace.na}{
Whether or not to replace missing values. Does not work with years and month.
If \code{replace.na = TRUE} (default), missing values are replace with 1.
}
  \item{replace.day}{
Bypasses the default setting replacing missing days with 1. If \code{replace.day = "15"}, missing days will be imputed with 15 instead.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Date vector if hour, minutes and seconds are missing. Datatime vector otherwise.
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
# Data
data <- cbind(c(rep(2012, 5), NA, rep(2013, 4)),
              c(NA, 2:10),
              c(10:18, NA),
              c(3:6, NA, 8:12))
colnames(data) <- c("Annee", "Mois", "Jour", "Heure")
data

# Not working : Year missing
paste_date(data, month = 2)
# Working
paste_date(data, year = 1, month = "Mois")

# replace.na
paste_date(data, year = 1, month = 2, day = 3, hour = 4)
paste_date(data, year = 1, month = 2, day = 3, hour = 4, replace.na = FALSE)
}
