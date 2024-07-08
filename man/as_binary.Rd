\title{
  Convertion of a vector to a binary
}\name{as.binary}\alias{as.binary}\description{
Convert and format a vector into a binary variable.
}\usage{
  as.binary(x,
            true.val = 1,
            na.ignore = TRUE,
            values = "logical",
            ref = "neg",
            lang = "fr")
}\arguments{
  \item{x}{
    Vector to convert.
}
  \item{true.val}{
    Value conrseponding to the true value in the conversion.
}
\item{na.ignore}{
Whether to ignore the \code{NA} or to include them as negative values. 
Default is \code{na.ignore = TRUE} and missing values will be replaced.
}
\item{values}{
Type of the output variable. Possible options are :
\code{"logical"} (default) for a boolean,
\code{"factor"} for a Yes/No factor and
\code{"numeric"} for a 1/0 numeric.
}
\item{ref}{
Only used if \code{values = "factor"}. Choice for the reference category if converted to a factor.
Default is \code{"neg"} and the negative factor will be used as the reference, other option is \code{"pos"}.
}
\item{lang}{
Only used if \code{values = "factor"}. Language in which to translate the output factor. Default is \code{"fr"} 
for french, can also be \code{"en"} for english.
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
