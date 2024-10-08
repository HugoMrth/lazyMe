\title{
  Censor low frequencies of qualitative variables in a descriptive object
}
\name{tidyDesc_censorLowFreq}
\alias{tidyDesc_censorLowFreq}
\description{
  Detects and replaces low frequencies in qualitative variablesof a descriptive object. Frequencies lower than 10 (default) will be replace
with "< 10", along with every other frequenciy of said variables. This is in order not to be able to trace back the low N. \cr
This is mainly used when working on the SNDS (French National Health Data Base), where one is legally bounded not to present any
result when less than 10 individuals present a given characteristic. This functions provides an easy way of censoring and putting aside
the characteristics too rare to be presented.
}\usage{
  tidyDesc_censorLowFreq(desc.object, threshold = 10)
}\arguments{
  \item{desc.object}{
    A descriptive object as outputed by the \code{describe} function.
}
  \item{threshold}{
    Threshold to consider : number of observations below which every cell will be replace with "< threshold".
}
}\value{
  A tidy descriptive object
}\author{
  Hugo Marthinet
}\examples{
  desc1 <- describe(iris[1:105,])
  desc2 <- tidyDesc_censorLowFreq(desc1)
}
