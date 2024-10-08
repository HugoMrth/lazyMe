\title{
Unaggregate a dataset
}
\name{unaggregate}
\alias{unaggregate}
\description{
Unaggregate a dataset using a variable containing the frequency of each group/row.
Each group will then be copied and pasted n times, n being the value in the frequency column.
}\usage{
  unaggregate(data, N)
}\arguments{
  \item{data}{
    Data
}
  \item{N}{
    The frequency column
}
}\value{
A long dataset
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
