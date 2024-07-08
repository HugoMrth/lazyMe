\title{
  Opposite of %in%
}\name{not_in}\alias{not.in}\description{
}\usage{
  not.in(x, y)
  x %ni% y
}\arguments{
  \item{x}{
    A vector of values
}
  \item{y}{
    A vector of values
}
}\value{
A boolean vector the same length as x
}\author{
  Joffrey Marchi
}\examples{
x <- c("RiRi", "Fifi", "Loulou")
y <- c("Fifi", "Brindacier")

not.in(x, y)
x %ni% y
}
