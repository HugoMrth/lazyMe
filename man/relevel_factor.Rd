\title{
  Factor conversion and re-leveling
}\name{relevel_factor}\alias{relevel_factor}\description{
Combines the \code{factor} and \code{relevel} function into one. Association between old and new levels
is handles trough a two by two lists matching. The default reference category is set as the most common one
instead of the base method in R being the alphabetical order.
}\usage{
  relevel_factor(fac, new.levels = list(), ref = NULL)
}\arguments{
  \item{fac}{
    String or factor vector.
}
  \item{new.levels}{
A list where each element is named after the old values of the vector levels, and the values are the new values to be attributed instead.
Can also be a two columns data.frame where the first column correponds to the old levels, and the second one to the new levels.
By default, levels are unchanged.
}
  \item{ref}{
The reference category to set. By default, the most represented category will be used.
}
}\value{
  A factor vector.
}\author{
  Hugo Marthinet
}\seealso{
  \code{\link{factor}}, \code{\link{relevel}}
}\examples{
## Using tabdata form tab
library(tab)

# "Blanc" as the ref
table(tabdata$Race)
relevel_factor(fac = tabdata$Race,
               new.levels = list("White" = "Blanc", "Black" = "Noir", "Mexican American" = "Hispanique", "Other" = "Autre"))

# Changing the ref
relevel_factor(fac = tabdata$Race,
               new.levels = list("White" = "Blanc", "Black" = "Noir", "Mexican American" = "Hispanique", "Other" = "Autre"),
               ref = "Autre")

# Alternative use
relevel_factor(fac = tabdata$Race,
               new.levels = cbind(c("White", "Black", "Mexican American", "Other"),
                                  c("Blanc", "Noir", "Hispanique", "Autre"))
}
