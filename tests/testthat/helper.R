# Objects or functions defined in a helper file are available to all of your tests.
library(dplyr)

iris2 <- iris %>%
  mutate(
    Weights = 1
  )

iris_na <- iris
iris_na$Sepal.Length[c(12, 15, 59, 62, 78)] <- NA
iris_na$Species[c(12, 15, 59, 62, 78)] <- NA



desc11 <- dataDesc::describe(iris %>% mutate(Species = ifelse(Species == "setosa", "Yes", "No")))
desc12 <- tidyDesc_binary(desc11)

desc21 <- dataDesc::describe(iris[1:105,])
desc22 <- tidyDesc_censorLowFreq(desc21)

desc1 <- dataDesc::describe(iris)
desc2 <- dataDesc::describe(iris, factor = "Species")
desc3 <- dataDesc::describe(iris, factor = "Species", include.test.name = T)
