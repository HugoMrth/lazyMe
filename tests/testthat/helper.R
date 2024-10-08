# Objects or functions defined in a helper file are available to all of your tests.
x1 <- sample(c("Yes", "No"), 25, replace = TRUE)
x2 <- sample(c("Yes", "No", "Maybe"), 25, replace = TRUE)
x3 <- sample(c("Yes", "No", NA), 25, replace = TRUE)


data_agg <- data.frame(cbind(Sexe = c("H", "H", "H", "H", "F", "F", "F", "F"),
                             Age = c(18, 18, 25, 25, 18, 18, 25, 25),
                             Armee = c("T", "A", "T", "A", "T", "A", "T", "A"),
                             N = c(10, 9, 2, 5, 1, 4, 6, 3)))

Ind <- rep(1:4, 2)
id <- c("A", "A", "A", "A", "B", "B", "B", "B")
x1 <- c(1, 2, 3, 4, 5, 6, 7, 8)
x2 <- rev(x1)
data_spread <- cbind(Ind, id, x1, x2)
