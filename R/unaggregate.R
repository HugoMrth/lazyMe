unaggregate <- function(data, N) {
  as.data.frame(data[rep(seq_len(nrow(data)), data[, N]), which(colnames(data) != N)])
}

# data <- data.frame(cbind(Sexe = c("H", "H", "H", "H", "F", "F", "F", "F"),
#                          Age = c(18, 18, 25, 25, 18, 18, 25, 25),
#                          Armee = c("T", "A", "T", "A", "T", "A", "T", "A"),
#                          N = c(10, 9, 2, 5, 1, 4, 6, 3)))
#
# data
# unaggregate(data, "N")
