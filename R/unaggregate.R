unaggregate <- function(data, N) {
  as.data.frame(data[rep(seq_len(nrow(data)), data[, N]), which(colnames(data) != N)])
}
