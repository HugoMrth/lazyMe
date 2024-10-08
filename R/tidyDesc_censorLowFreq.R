tidyDesc_censorLowFreq <- function(desc.object, threshold = 10){
  X <- which(substr(desc.object[, 1], 1, 2) == "  ")
  dXb <- c(diff(X), 2) > 1
  listQ <- split(X, f = rep(1:sum(dXb), c(which(dXb)[1], diff(which(dXb)))))
  
  res <- desc.object
  sapply(listQ, function(x) {
    if (any(as.numeric(sub("\\ .*", "", unlist(as.vector(res[x, str_detect(colnames(res), " n")])))) < threshold)) {
      res[x, str_detect(colnames(res), " n")] <<- paste("<", threshold)
    }
  })
  res
}
