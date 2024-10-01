str_index <- function(string,
                      pattern,
                      mode = c("first", "all", "last")) {

  if(is.null(string)) stop("string missing")
  if(is.null(pattern)) stop("pattern missing")

  str_index_intermediaire <- function(string, pattern) {
    unname(lapply(sapply(string, function(x) { gregexpr(pattern, x)}), as.vector))
  }

  if (length(mode) > 1) mode <- "all"

  ifelse(mode == "all",
         # Si tous les index sont a retouner
         return(str_index_intermediaire(string, pattern)),
         # Selection si seulement le premier ou le dernier
         return(unlist(lapply(str_index_intermediaire(string, pattern), function(x) {
           x[ifelse(mode == "first", 1, length(x))]})))
         )
}
