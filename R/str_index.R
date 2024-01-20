str_index <- function(string,
                      pattern,
                      mode = c("premiere", "toutes", "derniere")) {

  #### Check Params ####

  if(is.null(string)){
    stop("string manquant")
  }

  if(is.null(pattern)){
    stop("pattern manquant")
  }

  str_index_intermediaire <- function(string, pattern) {
    unname(lapply(sapply(string, function(x) { gregexpr(pattern, x)}), as.vector))
  }

  #### Code Fonction ####

  if (length(mode) > 1) {
    mode <- "toutes"
  }

  ifelse(mode == "toutes",
         # Si tous les index sont a retouner
         return(str_index_intermediaire(string, pattern)),
         # Selection si seulement le premier ou le dernier
         return(unlist(lapply(str_index_intermediaire(string, pattern), function(x) {x[ifelse(mode == "premiere", 1, length(x))]})))
         )
}

# str_index("Bonjour, un test de la fonction svp", "o")
# str_index("Bonjour, un test de la fonction svp", "o", "premiere")
# str_index("Bonjour, un test de la fonction svp", "o", "derniere")

# str_index(c("Bonjour", "un test de la fonction", "svp"), "o")
