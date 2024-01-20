relevel_factor <- function(fac, new.levels = NULL, ref = NULL) {

  #### Check Params ####

  if(is.null(fac)){
    stop("fac manquant")
  }

  #### Code Fonction ####

  # Cas nul
  if (is.null(new.levels)) {
    res <- as.factor(fac)
  }

  #Cas liste
  if (is.list(new.levels)) {

      # Relveling simple
      res <- factor(fac,
                    levels = names(new.levels),
                    labels = new.levels)
      }


  #Cas DF
  if (is.data.frame(new.levels) | is.matrix(new.levels)) {
    # Relveling simple
    res <- factor(fac,
                  levels = new.levels[, 1],
                  labels = new.levels[, 2])
  }

  # Changement de facteur de reference
  if (is.defined(ref)) {
    res <- relevel(res, ref)
  } else {
    res <- relevel(res, names(which.max(table(as.character(res)))))
  }

  return(res)
}
