nb.decimal <- function(double) {

  #### Check Params ####
  if(is.null(double)) stop("double missing")
  if (!is.numeric(double)) double <- as.numeric(double)

  #### Code Fonction ####
  if ((double %% 1) != 0){
    nchar(strsplit(sub('0+$', '', as.character(double)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

