convertAllInto <- function(data, type, converInto) {
  match.arg(type,
            choices = c("integer", "character", "factor", "numeric"))

  match.arg(converInto,
            choices = c("integer", "character", "factor", "numeric"))


  if (converInto == type) {
    message("parameters 'type' and converInto' are the same : cannot convert")
    return()
  } else {
    fun1 <- switch (
      type,
      integer = "is.integer",
      character = "is.character",
      factor = "is.factor",
      numeric = "is.numeric"
    )

    fun2 <- switch(
      converInto,
      integer = "as.integer",
      character = "as.character",
      factor = "as.factor",
      numeric = "as.numeric"
    )

    vec_type <- unlist(lapply(data, fun1))
    data[, vec_type] <-     ifelse(sum(vec_type) == 1,
                                   sapply(data[, vec_type], fun2),
                                   apply(data[, vec_type], 2, fun2))
    return(data)
  }
}

# convertAllInto(iris, "factor", "numeric")
# convertAllInto(iris, "numeric", "character")
# convertAllInto(iris, "factor", "factor")
# convertAllInto(iris, "factorrrrr", "factor")
