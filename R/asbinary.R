as.binary <- function(x,
                      true.val = 1,
                      na.ignore = TRUE,
                      values = "logical",
                      ref = NULL,
                      lang = NULL) {

  if (length(table(x)) != 2)  stop("vector isn't binary (more than 2 values) : cannot convert")
  if (!(true.val %in% x)) stop("'true.val' not in vector values, cannot convert")

  if (!is.null(ref) & values != "factor") message("ref is only used when : values = 'factor'")
  if (is.null(ref)) ref <- "neg"
  if (!is.null(lang) & values != "factor") message("lang is only used when : values = 'factor'")
  if (is.null(lang)) lang <- "fr"

  match.arg(values, choices = c("logical", "factor", "numeric"))
  match.arg(lang, choices = c("fr", "en"))
  match.arg(ref, choices = c("neg", "pos"))


  val <- switch(
    values,
    logical = c(TRUE, FALSE),
    factor = c("Oui", "Non"),
    numeric = c(1, 0)
  )

  if (na.ignore) {
    res <- ifelse(!is.na(x) & x == true.val, val[1], val[2])
  } else {
    res <- rep(NA, length(x))
    res[!is.na(x)] <-
      ifelse(x[!is.na(x)] == true.val, val[1], val[2])
  }

  if (values == "factor") {
    if (lang == "fr") {
      ref_val <- ifelse(ref == "neg", "Non", "Oui")
      res <- relevel(as.factor(res), ref = ref_val)
    }
    if (lang == "en") {
      ref_val <- ifelse(ref == "neg", "No", "Yes")
      res <- relevel(factor(
        res,
        levels = c("Oui", "Non"),
        labels = c("Yes", "No")
      ),
      ref = ref_val)
    }
  }
  return(res)
}
