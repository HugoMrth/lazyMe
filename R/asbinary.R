as.binary <- function(x,
                      true.val = 1,
                      na.ignore = TRUE,
                      values = "logical",
                      ref = NULL,
                      lang = NULL) {

  if (length(table(x)) != 2) {
    warning("vector isn't binary (more than 2 values) : cannot convert")
    return()
  }

  if (!(true.val %in% x)) {
    warning("'true.val' not in vector values, cannot convert")
    return()
  }

  if (!is.null(ref) & values != "factor")
    message("ref is only used when : values = 'factor'")
  if (is.null(ref))
    ref <- "neg"
  if (!is.null(lang) & values != "factor")
    message("lang is only used when : values = 'factor'")
  if (is.null(lang))
    lang <- "fr"

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





# (x1 <- sample(c("Yes", "No"), 25, replace = TRUE))
# (x2 <- sample(c("Yes", "No", "Maybe"), 25, replace = TRUE))
# (x3 <- sample(c("Yes", "No", NA), 25, replace = TRUE))
#
# # Errors
# as.binary(x1, true.val = "Maybe")
# as.binary(x1, true.val = "Yes", lang = "por")
# as.binary(x1, true.val = "Yes", values = "int")
# as.binary(x2, true.val = "Yes")
#
#
# as.binary(x1, true.val = "Yes")
# as.binary(x1, true.val = "No")
# as.binary(x1, true.val = "Yes", values = "numeric")
# as.binary(x1, true.val = "Yes", values = "factor")
# as.binary(x1, true.val = "Yes", values = "factor", ref = "pos")
# as.binary(x1, true.val = "Yes", values = "factor", lang = "en")
# as.binary(x3, true.val = "Yes")
# as.binary(x3, true.val = "Yes", na.ignore = FALSE)
