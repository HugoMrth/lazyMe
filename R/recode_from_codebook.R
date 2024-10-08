recode_from_codebook <- function(data, codebook, varname,
                                    id.var, id.old, id.new,
                                    type = c("QCU", "QCM"),
                                    multi.label = c("oui", "label")) {

  #### Gestion des parametres ####
  type <- match.arg(type, choices = c("QCU", "QCM"))
  multi.label <- match.arg(multi.label, choices = c("oui", "label"))

  #### Code fonction ####
  res <- as.factor(data[, varname])
  df_recode <- codebook[codebook[, id.var] == varname, c(id.old, id.new)]

  if (type == "QCU") {
    res <- relevel_factor(res, new.levels = df_recode)
  } else {
    if (multi.label == "oui") {
      levels(res) <- c("Non", "Oui")
    } else {
      levels(res) <- c("Non", df_recode[2])
    }
  }

  return(res)
}
