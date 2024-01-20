recode_from_dictionnary <- function(data, dictionnary, varname,
                                    id.var, id.old, id.new,
                                    type = c("QCU", "QCM"),
                                    multi.label = c("oui", "label")) {

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

  #### Gestion des parametres ####
  type <- match.arg(type, choices = c("QCU", "QCM"))
  multi.label <- match.arg(multi.label, choices = c("oui", "label"))

  if(type %ni% c("QCU", "QCM")){
    warning("type doit être 'QCU' ou 'QCM' : valeur par défaut assignee : 'QCU")
    type <- "QCU"
  }

  if(multi.label %ni% c("oui", "label")){
    warning("type doit être 'oui' ou 'label' : valeur par défaut assignee : 'oui")
    multi.label <- "oui"
  }

  #### Code fonction ####
  res <- as.factor(data[, varname])
  df_recode <- dictionnary[dictionnary[, id.var] == varname, c(id.old, id.new)]

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

head(iris)
(dic_iris <- cbind(Variable = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "Species", "Species"),
                   Code = c("Numeric", "Numeric", "Numeric", "Numeric", "setosa", "versicolor", "virginica"),
                   Label = c(NA, NA, NA, NA, "1", "2", "3")))


recode_from_dictionnary(iris, dic_iris, "Species",
                        "Variable", "Code", "Label",
                        type = "QCU")
recode_from_dictionnary(iris, dic_iris, "Species",
                        1, 2, 3,
                        type = "QCU")
