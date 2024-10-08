FAIR <- function(data,
                 id = NULL,
                 save.Rdata = FALSE,
                 print.metadata = TRUE) {
  #### Check Params ####
  if (is.null(data)) {
    stop("data missing")
  }

  #### Code Fonction ####
  name <- deparse(substitute(data))
  data <- as.data.frame(data)

  #### Detection variables binaires ####
  varsBinaire <- sapply(data, function(x) {
    ifelse(is.numeric(x),                     # Pour toute les variables numériques
           all(names(table(x)) %in% c(0, 1)), # Si seulement des 0 ou 1 (ou NA)
           FALSE)})                           # Sinon, FALSE

  #Conversion en facteur
  data[, varsBinaire] <- lapply(data[, varsBinaire], function(x) {
    relevel_factor(as.factor(x),                             # Conversion en facteur
                   new.levels = list("0" = "0", "1" = "1"))  # On force les deux facteurs même si un seul est présent
  })


  DATA <- list(
    data = data,

    #Dictionnaire
    Codebook = data.frame(
      variable = colnames(data),
      N = sapply(data, function(x)
        sum(!is.na(x))),
      storage = sapply(data, storage.mode),
      class = sapply(data, class),
      nb.levels = unlist(lapply(sapply(data, levels), length)),
      levels = unlist(ifelse(sapply(data, class) == "factor", paste(sapply(data, levels), sep = ", "), NA))
    ),

    #Infos sur les donnees
    MetaData = list(
      df.name = name,
      colnames = colnames(data),
      rownames = rownames(data),
      varsNum = colnames(data)[sapply(data, class) == "numeric"],
      varsFactor = colnames(data)[sapply(data, class) == "factor"],
      varsChar = colnames(data)[sapply(data, class) == "character"],
      dim = dim(data),
      path = get_script_file_path()
    ),

    #NA
    NAs = list(
      df_na = data.frame(nb_na = sapply(data, function(x) sum(is.na(x))),
                          percent_na = sapply(data, function(x) sum(is.na(x)))/nrow(data)),
      nb.NA.tot = sum(is.na(data)),
      max.NA.col = max(apply(data, 2, function(x) {
        sum(is.na(x))
      })),
      max.NA.row = max(apply(data, 1, function(x) {
        sum(is.na(x))
      }))
    ),

    #Info système
    Syst = list(
      time = list(load.date = Sys.time()),
      Rversion = getRversion()
    )
  )

  if(save.Rdata) {
    file <- paste0(get_script_file_path(), "/FAIR_", name, ".Rdata")
    save(DATA, file = file)
    cat(paste("Le jeu de donnees FAIRise a ete enregistre dans le fichier : ", file))
  }
  if(print.metadata){
    View(DATA)
    cat(paste("Vous pouvez visualiser le jeu de donnees, le dictionnaire et le descriptif depuis la fenetre qui vient de s'ouvrir."))
  }
  return(DATA)
}

# IRIS <- FAIR(iris)
#
# IRIS$data[, IRIS$dictionnary$class == "factor"]
# getMethodsMetaData(iris)


# iris2 <- iris
# iris2$Species <- ifelse(iris2$Species == "virginica", 1, 0)
# iris2

# library(useFull)
# library(openxlsx)
# library(car)
# load_libraries()
# DATA <- read.xlsx("C:/Users/h.marthinet/Documents/Travail/12 - Soutien Méthodo/07 - Mathilde DE RAMBUTEAU/DATA/SAFEMEDARM 243 Réponses results-survey.xlsx",
#                   na.strings = "N/A")
#
# DATA_FAIR <- FAIR(DATA)
