paste_date <- function(data,
                      year = NULL, month = NULL, day = NULL,
                      hour = NULL, minute = NULL, second = NULL,
                      replace.na = TRUE, replace.day = c("15", "01")) {


  #### Check Params ####
  if(is.null(data)){
    stop("data manquant")
  }

  if(is.null(year)){
    stop("Au moins la colonne d'année doit être renseignée")
  }

  #### Code Fonction ####

  # year <- 1
  # month <- 2
  # day <- 4

  ind <- (is.defined(data[, year])) & (is.defined(as.numeric(data[, year]))) & (as.numeric(data[, year]) > 0) & (!is.na(data[, year]))
  D <- data[ind, year]
  res <- rep(NA, length(data[, year]))

  #Ajout mois
  if (is.defined(month)) {
    D <- D[!is.na(data[ind, month])]
    ind <- ind & !is.na(data[, month])
    data[is.na(data[, month]), month] <- 1
    D <- paste0(D, "-", str_pad(data[ind, month], 2, pad = "0"))
  } else {
    D <- paste0(D, "-01")
  }
  #Ajout jour
  if (is.defined(day)) {
    if (!replace.na) {
      D <- D[!is.na(data[ind, day])]
      ind <- ind & !is.na(data[, day])
    }

    data[is.na(data[, day]), day] <- match.arg(replace.day, choices = c("15", "01"), several.ok = FALSE)
    D <- paste0(D, "-", str_pad(data[ind, day], 2, pad = "0"))
  } else {
    if (replace.na) {
      D <- paste0(D, "-", match.arg(replace.day, choices = c("15", "01"), several.ok = FALSE))
    } else {
      D <- paste0(D, "-01")
    }
  }

  #Si pas de time, on calcul direct
  if (is.null(hour) & is.null(minute) & is.null(second)) {
    res[ind] <- as.character(ymd(D))
  #Sinon
  } else {
    #Ajout heure
    if (is.defined(hour)) {
      if (!replace.na) {
        D <- D[!is.na(data[ind, hour])]
        ind <- ind & !is.na(data[, hour])
      }
      data[is.na(data[, hour]), hour] <- 1
      D <- paste0(D, " ", str_pad(data[ind, hour], 2, pad = "0"))
    } else {
      D <- paste0(D, "-01")
    }
    #Ajout minute
    if (is.defined(minute)) {
      if (!replace.na) {
        D <- D[!is.na(data[ind, minute])]
        ind <- ind & !is.na(data[, minute])
      }
      data[is.na(data[, minute]), minute] <- 1
      D <- paste0(D, ":", str_pad(data[ind, minute], 2, pad = "0"))
    } else {
      D <- paste0(D, "-01")
    }
    #Ajout seconde
    if (is.defined(second)) {
      if (!replace.na) {
        D <- D[!is.na(data[ind, second])]
        ind <- ind & !is.na(data[, second])
      }
      data[is.na(data[, second]), second] <- 1
      D <- paste0(D, ":", str_pad(data[ind, second], 2, pad = "0"))
    } else {
      D <- paste0(D, "-01")
    }
    res[ind] <- as.character(ymd_hms(D))
  }

  return(res)
}

# library(stringr)
# library(lubridate)
# data <- cbind(c(rep(2012, 5), NA, rep(2013, 4)),
#               c(NA, 2:10),
#               c(10:18, NA),
#               c(3:6, NA, 8:12))
#
#
#
# paste_date(data, month = 2)
# paste_date(data, 1, 2)
#
# paste_date(data, year = 1, month = 2, day = 4)
# paste_date(data, year = 1, month = 2, day = 3, hour = 4)
# paste_date(data, year = 1, month = 2, day = 3, hour = 4, replace.na = FALSE)
