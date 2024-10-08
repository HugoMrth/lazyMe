write.desc <- function(desc,
                      file,
                       columnsBorders = TRUE,
                       boldLabels = TRUE,
                       backgroundColors = FALSE,
                       autoWidth = TRUE
                              # ,
                              # colors = TRUE
                              ) {

  # desc <- desc.univar(iris)
  # file <- "C:/Users/h.marthinet/Documents/Travail/34 - R/test.xlsx"

  # .........Creation Workbook ####
  wb <- createWorkbook()

  #Passage en liste si un seul df
  if (is.data.frame(desc)) {
    desc <- list(Descriptif = desc)
  }

  # .........Boucle sur les Feuilles ####
  for (i in 1:length(desc)) {
    #Gestion nom de la feuille
    sheetName <- names(desc)[i]
    if (nchar(sheetName > 30)) {
      sheetName <- substr(sheetName, 1, 30)
    }

    sheetName <- str_clean(sheetName)
    sheetName <- str_replace_all(sheetName, "'", " ")


    addWorksheet(wb, sheetName)
    writeData(wb, sheetName, desc[[i]])


    # .....Creation des styles ####

    style_border_top <- createStyle(border = "top", borderStyle = "medium")
    style_border_bot <- createStyle(border = "bottom", borderStyle = "medium")
    style_border_left <- createStyle(border = "left", borderStyle = "medium")
    style_border_right <- createStyle(border = "right", borderStyle = "medium")

    style_font_bold <- createStyle(textDecoration = "bold")

    style_background_gray <- createStyle(bgFill = "#D3D3D3")





    # .....Application des styles ####

    # Bordures ####
    if (columnsBorders) {
      #Bordure Sup premiere ligne
      conditionalFormatting(wb, sheetName,
                            cols = 2:ncol(desc[[i]]), rows = 1,
                            rule = "!=0", style = style_border_top)
      #Bordure inf premiere ligne
      conditionalFormatting(wb, sheetName,
                            cols = 1:ncol(desc[[i]]), rows = 1,
                            rule = "!=0", style = style_border_bot)
      #Bordure inf derniere ligne
      conditionalFormatting(wb, sheetName,
                            cols = 1:ncol(desc[[i]]), rows = nrow(desc[[i]]) + 1,
                            rule = "!=0", style = style_border_bot)
      #Bordure gauche premiere col
      conditionalFormatting(wb, sheetName,
                            cols = 1, rows = 1:(nrow(desc[[i]]) + 1),
                            rule = "!=0", style = style_border_left)
      #Bordure droite premiere col
      conditionalFormatting(wb, sheetName,
                            cols = 1, rows = 1:(nrow(desc[[i]]) + 1),
                            rule = "!=0", style = style_border_right)
      #Bordure droite derniere col
      conditionalFormatting(wb, sheetName,
                            cols = ncol(desc[[i]]), rows = 1:(nrow(desc[[i]]) + 1),
                            rule = "!=0", style = style_border_right)

      if (any(str_detect(colnames(desc[[i]]), "p"))){
        conditionalFormatting(wb, sheetName,
                              cols = ncol(desc[[i]]), rows = 1:(nrow(desc[[i]]) + 1),
                              rule = "!=0", style = style_border_left)
        conditionalFormatting(wb, sheetName,
                              cols = 3, rows = 1:(nrow(desc[[i]]) + 1),
                              rule = "!=0", style = style_border_right)
      }
    }



    #Gras ####
    if (boldLabels) {
      #Gras premiere ligne
      conditionalFormatting(wb, sheetName,
                            cols = 1:ncol(desc[[i]]), rows = 1,
                            rule = "!=0", style = style_font_bold)
      #Gras noms de variables
      cond_gras <- which(substr(desc[[i]][, 1], 1, 1) != " ") + 1
      for (j in cond_gras) {
        conditionalFormatting(wb, sheetName,
                              cols = 1, rows = j,
                              rule = "!=0", style = style_font_bold)
      }
    }

    #Couleurs ####

    if (backgroundColors) {
      #indices des variables
      ind_vars <- which(substr(desc[[i]][, 1], 1, 1) != " ") + 1
      #ajout fin si nb varibles impair
      if (length(ind_vars) %% 2 != 0) {
        ind_vars <- c(ind_vars, nrow(desc[[i]]) + 1)
      }

      #indices des colonnes
      ind_cols <- c()
      for (j in seq(1, length(ind_vars), by = 2)) {
        ind_cols <- c(ind_cols,
                      ind_vars[j]:(ind_vars[j+1]-1))

        if (ind_vars[j+1] == nrow(desc[[i]]) + 1) {
          ind_cols <- c(ind_cols,
                        nrow(desc[[i]]) + 1)
        }
      }
      #application du style
      for (j in ind_cols) {
        conditionalFormatting(wb, sheetName,
                              cols = 1:ncol(desc[[i]]), rows = j,
                              rule = "!=0", style = style_background_gray)
      }
    }

    #Auto Width ####
    if (autoWidth) {
      setColWidths(wb, sheetName,
                   cols = 1:ncol(desc[[i]]),
                   width = "auto")
    }

  }

  # .........Impression ####
  saveWorkbook(wb, file, TRUE)

}

# library(openxlsx)
# library(useFull)

# file <- "C:/Users/h.marthinet/Documents/Travail/34 - R/test.xlsx"
#
# desc_uni <- desc.univar(iris)
# desc_bi <- desc.bivar(iris, dependent = "Species")
# liste_desc <- list("Univarie" = desc_uni,
#                    Bivarie = desc_bi)


# desc.write(desc_uni, "C:/Users/h.marthinet/Documents/Travail/34 - R/test1.xlsx")
# desc.write(desc_bi, "C:/Users/h.marthinet/Documents/Travail/34 - R/test2.xlsx")

# desc.write(liste_desc, "C:/Users/h.marthinet/Documents/Travail/34 - R/test3.xlsx",
#            backgroundColors = TRUE)
