tidyDesc_binary <- function(desc.object) {
  i <- 2
  n <- 0
  buffer_start <- sum(colnames(desc.object) %in% c("Var", "N")) + 1
  buffer_end <- sum(colnames(desc.object) %in% c("pval", "test")) # number of columns that should be kept at the end of the desc
  # Suppressing Oui/Non lines to shorten the table
  while (i < (nrow(desc.object))) {
    # Oui/non double lines detection
    # making sure ther's nothing after
    if (((desc.object[i, 1] == "  Non" & desc.object[i+1, 1] == "  Oui" ) |
         (desc.object[i, 1] == "  Oui" & desc.object[i+1, 1] == "  Non") | 
         (desc.object[i, 1] == "  Yes" & desc.object[i+1, 1] == "  No") |
         (desc.object[i, 1] == "  No" & desc.object[i+1, 1] == "  Yes")) & 
        ifelse(i+1 == nrow(desc.object), TRUE, substr(desc.object[i+2, 1], 1, 2) != "  ")) {
      # replacing empty line with the data
      
      # if yes is the first line, keep it, otherwise, skip to the next one
      rowid <- ifelse(desc.object[i, 1] == "  Oui" | desc.object[i, 1] == "  Yes", i, i+1)
      
      if (buffer_end == 0) {
        # if no buffer_end, keep the whole line
        temp <- desc.object[rowid, buffer_start:ncol(desc.object)]
      } else {
        # otherwise, fetch the buffer_end columns in the title line
        temp <- c(desc.object[rowid, buffer_start:(ncol(desc.object) - buffer_end)],
                  desc.object[i - 1, (ncol(desc.object) - buffer_end + 1):ncol(desc.object)])
      }
      
      desc.object[i-1, buffer_start:ncol(desc.object)] <- temp
      # suppressing the oui/non lines
      desc.object <- desc.object[-c(i, i+1),]
      n <- n + 1
    }
    i <- i + 1
  }
  message(paste(n, "Yes/No variables have been detected and", 2*n, "lines have been suppressed"))
  return(desc.object)
}
