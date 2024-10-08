spread_mult <- function(df = NULL, key = NULL, value = NULL) {
  #### Check Params ####
  if(is.null(df)) stop("df missing")
  if(is.null(key)) stop("key missing")
  if(is.null(value)) stop("value missing")

  #### Code Fonction ####
  df <- as.data.frame(df)
  pivot_wider(data = df,
              names_from = all_of(key),
              values_from = all_of(value))
}
