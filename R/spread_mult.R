spread_mult <- function(df, key, value) {

  #### Check Params ####

  if(is.null(df)){
    stop("df missing")
  }

  if(is.null(key)){
    stop("key missing")
  }

  if(is.null(value)){
    stop("value missing")
  }


  #### Code Fonction ####

  keyq <- rlang::enquo(key)

  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temps, value)
}
