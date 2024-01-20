str_as_function <- function(f, arg) {
  eval(parse(text = paste0(f, "(arg)")))
}
