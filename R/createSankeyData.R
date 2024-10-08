createSankeyData <- function(data,
                             states,
                             timesColumns) {
  data <- as.data.frame(data)
  
  n_states <- length(states)
  n_times <- length(timesColumns)
  nodesCols <- c("#AAC0AF", "#B28B84", "#1C4073", "#0f766e", "#653239", "#472C1B", "#5C2751")[1:n_states]
  linksCols <- c("#D0DCD3", "#D0B8B4", "#285CA4", "#17B5A7", "#964A54", "#76492D", "#8F3D7E")[1:n_states]
  
  
  vals <- c()
  for (i in 2:n_times) {
    for (j in 1:n_states) {
      vals <- c(vals, table(data[, timesColumns[i]][data[, timesColumns[i-1]] == states[j]]))
    }
  }

  dataSankeyTem <- list(
    Nodes = data.frame(
      label = rep(states, n_times),
      color = rep(nodesCols, n_times)
    ),
    Links = data.frame(
      source = c(rep(1:(n_states*(n_times-1)), each = n_states)) - 1,
      target = as.vector(sapply(split((n_states+1):(n_states*n_times), rep(1:(n_times-1), each = n_states)), function(x) {rep(x, n_states)})) - 1,
      value = vals,
      color = rep(rep(linksCols, each = n_states), n_times-1)
    ))
}
