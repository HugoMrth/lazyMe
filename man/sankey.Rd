\title{
  Sankey
}
\name{sankey}
\alias{createSankeyData}
\alias{plotSankey}
\description{
  \code{createSankeyData()} format the raw data into nodes and links as requiredby \code{plotly} to draw a Sankey plot. \code{plotSankey()} draws the Sankey using the output of \code{createSankeyData()}.
}\usage{
  createSankeyData(data,
                   states,
                   timesColumns)

  plotSankey(Nodes, Links)
}\arguments{
  \item{data}{
  Dataset providing the data.
}
  \item{states}{
  Vector of the states to consider. This must contain every possible states.

}
  \item{timesColumns}{
  Names of the states variables at the different times. This must be provided in chronological order.
    Note that it is recommended that the states variables in the dataset are converted to factors, and that the \code{states} parameter
    provides the factor levels in the same order as in the data.
}
  \item{Nodes}{
  First element of the \code{createSankeyData()} output. Provides data to draw the Nodes of the Sankey plot.
}
  \item{Links}{
  Second element of the \code{createSankeyData()} output. Provides data to draw the Links of the Sankey plot.
}
}\value{
  A list of nodes and links required to draw a Sankey plot in \code{plotly}, and a plotly object.
}\author{
  Hugo Marthinet
}\examples{
  data <- data.frame(
    T1 = sample(c("State 1", "State 2", "State 3"), 1000, replace = TRUE),
    T2 = sample(c("State 1", "State 2", "State 3"), 1000, replace = TRUE),
    T3 = sample(c("State 1", "State 2", "State 3"), 1000, replace = TRUE)
  )


  plotData <- createSankeyData(data,
                   c("State 1", "State 2", "State 3"),
                   c("T1", "T2", "T3"))

  plotSankey(plotData$Nodes, plotData$Links)
}
