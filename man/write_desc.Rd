\title{
   Write a descriptive object in .xlsx
}
\name{write_desc}
\alias{write.desc}
\description{
  Simple \code{write.xlsx()} placeholder, adding some parameters adapted to a descriptive object as produced by \code{datDesc::describe()}.
}\usage{
  write.desc(desc.object,
             file,
             columnsBorders = TRUE,
             boldLabels = TRUE,
             backgroundColors = FALSE,
             autoWidth = TRUE)
}\arguments{
  \item{desc.object}{
    A descriptive object as produced by \code{datDesc::describe()}.
}
  \item{file}{
    File path to write to. Path must include \code{.xlsx} extension.
}
  \item{columnsBorders}{
    Whether to add borders or not. Default is \code{columnsBorders = TRUE} and borders will be added.
}
  \item{boldLabels}{
    Whether to write variables names in bold. Default is \code{boldLabels = TRUE}.
}
  \item{backgroundColors}{
    Whether to ass some slight cell color changes (row wise) to highlight the different variables. Default is \code{backgroundColors = FALSE}.
}
  \item{autoWidth}{
    Whether to automatically adjust columns width. Default is \code{autoWidth = TRUE}. Note that this sin't recommended when columns labels are really long,
as it will only adjust the widht over a single line.
}
}\value{
  .xlsx file
}\author{
  Hugo Marthinet
}
\seealso{
  \code{\link{describe}}
}\examples{
  
}
