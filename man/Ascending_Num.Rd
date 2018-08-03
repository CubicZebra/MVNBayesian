\name{Ascending_Num}
\alias{Ascending_Num}
\title{
  Ascending_Num
}
\description{
  Renumbering cluster vector from small to large based on frequency.
}
\usage{
# Tidy vector-like data by elemental frequency:
Ascending_Num(data)
}
\arguments{
  \item{data}{
     should be an 1d-vector contained integer elements.
}
}
\value{
  return a vector renumbered by counts of elements
}

\author{
  ZHANG Chen
}
\examples{
library(plyr)

x <- c(1,2,2,2,2,2,2,2,3,3,3,1,3,3,3)
x
Ascending_Num(x)
}
\keyword{ Renumbering }
