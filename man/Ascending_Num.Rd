\name{Ascending_Num}
\alias{Ascending_Num}
\title{
  Renumbering vector by elemental frequency
}
\description{
  Renumbering vector by elemental frequency in ascending order.
}
\usage{
# Tidy vector by elemental frequency:
Ascending_Num(data)
}
\arguments{
  \item{data}{
     An 1d-vector.
}
}
\value{
  return a renumbered vector by elemental frequency. Factors will be positive integers arrayed in ascending order.
}
\examples{
library(plyr)

x <- c(1,2,2,2,2,2,2,2,3,3,3,1,3,3,3)
x
Ascending_Num(x)
}
\keyword{Renumbering index}
