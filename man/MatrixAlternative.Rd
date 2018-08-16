\name{MatrixAlternative}
\alias{MatrixAlternative}
\title{
  Interchanging specified rows and columns
}
\description{
  Interchange all elements between two specified rows and columns in a matrix.
}
\usage{
# A matrix-like data
MatrixAlternative(data, sub, rep)
}
\arguments{
  \item{data}{
     A matrix to be processed.
}
  \item{sub}{
     A positive integer. The first selected dimension.
}
  \item{rep}{
     A positive integer. The second selected dimension. Default value is 1.
}
}
\value{
  return a matrix with interchanged rows and columns in two specified dimensions.
}
\examples{
library(plyr)

M <- matrix(1:9,3,3,1)
M
MatrixAlternative(M, 2)
}
\keyword{Matrix preprocessing}
