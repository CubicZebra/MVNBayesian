\name{MatrixAlternative}
\alias{MatrixAlternative}
\title{
  MatrixAlternative
}
\description{
  exchange all elements between two specified dimensions of a square matrix.
}
\usage{
# A matrix-like data
MatrixAlternative(data, sub, rep)
}
\arguments{
  \item{data}{
     a square matrix
}
  \item{sub}{
     first selected dimension.
}
  \item{rep}{
     second selected dimension, the default value is 1.
}
}
\value{
return a matrix with rows and columns in two customized dimensions exchanged
}
\author{
  ZHANG Chen
}
\examples{
library(plyr)

M <- matrix(1:9,3,3,1)
M
MatrixAlternative(M, 2)
}
\keyword{ Interchanging columns and rows }
