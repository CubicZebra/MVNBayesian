\name{MixMVN_BayesianPosteriori}
\alias{MixMVN_BayesianPosteriori}
\title{
  Calculate Bayesian posteriori MVN mixture distribution
}
\description{
  The function to export the mixture probabilities, the mean vectors and covariance matrices of Bayesian posteriori MVN mixture distribution in the basis of given priori information (priori MVN mixture) and observation data (a design matrix containing all variables).
}
\usage{
# paramtric columns-only as input data:
# data <- dataset2[,1:4]

# Specify species to get parameters of MVN mixture model:
MixMVN_BayesianPosteriori(data, species, idx)
}
\arguments{
  \item{data}{
     A data.frame or matrix-like data: obervations should be arrayed in rows while variables should be arrayed in columns.
}
  \item{species}{
     A positive integer. The number of clusters for import data. It will be only called once by the next argument \code{idx} through \code{\link[stats:kmeans]{kmeans}} clustering algrithm in this function. Default value is 1, which means no clustering algrithm is used.
}
  \item{idx}{
     A vector-like data to import for accepting clustering result. Default value is generated by \code{\link[stats:kmeans]{kmeans}} clustering. Notice the length of idx should be the same as observation numbers of data (rows).
}
}
\value{
  return a matrix-like result containing all parameters of Bayesian posteriori MVN mixture distribution:
  Clusters are arrayed in rows, while the mixture probabilities, posteriori mean vectors and posteriori covariance matrices are arrayed in columns.
}
\seealso{
 \code{\link[stats:kmeans]{kmeans}}, \code{\link[MVNBayesian:MVN_BayesianPosteriori]{MVN_BayesianPosteriori}}
}
\examples{
library(plyr)

# Design matrix should only contain columns of variables
# Export will be a matrix-like data
# Using kmeans (default) clustering algrithm
data_dim <- dataset2[,1:4]
result <- MixMVN_BayesianPosteriori(data=data_dim, species=3)
result

# Get the parameters of the cluster1:
result[1,]

# Get the mixture probability of cluster2:
# (Attention to the difference between
# result[2,1][[1]] and result[2,1])
result[2,1][[1]]

# Get the mean vector of cluster1:
result[1,2][[1]]

# Get the covariance matrix of cluster3:
result[3,3][[1]]
}
\keyword{Bayesian posteriori}
\keyword{MVN mixture distribution}
