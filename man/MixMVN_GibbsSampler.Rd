\name{MixMVN_GibbsSampler}
\alias{MixMVN_GibbsSampler}
\title{
  Gibbs sampler for MVN mixture distribution
}
\description{
  Generating random vectors on the basis of a given MVN mixture distribution, through Gibbs sampling algorithm or matrix factorization.
}
\usage{
# Bayesian posteriori MVN mixture model as input data:
# data <- MixMVN_BayesianPosteriori(dataset2[,1:4], species=3)

# Generate random vectors based on Bayesian posteriori MVN mixture:
MixMVN_GibbsSampler(n, data, random_method = c("Gibbs", "Fast"), reject_rate=0, ...)
}
\arguments{
  \item{n}{
     A positive integer. The numbers of random vectors to be generated.
}
  \item{data}{
     A matrix-like data which contains the mixture probability, mean vector and covariance matrix for each cluster in each row.
}
  \item{random_method}{
     The method to generate random vectors. Options are \code{"Gibbs"}: Gibbs sampling for MVN mixture model; and \code{"Fast"}: call \code{\link[mvtnorm:Mvnorm]{rmvnorm()}} to generate random vectors based on matrix factorization.
}
  \item{reject_rate}{
     A numeric value which will be efficient if the \code{random_method} is "Gibbs": Determine the discarded items in burn-in period by ratio. Default value is 0. For details see \code{\link[MVNBayesian:MVN_GibbsSampler]{MVN_GibbsSampler}}.
}
  \item{\dots}{
     Other arguments to control the process in Gibbs sampling if the \code{random_method} is "Gibbs".
}
}
\details{
  It is recommanded using the random method of "Fast" due to the high efficiency. The time complexity of "Gibbs" method is O(k*n) where the k means dimensionality of MVN mixture model and n means generated numbers of random vectors; while that of the "Fast" method is only O(n), without considering the effect of burn-in period. this discrepancy will be even further significant when we use MCMC methods to do some further analysis in which random vectors will be generated every time when we set conditions.
}
\value{
  return a series random vectors in the basis of given MVN mixture distribution.
}
\seealso{
 \code{\link[MVNBayesian:Ascending_Num]{Ascending_Num}}, \code{\link[MVNBayesian:MixMVN_BayesianPosteriori]{MixMVN_BayesianPosteriori}}, \code{\link[MVNBayesian:MVN_BayesianPosteriori]{MVN_BayesianPosteriori}}
}
\examples{
library(plyr)
library(mvtnorm)
library(stats)

# Use dataset2 for demonstration. Get parameters of Bayesian
# posteriori multivariate normal mixture distribution
head(dataset2)
dataset2_par <- dataset2[,1:4] # only parameter columns are premitted
MixBPos <- MixMVN_BayesianPosteriori(dataset2_par, species=3)
MixBPos

# Generate random vectors using Gibbs sampling:
MixBPos_Gibbs <- MixMVN_GibbsSampler(5000, MixBPos, random_method = "Gibbs")
head(MixBPos_Gibbs)

# Compared generation speed of "Gibbs" to that of "Fast"
MixBPos_Fast <- MixMVN_GibbsSampler(5000, MixBPos, random_method = "Fast")
head(MixBPos_Fast)
\donttest{
# Visulization by clusters:
library(rgl)
dimen1 <- MixBPos_Gibbs[,1]
dimen2 <- MixBPos_Gibbs[,2]
dimen3 <- MixBPos_Gibbs[,3]
dimen4 <- MixBPos_Gibbs[,4]
plot3d(x=dimen1, y=dimen2, z=dimen3, col=MixBPos_Gibbs[,5], size=2)
}
}
\keyword{MVN mixture distribution}
\keyword{Gibbs sampling}
