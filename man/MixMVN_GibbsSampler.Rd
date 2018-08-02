\name{MixMVN_GibbsSampler}
\alias{MixMVN_GibbsSampler}
\title{
  MixMVN_GibbsSampler
}
\description{
  Gibbs sampler for generating random vectors according to an given multivariate normal mixture distribution.
}
\usage{
# Bayesian posterior mix MVN as input data:
# data <- MixMVN_BayesianPosterior(dataset2[,1:4], species=3)

# Generate random vectors based on Bayesian posterior mix MVN:
MixMVN_GibbsSampler(n, data, random_method = c("Gibbs", "Fast"), reject_rate=0, ...)
}
\arguments{
  \item{n}{
     Numbers of generated random vectors
}
  \item{data}{
     A matrix-like data which contains mixture probability, mean vector and covariance matrix for each cluster in each row.
}
  \item{random_method}{
     The method to generate random vectors. Options are \code{"Gibbs"}: Gibbs sampling for mixture model; and \code{"Fast"}: call \code{\link[mvtnorm:Mvnorm]{rmvnorm}} to generate random vectors based on matrix decomposition.
}
  \item{reject_rate}{
     Effective if the random_method used "Gibbs": to discard the generated data in burn-in period by ratio. Default value is 0 since in \code{MVN_GibbsSampler()}, default initial value start near the mean vectors.
}
  \item{\dots}{
     Other arguments to control the process of Gibbs sampling if the random_method used "Gibbs"
}
}
\details{
  It is recommanded using the random method of "Fast" due to the high efficiency. The time complexity of "Gibbs" is O(k*n) where the k means dimensionality, the n means generated numbers; while that of "Fast" is only O(n), without considering the period of burn-in. this discrepancy will be even further significant when we use MCMC methods to do some further analysis. Since it librarys us frequently alter some variable to generate some random vector sets with different patterns.
}
\value{
return a certain number of random vectors based on specified mixture MVN distribution
}
\author{
  ZHANG Chen
}
\seealso{
 Ascending_Num, MixMVN_BayesianPosterior, MVN_BayesianPosterior
}
\examples{
library(plyr)
library(mvtnorm)
library(stats)

# Use dataset2 for demonstration. Get parameters of Bayesian
# posterior multivariate normal mixture distribution
head(dataset2)
dataset2_par <- dataset2[,1:4] # only parameter columns are premitted
MixBPos <- MixMVN_BayesianPosterior(dataset2_par, species=3)
MixBPos

# Generate random vectors using Gibbs sampling:
MixBPos_Gibbs <- MixMVN_GibbsSampler(5000, MixBPos, random_method = "Gibbs")
head(MixBPos_Gibbs)

# Compared generation speed of "Gibbs" to that of "Fast"
MixBPos_Fast <- MixMVN_GibbsSampler(5000, MixBPos, random_method = "Gibbs")
head(MixBPos_Fast)

## Visulization:
#library(rgl)
#dimen1 <- MixBPos_Gibbs[,1]
#dimen2 <- MixBPos_Gibbs[,2]
#dimen3 <- MixBPos_Gibbs[,3]
#dimen4 <- MixBPos_Gibbs[,4]
#plot3d(x=dimen1, y=dimen2, z=dimen3, col=MixBPos_Gibbs[,5], size=2)
}
\keyword{ multivariate normal mixture distribution }
\keyword{ Gibbs sampling }
