\name{MixMVN_MCMC}
\alias{MixMVN_MCMC}
\title{
  MixMVN_MCMC
}
\description{
  Function to get a MCMC simulation results, based on multivariate normal mixture distribution Bayesian posterior model.
}
\usage{
# Bayesian posterior mix MVN as input data:
# data <- MixMVN_BayesianPosterior(dataset2[,1:4], 3)

# run MCMC simulation based on Bayesian posterior mix MVN:
MixMVN_MCMC(data, steps, pars, values, tol, random_method, ...)
}
\arguments{
  \item{data}{
     A matrix-like data which contains mixture probability, mean vector and covariance matrix for each cluster in each row.
}
  \item{steps}{
     Numbers of desired random vectors
}
  \item{pars}{
     A vector to specify dimension(s) to be fixed. if we hope the 1st and the 3rd dimension can be fixed as 7 and 10 respectively, set this argument as c(1,3).
}
  \item{values}{
     A vector to assign value(s) to specified dimension(s). if we hope the 1st and the 3rd dimension can be fixed as 7 and 10 respectively, set this argument as c(7,10).
}
  \item{tol}{
     Tolerance. A numeric value to expand the fixed dimension(s), to allow it occupy some space to receive the generated random vectors with some probability. Default value uses 0.3 (e.g. for par=c(1,3)&values=c(7,10), for a certain vector m, if 2d-norm of (m[1,3]-c(7,10)) is less than 0.3, it will be accepted).
}
  \item{random_method}{
     The method to generate random vectors. Options are \code{"Gibbs"}: Gibbs sampling for mixture model; and \code{"Fast"}: call \code{rmvnorm()} to generate random vectors based on matrix decomposition. Default option is \code{"Fast"}.
}
  \item{\dots}{
     Other arguments to control the process of Gibbs sampling if the random_method used "Gibbs"
}
}
\value{
return a list which contains:
\item{AcceptRate}{Acceptance of specified conditions of MCMC}
\item{MCMCdata}{Full generated random vectors in MCMC process based on mix MVN}
\item{Accept}{Subset of accepted sampling in MCMCdata}
\item{Reject}{Subset of rejected sampling in MCMCdata}
}
\author{
  ZHANG Chen
}
\seealso{
 MixMVN_BayesianPosterior, MixMVN_GibbsSampler, MVN_GibbsSampler, MVN_FConditional
}
\examples{
library(plyr)
library(mvtnorm)
library(stats)

# dataset2 has 4 parameters: dimen1, dimen2, dimen3 and dimen4:
head(dataset2)
dataset2_dim <- dataset2[,1:4] # extract parametric columns

# Get posterior parameters of dataset2 using kmeans 3 clustering:
MixBPos <- MixMVN_BayesianPosterior(dataset2_dim, 3)

# Generate random vectors:
MixBPos_Fast <- MixMVN_GibbsSampler(5000, MixBPos, random_method = "Fast")
head(MixBPos_Fast)

# If we want to know when dimen1=1, which clusters are accepted, run:
MixBPos_MCMC <- MixMVN_MCMC(MixBPos, steps=5000, pars=c(1), values=c(1), tol=0.3)
MixBPos_MCMC$AcceptRate
result <- MixBPos_MCMC$MCMCdata
head(result)
count(result[which(result[,7]==1),5])

## Visualization using plot3d() if necessary:
#library(rgl)
## Clustering result in the rest 3 dimensions:
#plot3d(result[,2], result[,3], z=result[,4], col=result[,5], size=2)
## Acceptance rejection visualization:
#plot3d(result[,2], result[,3], z=result[,4], col=result[,7]+1, size=2)
}
\keyword{ multivariate normal mixture distribution }
\keyword{ Gibbs sampling }
\keyword{ MCMC }
