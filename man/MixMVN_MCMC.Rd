\name{MixMVN_MCMC}
\alias{MixMVN_MCMC}
\title{
  MCMC simulation for MVN mixture distribution
}
\description{
  Function to get a MCMC simulation results based on the imported MVN mixture distribution. It is commonly used for inquiring the specified conditional probability of MVN mixture distribuiton calculated through Bayesian posteriori.
}
\usage{
# Bayesian posteriori mix MVN as input data:
# data <- MixMVN_BayesianPosteriori(dataset2[,1:4], 3)

# run MCMC simulation based on Bayesian posteriori mix MVN:
MixMVN_MCMC(data, steps, pars, values, tol, random_method, ...)
}
\arguments{
  \item{data}{
     A matrix-like data containing the mixture probability, mean vector and covariance matrix for each cluster in each row.
}
  \item{steps}{
     A positive integer. The numbers of random vectors to be generated for MCMC step.
}
  \item{pars}{
     A integer vector to declare fixed dimension(s). For example if the desired dimensions are 1st=7 and 3rd=10, set this argument as c(1,3).
}
  \item{values}{
     A numeric vector to assign value(s) to declared dimension(s). For example if the desired dimensions are 1st=7 and 3rd=10, set this argument as c(7,10).
}
  \item{tol}{
     Tolerance. A numeric value to control the generated vectors to be accepted or rejected. Criterion uses Euclidean distance in declared dimension(s). Default value is 0.3.
}
  \item{random_method}{
     The method to generate random vectors. Options are \code{"Gibbs"}: Gibbs sampling for MVN mixture model; and \code{"Fast"}: call \code{\link[mvtnorm:Mvnorm]{rmvnorm()}} to generate random vectors based on matrix factorization. Default option is \code{"Fast"}.
}
  \item{\dots}{
     Other arguments to control the process in Gibbs sampling if the \code{random_method} is "Gibbs".
}
}
\value{
return a list which contains:
\item{AcceptRate}{Acceptance of declared conditions of MCMC}
\item{MCMCdata}{All generated random vectors in MCMC step based on MVN mixture distribution}
\item{Accept}{Subset of accepted sampling in MCMCdata}
\item{Reject}{Subset of rejected sampling in MCMCdata}
}
\seealso{
 \code{\link[MVNBayesian:MixMVN_BayesianPosteriori]{MixMVN_BayesianPosteriori}}, \code{\link[MVNBayesian:MixMVN_GibbsSampler]{MixMVN_GibbsSampler}}, \code{\link[MVNBayesian:MVN_GibbsSampler]{MVN_GibbsSampler}}, \code{\link[MVNBayesian:MVN_FConditional]{MVN_FConditional}}
}
\examples{
library(plyr)
library(mvtnorm)
library(stats)

# dataset2 has 4 parameters: dimen1, dimen2, dimen3 and dimen4:
head(dataset2)
dataset2_dim <- dataset2[,1:4] # extract parametric columns

# Get posteriori parameters of dataset2 using kmeans 3 clustering:
MixBPos <- MixMVN_BayesianPosteriori(dataset2_dim, 3)

# If we want to know when dimen1=1, which clusters are accepted, run:
MixBPos_MCMC <- MixMVN_MCMC(MixBPos, steps=5000, pars=c(1), values=c(1), tol=0.3)
MixBPos_MCMC$AcceptRate
result <- MixBPos_MCMC$MCMCdata
head(result)

# count accepted samples by clustering:
count(result[which(result[,7]==1),5])
\donttest{
library(rgl)
# Visualization using plot3d() if necessary:
# Clustering result in the rest 3 dimensions:
plot3d(result[,2], result[,3], z=result[,4], col=result[,5], size=2)

# Acceptance rejection visualization:
plot3d(result[,2], result[,3], z=result[,4], col=result[,7]+1, size=2)
}
}
\keyword{MVN mixture distribution}
\keyword{Gibbs sampling}
\keyword{MCMC}
