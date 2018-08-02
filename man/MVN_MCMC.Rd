\name{MVN_MCMC}
\alias{MVN_MCMC}
\title{
  MVN_MCMC
}
\description{
  Function to get a MCMC simulation results, based on multivariate normal distribution Bayesian posterior model.
}
\usage{
# Bayesian posterior as input data
# data <- MVN_BayesianPosterior(dataset1, pri_mean=c(80,16,3))

# run MCMC simulation using Bayesian posterior:
MVN_MCMC(data, steps, pars, values, tol, ...)
}
\arguments{
  \item{data}{
     A bivariate list which contains the mean vector (\code{data$mean}) and the covariance matrix (\code{data$var}) of the given multivariate normal distribution.
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
  \item{\dots}{
     Other arguments to control the process of Gibbs sampling.
}
}
\value{
return a list which contains:
\item{AcceptRate}{Acceptance of specified conditions of MCMC}
\item{MCMCdata}{Full generated random vectors in MCMC process based on MVN}
\item{Accept}{Subset of accepted sampling in MCMCdata}
\item{Reject}{Subset of rejected sampling in MCMCdata}
}
\author{
  ZHANG Chen
}
\seealso{
 MVN_GibbsSampler, MVN_FConditional
}
\examples{
library(mvtnorm)
library(plyr)

# dataset1 has three parameters: fac1, fac2 and fac3:
head(dataset1)

# Get posterior parameters of dataset1 using prior of c(80,16,3):
BPos <- MVN_BayesianPosterior(dataset1, pri_mean=c(80,16,3))

# If we want to know when fac1=78, how fac2 responses to fac3, run:
BPos_MCMC <- MVN_MCMC(BPos, steps=8000, pars=c(1), values=c(78), tol=0.3)
MCMC <- BPos_MCMC$MCMCdata
head(MCMC)

## Visualization using plot3d() if necessary:
#library(rgl)
#plot3d(MCMC[,1], MCMC[,2], z=MCMC[,3], col=MCMC[,5]+1, size=2)

# Visualization: 2d scatter plot
MCMC_2d <- BPos_MCMC$Accept
head(MCMC_2d)
plot(MCMC_2d[,3], MCMC_2d[,2], pch=20, col="red", xlab = "fac3", ylab = "fac2")
# Compared to the following scatter plot when fac1 is not fixed:
plot(BPos_MCMC$MCMCdata[,3], BPos_MCMC$MCMCdata[,2], pch=20, col="red", xlab = "fac3",
ylab = "fac2")
}
\keyword{ multivariate normal distribution }
\keyword{ Gibbs sampling }
\keyword{ MCMC }
