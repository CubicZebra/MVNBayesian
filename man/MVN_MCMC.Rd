\name{MVN_MCMC}
\alias{MVN_MCMC}
\title{
  MCMC simulation for MVN distribution
}
\description{
  Function to get a MCMC simulation results based on the imported MVN distribution. It is commonly used for inquiring the specified conditional probability of MVN distribuiton calculated through Bayesian posteriori.
}
\usage{
# Bayesian posteriori as input data
# data <- MVN_BayesianPosteriori(dataset1, pri_mean=c(80,16,3))

# run MCMC simulation using Bayesian posteriori:
MVN_MCMC(data, steps, pars, values, tol, ...)
}
\arguments{
  \item{data}{
     A double level list which contains the mean vector (\code{data$mean}) and the covariance matrix (\code{data$var}) of a given MVN distribution.
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
  \item{\dots}{
     Other arguments to control the process in Gibbs sampling.
}
}
\value{
return a list which contains:
\item{AcceptRate}{Acceptance of declared conditions of MCMC}
\item{MCMCdata}{All generated random vectors in MCMC step based on MVN distribution}
\item{Accept}{Subset of accepted sampling in MCMCdata}
\item{Reject}{Subset of rejected sampling in MCMCdata}
}
\seealso{
 \code{\link[MVNBayesian:MVN_GibbsSampler]{MVN_GibbsSampler}}, \code{\link[MVNBayesian:MVN_FConditional]{MVN_FConditional}}
}
\examples{
library(mvtnorm)
library(plyr)

# dataset1 has three parameters: fac1, fac2 and fac3:
head(dataset1)

# Get posteriori parameters of dataset1 using prior of c(80,16,3):
BPos <- MVN_BayesianPosteriori(dataset1, pri_mean=c(80,16,3))

# If we want to know when fac1=78, how fac2 responses to fac3, run:
BPos_MCMC <- MVN_MCMC(BPos, steps=8000, pars=c(1), values=c(78), tol=0.3)
MCMC <- BPos_MCMC$MCMCdata
head(MCMC)
\donttest{
# Visualization using plot3d() if necessary:
library(rgl)
plot3d(MCMC[,1], MCMC[,2], z=MCMC[,3], col=MCMC[,5]+1, size=2)

# Visualization: 2d scatter plot
MCMC_2d <- BPos_MCMC$Accept
head(MCMC_2d)
plot(MCMC_2d[,3], MCMC_2d[,2], pch=20, col="red", xlab = "fac3", ylab = "fac2")

# Compared to the following scatter plot when fac1 is not fixed:
plot(BPos_MCMC$MCMCdata[,3], BPos_MCMC$MCMCdata[,2], pch=20, col="red", xlab = "fac3",
ylab = "fac2")
}
}
\keyword{MVN distribution}
\keyword{Gibbs sampling}
\keyword{MCMC}
