\name{MVN_FConditional}
\alias{MVN_FConditional}
\title{
  MVN_FConditional
}
\description{
  Given known mean vector and covariance matrix of a specific multivariate normal distribution, export mean and variance of marginal distribution (normal) in specified dimension when other dimensions have been determined by the 3rd parameter z.
}
\usage{
# Bayesian posterior as input data:
# data <- MVN_BayesianPosterior(dataset1, c(80,16,3))

# inquire parameters of full-conditional distribution based on Bayesian posterior:
MVN_FConditional(data, variable, z)
}
\arguments{
  \item{data}{
     a list containing all parameters of a multivariate normal distribution: mean vector is \code{data$mean} and covariance matrix is \code{data$var}.
}
  \item{variable}{
     1d numeric to specify the dimension we concerned in conditions of the rest dimensions were determined.
}
  \item{z}{
     an nd-vector to assign conditions (n = dimensions of the multivariate normal distribution).
}
}
\details{
  Suppose there is a nd-multivariate normal distribution, while any n-1 dimensions are fixed, it will degenerate to an 1d-normal distribution in the rest dimension.
}
\value{
return a list which contains parameters of full-conditional distributions of MVN in specified dimension:
\item{mean}{a numberic mean of full-conditional distribution of MVN}
\item{var}{a numberic variance of full-conditional distribution of MVN}
}
\author{
  ZHANG Chen
}
\seealso{
 MVN_BayesianPosterior, MatrixAlternative
}
\examples{
head(dataset1)
BPos <- MVN_BayesianPosterior(dataset1, c(80,16,3))
BPos # Bayesian Posterior
result <- MVN_FConditional(BPos, variable = 1, z=c(75, 13, 4))
result$mean
# class(result$mean)
result$var
# class(result$var)

##  compare the following results:
# MVN_FConditional(BPos, variable = 2, z=c(75, 13, 4))
# MVN_FConditional(BPos, variable = 2, z=c(75, 88, 4))
# MVN_FConditional(BPos, variable = 1, z=c(75, 88, 4))
}
\keyword{ full conditional distribution }
\keyword{ multivariate normal distribution }
