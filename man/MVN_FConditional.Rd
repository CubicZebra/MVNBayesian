\name{MVN_FConditional}
\alias{MVN_FConditional}
\title{
  Calculate full conditional normal ditribution of MVN
}
\description{
  Function to export parameters of full conditional normal distribution in basis of given MVN distribution, the undecided dimension, as well as all values in the rest dimensions.
}
\usage{
# Bayesian posteriori as input data:
# data <- MVN_BayesianPosteriori(dataset1, c(80,16,3))

# inquire parameters of full-conditional distribution based on Bayesian posteriori:
MVN_FConditional(data, variable, z)
}
\arguments{
  \item{data}{
     A double level list containing all parameters of MVN distribution: mean vector (\code{data$mean}) and covariance matrix (\code{data$var}).
}
  \item{variable}{
     A integer to specify the undecided dimension.
}
  \item{z}{
     A nd-vector to assign conditions (n = dimensions of given MVN distribution). It should be noted that the value in dimension specified by \code{variable} doesn't participate in the calculation.
}
}
\details{
  It can be proved that any full conditional distribution from a given MVN will degenerate to an 1d-normal distribution.
}
\value{
return a double level list containing the following parameters of full conditional normal distributions of given MVN in specified dimension:
\item{mean}{a numberic mean of a normal distribution}
\item{var}{a numberic variance of a normal distribution}
}
\seealso{
 \code{\link[MVNBayesian:MVN_BayesianPosteriori]{MVN_BayesianPosteriori}}, \code{\link[MVNBayesian:MatrixAlternative]{MatrixAlternative}}
}
\examples{
head(dataset1)
BPos <- MVN_BayesianPosteriori(dataset1, c(80,16,3))
BPos # Bayesian Posteriori
result <- MVN_FConditional(BPos, variable = 1, z=c(75, 13, 4))
result$mean
class(result$mean)
result$var
class(result$var)

# compare the following results:
MVN_FConditional(BPos, variable = 2, z=c(75, 13, 4))
MVN_FConditional(BPos, variable = 2, z=c(75, 88, 4))
MVN_FConditional(BPos, variable = 1, z=c(75, 88, 4))
}
\keyword{Full conditional distribution}
\keyword{MVN distribution}
