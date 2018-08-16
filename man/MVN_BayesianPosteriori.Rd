\name{MVN_BayesianPosteriori}
\alias{MVN_BayesianPosteriori}
\title{
  Calculate Bayesian posteriori MVN distribution
}
\description{
  The function to export the mean vector and covariance matrix of Bayesian posteriori MVN distribution in the basis of given priori information (priori MVN) and observation data (a design matrix containing all variables).
}
\usage{
# Given the data as design matrix, priori mean vector and priori covariance
# matrix, this function will export a list which contains mean ($mean) and
# covariance ($var) of Bayesian posteriori multivariate normal distribution.

MVN_BayesianPosteriori(data, pri_mean, pri_var)
}
\arguments{
  \item{data}{
     A data.frame or matrix-like data: obervations should be arrayed in rows while variables should be arrayed in columns.
}
  \item{pri_mean}{
     A numeric vector to assign priori mean for MVN. Default value applies \code{\link[base:colSums]{colMeans()}} to data.
}
  \item{pri_var}{
     A matrix-like parameter to assign priori covariance matrix. Default value uses unit matrix.
}
}
\note{
  It is strongly recommanded that users should have some prior knowledge of ill-conditioned system before using this function. Simply, ill-conditioned system, or singular matrix, is caused by a) insufficient data or b) almostly linear dependency of two certain parameters, which two can result in a excessively small eigenvalue then cause a ill-conditioned (singular) system. Therefore users must diagnose their data firstly to confirm the fact that the it contains enough observations, and the degree of freedom is strictly equal to the number of parameters as well. Additionally, for the argument \code{pri_var}, a real symmetric matrix is desired by definition.
}
\value{
return a double level list containing:
\item{mean}{mean vector of Bayesian posteriori MVN distribution}
\item{var}{covariance of Bayesian posteriori MVN distribution}
}
\examples{
# Demo using dataset1:
head(dataset1)
BPos <- MVN_BayesianPosteriori(dataset1, c(80,16,3))
BPos$mean
BPos$var

# Singular system caused by insufficient data
eigen(var(dataset1[1:3,]))$values
rcond(var(dataset1[1:3,]))
eigen(var(dataset1[1:6,]))$values
rcond(var(dataset1[1:6,]))

# Singular system caused by improper degree of freedom
K <- cbind(dataset1, dataset1[,3]*(-2)+3)
eigen(var(K[,2:4]))$values
rcond(var(K[,2:4]))
}
\keyword{Bayesian posteriori}
\keyword{MVN distribution}
