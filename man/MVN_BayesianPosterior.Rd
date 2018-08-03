\name{MVN_BayesianPosterior}
\alias{MVN_BayesianPosterior}
\title{
  MVN_BayesianPosterior
}
\description{
  Given a design matrix (data) and priori information, export the mean vector and covariance matrix of Bayesian posterior multivariate normal distribution.
}
\usage{
# Given the data as design matrix, priori mean vector and priori covariance
# matrix, this function will export a list which contains mean ($mean) and
# covariance ($var) of Bayesian posterior multivariate normal distribution.

MVN_BayesianPosterior(data, pri_mean, pri_var)

# defualt pri_mean uses colMenas()
# defualt pri_var uses unit matrix
}
\arguments{
  \item{data}{
     Design matrix: data.frame or matrix-like data, \code{dim(data)[1]} should be the number of observations and \code{dim(data)[2]} should be the number of variables or parameters.
}
  \item{pri_mean}{
     priori mean: necessary vector which should be of the identical dimensions of data (\code{length(pri_mean) == dim(data)[2]}). Users can set according to their own prior knowledge.
}
  \item{pri_var}{
     prior covariance matrix: a real symmetric matrix by definition; the default value is an unit matrix with the same dimension of priori mean vector.
}
}
\details{
  Although this function is very simple, the observation data should be diagnosed firstly. it is strongly recommanded that researchers and developers should have some prior knowledge of ill-conditioned system before using this function. Simply, ill-conditioned system, or singular matrix, is caused by a) insufficient data or b) almostly linear dependency of two certain parameters, which two can result in a too small eigenvalue then cause a ill-conditioned (singular) system. Therefore users should make sure the data contains enough observations and the degree of freedom is strictly equal to the number of parameters.
}
\value{
return a list of:
\item{mean}{mean vector of Bayesian posterior}
\item{var}{covariance of Bayesian posterior}
}
\author{
  ZHANG Chen
}
\examples{
# Demo using dataset1:
head(dataset1)
BPos <- MVN_BayesianPosterior(dataset1, c(80,16,3))
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
\keyword{ Bayesian posterior distribution }
\keyword{ multivariate normal distribution }
