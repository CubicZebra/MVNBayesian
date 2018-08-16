\name{MVNBayesian-package}
\alias{MVNBayesian-package}
\alias{MVNBayesian}
\docType{package}
\title{
\packageTitle{MVNBayesian}
}
\description{
\packageDescription{MVNBayesian}
}
\details{
This package is aimed to build a easy approach for MVN (mixture) distribution in Bayesian analysis framework. Bayesian posteriori MVN (mixture) distribution can be calculated in conditions of given priori MVN (mixture) informations. The conjugated property of MVN distribution makes it effective in parameter estimation using Bayesian iterator. Joint and marginal probability densities of a certain MVN (mixture) can be achieved through random vector generator, using Gibbs sampling. Conditional probability densities from a certain MVN (mixture) can be simulated using MCMC method.
%%The DESCRIPTION file:
%%\packageDESCRIPTION{MVNBayesian}
%%\packageIndices{MVNBayesian}
%%\strong{*Common steps for Bayesian analysis:}\cr
%%\cr
%%1) Preprocess your data into a design matrix (a data.frame or a matrix-like data) and diagnose it;\cr
%%2) Use (Mix)MVN_BayesianPosteriori export concerned parameters;\cr
%%3) (Mix)MVN_GibbsSampler to generate random vectors based on parameters exported from previous step, determine the range of values for each parameter (proper visualization is recommanded);\cr
%%4) Use (Mix)MVN_MCMC to customize conditions for further analysis.\cr
}
\author{
\packageAuthor{MVNBayesian}

Maintainer: \packageMaintainer{MVNBayesian}
}
\references{
"Statistical Inference" by George Casella. Roger L. Berger;\cr
"Statistical Decision Theory and Bayesian Analysis" by James O. Berger;\cr
"Matrix Computation" by Gee H. Golub. Charles F. Van Loan;\cr
"Bayesian Statistics" by WEI Laisheng;\cr
"Machine Learning" by NAKAGAWA Hiroshi.
}
\keyword{ package }
\seealso{
\code{\link[stats:stats-package]{stats}}, \code{\link[mvtnorm:Mvnorm]{mvtnorm}}
}
\examples{
library(Rfast)
library(mvtnorm)
library(plyr)

head(dataset1)

BP <- MVN_BayesianPosteriori(dataset1)
BP

BP_Gibbs <- MVN_GibbsSampler(5000, BP)
colMeans(BP_Gibbs)
colrange(BP_Gibbs)

result <- MVN_MCMC(BP, 5000, c(1), c(77.03))
result$Accept
}
\keyword{Bayesian posteriori}
\keyword{MVN distribution}
\keyword{MVN mixture distribution}
\keyword{Gibbs sampling}
\keyword{MCMC}
