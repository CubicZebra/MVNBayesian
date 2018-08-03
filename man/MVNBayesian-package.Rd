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

The DESCRIPTION file:
\packageDESCRIPTION{MVNBayesian}
\packageIndices{MVNBayesian}\cr
\strong{*Common steps for Bayesian analysis:}\cr
\cr
1) Preprocess your data into a design matrix (a data.frame or a matrix-like data) and diagnose it;\cr
2) Use (Mix)MVN_BayesianPosterior export concerned parameters;\cr
3) (Mix)MVN_GibbsSampler to generate random vectors based on parameters exported from previous step, determine the range of values for each parameter (proper visualization is recommanded);\cr
4) Use (Mix)MVN_MCMC to customize conditions for further analysis.\cr
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
\code{\link{stats}}, \code{\link[mvtnorm:Mvnorm]{mvtnorm}}
}
\examples{
library(Rfast)
library(mvtnorm)
library(plyr)

# step.1
head(dataset1)

# step.2
BP <- MVN_BayesianPosterior(dataset1)
BP

# step.3
BP_Gibbs <- MVN_GibbsSampler(5000, BP)
colMeans(BP_Gibbs)
colrange(BP_Gibbs)

# step.4
result <- MVN_MCMC(BP, 5000, c(1), c(77.03))
result$Accept
}
\keyword{ Bayesian posterior distribution }
\keyword{ multivariate normal distribution }
\keyword{ multivariate normal mixture distribution }
\keyword{ Gibbs sampling }
\keyword{ MCMC }
