\name{MVN_BayesianIterator}
\alias{MVN_BayesianIterator}
\title{
  Parameter estimation using Bayesian iteration
}
\description{
  Function to execute parameter estimation for MVN distribution, under Bayesian analysis framework.
}
\usage{
# Get parameters of Bayesian posteriori MVN:
MVN_BayesianIterator(data, pri_mean=colMeans(data), Gibbs_nums=5000,
pseudo_nums=dim(data)[1], threshold=1e-04, iteration=100, ...)
}
\arguments{
  \item{data}{
     A data.frame or matrix-like data: obervations should be arrayed in rows while variables should be arrayed in columns.
}
  \item{pri_mean}{
     A numeric vector to assign priori mean for MVN. Default value applies \code{\link[base:colSums]{colMeans()}} to data.
}
  \item{Gibbs_nums}{
     A positive integer. The numbers of random vectors to be generated for each iteration step. Defult value is 5000.
}
  \item{pseudo_nums}{
     A positive integer. The argument to determine numbers of generated vectors used for each iteration step. Default value keeps the same scale as input data. Notice that a too small value can result in singular matrix.
}
  \item{threshold}{
     A numeric value to control stoping the iteration loop. Default value used 0.0001. While the Euclidean distance of mean vectors between pseudo-data (the last \code{pseudo_nums} items) and Bayesian posteriori is less than threshold, iteration stops.
}
  \item{iteration}{
     A positive integer. Argument to assign the maximum steps for iteration. Default value is 100 after which the iteration loop will compulsively exit.
}
  \item{\dots}{
     Other arguments to control the process in Gibbs sampling.
}
}
\details{
  Because that MVN distribution possess conjugated property in Bayesian analysis framework, the convergence of Bayesian iterator for MVN distribution can be ensured, accoumpanied with the shrink of 2nd-norm of Bayesian posteriori covariance matrix. But pay attention to the fact that pseudo-data leads to the randomness, the argument \code{pseudo_nums} should be set carefully.
}
\value{
return a double level list containing Bayesian posteriori after iteration process:
\item{mean}{Bayesian posteriori mean vector}
\item{var}{Bayesian posteriori covariance matrix}
}
\note{
  If the parameter values are the only interested thing we concerned, this iterator makes sense. Since it can significantly help us decrease the scale of covariance matrix, to obtain a more reliable estimation for the parameters. However, in more cases, some correlationships of a certain group of pamameters are more valuable, which are usually clued by the covariance matrix.
}
\seealso{
 \code{\link[MVNBayesian:MVN_BayesianPosteriori]{MVN_BayesianPosteriori}}, \code{\link[MVNBayesian:MVN_GibbsSampler]{MVN_GibbsSampler}}, \code{\link[MVNBayesian:MVN_FConditional]{MVN_FConditional}}, \code{\link[MVNBayesian:MatrixAlternative]{MatrixAlternative}}
}
\examples{
library(mvtnorm)

# Bayesian posteriori before iteration using dataset1 as example,
# c(80, 16, 3) as priori mean:
# View 2-norm of covariance matrix of Bayesian posteriori:
BPos_init <- MVN_BayesianPosteriori(dataset1, c(80,16,3))
BPos_init
norm(as.matrix(BPos_init$var), type = "2")

# Bayesian posteriori after iteration using c(80,16,3) as priori
# Using 30 last samples generated by GibbsSampler for each step:
BPos_fina1 <- MVN_BayesianIterator(dataset1, c(80,16,3), 5000, 30)
BPos_fina1
norm(as.matrix(BPos_fina1$var), type = "2")
\donttest{
# Too small pseudo_nums setting can results in singular system, try:
MVN_BayesianIterator(dataset1, pseudo_nums=3)
}
}
\keyword{MVN distribution}
\keyword{Bayesian posteriori}
\keyword{Gibbs sampling}
