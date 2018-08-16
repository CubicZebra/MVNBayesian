ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#调用
packages <- c("MVNBayesian")
ipak(packages)

head(dataset1)

colMeans(dataset1)

Bayesian_ES <- MVN_BayesianPosteriori(dataset1, c(80, 16, 3))
Bayesian_mean <- as.vector(t(Bayesian_ES$mean))
Bayesian_mean

for (i in 1:5){
  a <- MVN_BayesianPosteriori(dataset1[1:(i+20),],c(80,16,3))
  b <- as.vector(t(a$mean))
  print(b)
}

Bayesian_m1 <- MVN_FConditional(Bayesian_ES, 1, c(80,13,4))
Bayesian_m1

fac1_m <- rnorm(1000, Bayesian_m1$mean, Bayesian_m1$var)
hist(fac1_m,freq = F, main = "Histogram of fac1 | fac2=13, fac3=4", xlab = "fac1", ylab = "frequency (*100.%)")

fac1_fac2 <- MVN_BayesianPosteriori(dataset1[,1:2],c(80,16))
fac1_fac2
fac2_fac3 <- MVN_BayesianPosteriori(dataset1[,2:3],c(16,3))
fac2_fac3
fac1_fac3 <- MVN_BayesianPosteriori(dataset1[,c(1,3)],c(80,3))
fac1_fac3
Bayesian_ES

BP_Gibbs <- MVN_GibbsSampler(4000, Bayesian_ES)
colMeans(BP_Gibbs)
var(BP_Gibbs)

fac2_fac3

colMeans(BP_Gibbs[,2:3])
var(BP_Gibbs[,2:3])

BP_MCMC <- MVN_MCMC(Bayesian_ES, steps=8000, pars=c(1), values=c(78), tol=0.3, burn=0)
MCMC <- BP_MCMC$MCMCdata
head(MCMC)
dim(MCMC)
print(BP_MCMC$AcceptRate)

Marginal_D <- MCMC[,2:3]
plot(Marginal_D[,1], Marginal_D[,2], pch=20, col="red", xlab = "fac2", ylab = "fac3", main = "p(fac2, fac3)")

Conditional_D <- BP_MCMC$Accept[,2:3]
plot(Conditional_D[,1], Conditional_D[,2], pch=20, col="red", xlab = "fac2", ylab = "fac3", main = "p(fac2, fac3| fac1=78)")
