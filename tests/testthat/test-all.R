ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#libraries
packages <- c("mvtnorm","stats","plyr","Rfast","rgl")
ipak(packages)

# Test for MVN framework:
# step.1
head(dataset1)
# step.2
BP <- MVN_BayesianPosteriori(dataset1)
# step.3
BP_Gibbs <- MVN_GibbsSampler(5000, BP)
# step.4
result_MVN <- MVN_MCMC(BP, 5000, c(1), c(76.53))

# Test for MVN_BayesianIteratior:
result_Iter <- MVN_BayesianIterator(dataset1,c(80,16,3))

# Test for MixMVN framework:
# step.1
head(dataset2)
dataset2_test <- dataset2[,1:4]
# step.2
MixBP <- MixMVN_BayesianPosteriori(dataset2_test,3)
# step.3
MixBP_Gibbs <- MixMVN_GibbsSampler(8000, MixBP, random_method = "Fast")
# step.4
result_MixMVN <- MixMVN_MCMC(MixBP, 8000, c(1), c(1))

rm(list = ls())
