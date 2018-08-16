MVN_BayesianPosteriori <-
function(data, pri_mean=colMeans(data), pri_var=diag(length(data[1,]))){
  if(rcond(var(data)) < .Machine$double.eps){
     warning("Caustion: No enough data for Bayesian Posteriori.")
  }
  else{
    N <- length(data[,1])
    var <- solve(N*solve(var(data)) + solve(pri_var))
    temp <- crossprod(solve(var(data)), colSums(data)) + crossprod(solve(pri_var), pri_mean)
    mean <- var %*% temp
    results <- list("mean"=mean, "var"=var)
    return(results)
  }
}
