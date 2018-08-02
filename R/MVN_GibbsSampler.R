MVN_GibbsSampler <-
function(n, data, initial=rmvnorm(1, data$mean, data$var), reject_rate=0.2, burn=round(reject_rate*n)){
  d <- length(data$mean)
  obs <- matrix(0, n+1, d)
  obs[1,] <- initial
  for(i in 2:(n+1)){
    obs_temp <- obs[i-1,]
    for(j in 1:d){
      FCond_temp <- MVN_FConditional(data, j, obs_temp)
      obs_temp[j] <- as.numeric(FCond_temp$mean) + rnorm(1)*sqrt(as.numeric(FCond_temp$var))
    }
    obs[i,] <- obs_temp
  }
  colnames(obs) <- colnames(data$var)
  return(obs[burn:(n+1),])
}
