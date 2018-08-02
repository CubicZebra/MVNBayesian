MixMVN_GibbsSampler <-
function(n, data, random_method=c("Gibbs", "Fast"), reject_rate=0, ...){
  d <- length(data)/3
  p <- c(1:d)
  for (i in 1:d){
    p[i] <- data[i,1][[1]]
  }
  obs <- rmultinom(1, size = n, prob = p)
  X <- matrix(0,1,length(data[1,2][[1]])+1)
  for (i in 1:d){
    par <- list("mean"=data[i,2][[1]], "var"=data[i,3][[1]])
    if(random_method == "Gibbs"){
      X1 <- MVN_GibbsSampler(obs[i,1], par, reject_rate = reject_rate, ...)
    }
    else if(random_method == "Fast"){
      X1 <- rmvnorm(obs[i,1], par$mean, par$var)
      colnames(X1) <- rownames(data[1,2][[1]])
    }
    else{
      stop("Error: require parameter of random_method (\"Gibbs\" or \"Fast\").")
    }
    X1 <- cbind(X1, matrix(i,length(X1[,1]),1))
    X <- rbind(X, X1)
  }
  X <- X[-1,]
  colnames(X)[length(data[1,2][[1]])+1] <- "Cluster"
  return(X)
}
