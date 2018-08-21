MVN_MCMC <-
function(data, steps, pars, values, tol=0.3, ...){
  MCMC <- MVN_GibbsSampler(steps, data, ...)
  samples_num <- length(MCMC[,1])
  if(length(pars) != length(values) | length(pars) > length(data$mean)){
    stop("Error assignment: please confirm that pars and values have the identical and correct dimension  (be no more than that of input data). ")
  }
  else{
    idx <- matrix(NA, samples_num, 2)
    for (i in 1:samples_num){
      distance <- MCMC[i,pars]-values
      idx[i,1] <- norm(as.matrix(distance), type = "2")
      idx[i,2] <- (idx[i,1] <= tol)
    }
    MCMC <- cbind(MCMC, idx)
  }
  p <- length(MCMC[1,])
  colnames(MCMC)[p-1] <- "Euclidean.Distance"
  colnames(MCMC)[p] <- "Accept"
  AcceptRate <- as.numeric(count(MCMC[,p])[2,2]/samples_num)
  Accept <- MCMC[which(MCMC[,p] == 1),]
  Reject <- MCMC[which(MCMC[,p] == 0),]
  results <- list("AcceptRate"=AcceptRate, "MCMCdata"=MCMC, "Accept"=Accept, "Reject"=Reject)
  return(results)
}
