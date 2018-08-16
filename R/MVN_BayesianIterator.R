MVN_BayesianIterator <-
function(data, pri_mean=colMeans(data), Gibbs_nums=5000, pseudo_nums=dim(data)[1], threshold=1e-4, iteration=100, ...){
  data_BayesP <- MVN_BayesianPosteriori(data, pri_mean)
  for (i in 1:iteration){
    data_Gibbs <- MVN_GibbsSampler(Gibbs_nums, data_BayesP, ...)
    pseudo_data <- tail(data_Gibbs, pseudo_nums)
    data_BayesP <- MVN_BayesianPosteriori(pseudo_data, colMeans(pseudo_data), var(pseudo_data))
    p <- as.numeric(data_BayesP$mean)-as.vector(colMeans(pseudo_data))
    if(norm(as.matrix(p), type = "2") < threshold){
      print(paste("Finish at the", i, "step(s) in iteration.", sep = " "))
      break
    }
  }
  return(data_BayesP)
}
