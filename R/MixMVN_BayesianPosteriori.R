MixMVN_BayesianPosteriori <-
function(data, species=1, idx=kmeans(data, species)$cluster){
  temp_class <- MVN_BayesianPosteriori(data, colMeans(data))
  result <- list("probability"=1, "mean"=temp_class$mean, "var"=temp_class$var)
  idx <- as.numeric(idx)
  idx <- Ascending_Num(idx)
  clusters <- length(count(idx)$x)
  for(i in 1:clusters){
    data_temp <- data[which(idx == i),]
    if(rcond(var(data_temp)) < .Machine$double.eps){
      warning(paste("Caution: No enough data for the No.", i, " cluster.", sep = ""))
    }
    else{
      prob <- count(idx)[i,2]/sum(count(idx)[,2])
      a <- MVN_BayesianPosteriori(data_temp, rnorm(length(data[1,]), colMeans(data_temp)))
      newlist <- list("probability"=prob, "mean"=a$mean, "var"=a$var)
      result <- rbind(result, newlist)
      rownames(result)[i+1] <- as.character(paste("cluster",i,sep = ""))
    }
  }
  result <- result[-1,]
  return(result)
}
