MatrixAlternative <-
function(data, sub, rep=1){
  if(sub == rep){
    return(data)
  }
  data_copy <- data
  data_copy[rep,] <- data[sub,]
  data_copy[sub,] <- data[rep,]
  data_copy[,rep] <- data[,sub]
  data_copy[,sub] <- data[,rep]
  data_copy[rep,rep] <- data[sub,sub]
  data_copy[sub,sub] <- data[rep,rep]
  data_copy[rep,sub] <- data[sub,rep]
  data_copy[sub,rep] <- data[rep,sub]
  colnames(data_copy)[rep] <- colnames(data)[sub]
  colnames(data_copy)[sub] <- colnames(data)[rep]
  rownames(data_copy)[rep] <- rownames(data)[sub]
  rownames(data_copy)[sub] <- rownames(data)[rep]
  return(data_copy)
}
