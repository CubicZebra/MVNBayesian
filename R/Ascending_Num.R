Ascending_Num <-
function(data){
  coun <- count(data)
  sor <- rev(sort(coun$freq))
  for (i in 1:length(sor)){
    d <- as.numeric(coun[which(coun$freq == sor[i]),]$x)
    data <- replace(data, data == d, (-i))
  }
  data <- data*(-1)
  return(data)
}
