MVN_FConditional <-
function(data, variable, z){
  var_modi <- MatrixAlternative(data$var, variable)
  var_intersect <- var_modi[1,-1]
  var_complementary <- var_modi[-1,-1]
  vec_dif <- z - data$mean
  vec_dif_copy <- vec_dif
  vec_dif_copy[variable] <- vec_dif[1]
  vec_dif_copy <- vec_dif_copy[-1]
  temp <- crossprod(var_intersect, solve(var_complementary))
  mean <- data$mean[variable] + tcrossprod(temp, vec_dif_copy)
  var <- var_modi[1,1] - tcrossprod(temp, as.vector(t(var_intersect)))
  results = list("mean" = mean, "var"=var)
  return(results)
}
