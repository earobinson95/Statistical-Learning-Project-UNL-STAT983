# use source(r-code/regression-evaluation.R) to have function in your script file

regEval <- 
  function(y, yhat){
  MSE = mean((y-yhat)^2)
  MAE = mean(abs(y-yhat))
  return(MSE = MSE, MAE = MAE)
}