test <- function(fit_train, dat_test){
  
  svm_predicted <- predict(fit_train, dat_test, type = "response", coupler = "minpair")
  return(svm_predicted)
  
}