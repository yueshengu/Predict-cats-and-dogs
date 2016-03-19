test <- function(fit_train, dat_test){
  
  library("kernlab")
  
  svm_predicted <- predict(svm_model, baseline_test_features, type = "response", coupler = "minpair")
  return(svm_predicted)
  
}