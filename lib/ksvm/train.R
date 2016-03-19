train <- function(features, label_train){
  
  library("kernlab")
  svm_model <- ksvm(baseline_train_features, y = train_label)
  return(svm_model)
  
}