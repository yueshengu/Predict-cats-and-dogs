
if(!require(kernlab)){
  install.packages('kernlab')
  library(kernlab)
}
if(!require(gbm)){
  install.packages('gbm')
  library(gbm)
}

train <- function(dat_train, label_train){
  
  svm_model <- ksvm(dat_train, y = label_train,kernel = "vanilladot")
  return(svm_model)
  
}