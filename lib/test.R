test <- function(fit_train, dat_test){
  
  svm_predicted <- predict(fit_train$SVM, dat_test, type = "response", coupler = "minpair")
  
  gbmTest<-data.frame(dat_test)
  colnames(gbmTest)<-paste0('x',1:ncol(dat_test))
  
  gbm_predicted<-round(predict(fit_train$GBM,gbmTest,n.trees=fit_train$GBMTree,type='response'),0)
  
  return(list(baseline=svm_predicted,adv=gbm_predicted))
  
}