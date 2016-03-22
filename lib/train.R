
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
  
  boostingTuning<-data.frame(depth=rep(1:3,each=2),nodes=rep(c(10,50),3),tree=rep(NA,6),
                             cvError=rep(NA,6))
  gbmTrain<-data.frame(cbind(as.numeric(as.character(label_train)),dat_train))
  colnames(gbmTrain)<-c('y',paste0('x',1:ncol(dat_train)))
  
  
  for(i in 1:nrow(boostingTuning)){
    cat(i,'of 6\n')
    
    gbmTmp<-gbm(y~.,
                distribution = "bernoulli",
                data = gbmTrain,
                n.trees = 500,
                interaction.depth = boostingTuning$depth[i],
                n.minobsinnode = boostingTuning$nodes[i],
                shrinkage = 0.1,
                bag.fraction = 1,
                train.fraction = .7,
                cv.folds=5,
                keep.data = TRUE,
                verbose =F,
                class.stratify.cv=NULL,
                n.cores = NULL)
    boostingTuning$tree[i]<-gbm.perf(gbmTmp,method="cv",plot=F)
    boostingTuning$cvError[i]<-gbmTmp$cv.error[boostingTuning$tree[i]]
    
  }
  
  boostingTuning<-boostingTuning[order(boostingTuning$cvError),]
  
  gbm_model<-gbm(y~.,
                 distribution = "bernoulli",
                 data = gbmTrain,
                 n.trees = boostingTuning$tree[1],
                 interaction.depth = boostingTuning$depth[1],
                 n.minobsinnode = boostingTuning$nodes[1],
                 shrinkage = 0.1,
                 bag.fraction = 1,
                 train.fraction = 1,
                 cv.folds=0,
                 keep.data = TRUE,
                 verbose =F,
                 class.stratify.cv=NULL,
                 n.cores = NULL)
  
  return(list(SVM=svm_model,GBM=gbm_model,GBMTree=boostingTuning$tree[1]))
  
}