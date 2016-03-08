#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Yuting Ma
### Project 3
### ADS Spring 2016


#train <- function(dat_train, label_train, par=NULL){
#train <- function(feature_dir, label_train, baseline){
train <- function(features, label_train){ 
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification
  
  ### load libraries
  #library("gbm")
  library(e1071)
  #library(caret)
  #library(kernlab)
  
  #n_files <- length(list.files(feature_dir))
  
  ### Train baseline
    #full_matrix <- cbind(features, label_train)
    svmObj <- e1071::tune.svm(features, label_train, type='C-classification', kernel='linear', cost = c(2^seq(-10, 0, .5)))
    lin_model <- svm(features, label_train, type='C-classification', kernel='linear', cost = svmObj$best.parameters$cost)
    return(lin_model)
  
  ### Train with gradient boosting model
  #if(is.null(par)){
  #  depth <- 3
  #} else {
  #  depth <- par$depth
  #}
  #fit_gbm <- gbm.fit(x=features, y=label_train,
  #                   n.trees=2000,
  #                   distribution="bernoulli",
  #                   interaction.depth=3, 
  #                   bag.fraction = 0.5,
  #                   verbose=FALSE)
  #best_iter <- gbm.perf(fit_gbm, method="OOB")

  #return(list(fit=fit_gbm, iter=best_iter))
}
