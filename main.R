#############################################
### Main execution script for experiments ###
#############################################

### Author: Team 9
### Using Yuting Ma's code as starter
### Project 3
### ADS Spring 2016

#source("http://bioconductor.org/biocLite.R")
#install.packages("stringr")
#install.packages("caret")
#install.packages("kernlab")
#biocLite("EBImage")
#options("EBImage.display"= "raster") 
library(EBImage)
library(stringr)

#       Set up variables and files
#############################################
setwd("Documents/Spring 2016/DataScience/Project3/cycle3cvd-team9/")
img_train_dir <- "../data/train/"
img_test_dir <- "../data/test/"
img_train_feature_dir <- "../data/features/"
img_test_feature_dir <- "../data/testfeatures/"
file_names <- dir(img_train_dir)
test_file_names <- dir(img_test_dir)


fileNames<-dir('C:/Users/ygu/Desktop/columbia/images')
trainFileNames<-sample(fileNames,round(length(fileNames)*.7,0))
testFileNames<-fileNames[!fileNames%in%trainFileNames]

#               Class Labels
#############################################
train_label <- c()
for(i in 1:length(file_names)){
  if (substring(file_names[i], 1, 1) == toupper(substring(file_names[1], 1, 1))){
    # Cat = 1
    train_label <- c(train_label, 1)
  } else {
    # Dog = 0
    train_label <- c(train_label, 0)
  }
}
train_label <- as.factor(train_label)

test_label <- c()
for(i in 1:length(test_file_names)){
  if (substring(test_file_names[i], 1, 1) == toupper(substring(test_file_names[1], 1, 1))){
    # Cat = 1
    test_label <- c(test_label, 1)
  } else {
    # Dog = 0
    test_label <- c(test_label, 0)
  }
}
test_label <- as.factor(test_label)

#            Construct Features
#############################################
source("lib/feature.R")

# Baseline
baseline_train_feature_time <- system.time(baseline_train_features <- feature(img_train_dir, img_train_feature_dir))
baseline_test_feature_time <- system.time(baseline_test_features <- feature(img_test_dir, img_test_feature_dir))

#tm_feature_train <- system.time(dat_train <- feature(img_train_dir, "img_zip_train"))
#tm_feature_test <- system.time(dat_test <- feature(img_test_dir, "img_zip_test"))
#save(dat_train, file="./output/feature_train.RData")
#save(dat_train, file="./output/feature_test.RData")

#                Train Baseline
#############################################
source("./lib/train.R")
# I use cross-validation within this function to be more efficient
# GETTING WARNINGS
baseline_train_time <- system.time(baseline_model<- train(baseline_train_features, train_label))

#                Test Baseline
#############################################
source("./lib/test.R")
baseline_predict_time <- system.time(baseline_predictions<-test(baseline_model, baseline_test_features))
baseline_results = table(pred = baseline_predictions, true = test_label)
#baseline_rate = (baseline_results[2] + baseline_results[3]) / (baseline_results[1] + baseline_results[4] + baseline_results[2] + baseline_results[3])
#baseline_rate<-baseline_rate *100

#              Summarize Baseline
#############################################
cat("Time for constructing the baseline training features = ", baseline_train_feature_time[1], "s \n")
cat("Time for constructing the baseline testing features = ", baseline_test_feature_time[1], "s \n")
cat("Time for training the baseline linear svm with color histograms = ", baseline_train_time[1], "s \n")
cat("Time for making baseline predictions = ", baseline_predict_time[1], "s \n")


#   DID NOT USE THE FOLLOWING FOR BASELINE
#############################################

### Model selection with cross-validation
# Choosing between different values of interaction depth for GBM
source("./lib/cross_validation.R")
depth_values <- seq(3, 11, 2)
err_cv <- array(dim=c(length(depth_values), 2))
K <- 5  # number of CV folds
for(k in 1:length(depth_values)){
  cat("k=", k, "\n")
  err_cv[k,] <- cv.function(dat_train, label_train, depth_values[k], K)
}
save(err_cv, file="./output/err_cv.RData")

# Visualize CV results
pdf("./fig/cv_results.pdf", width=7, height=5)
plot(depth_values, err_cv[,1], xlab="Interaction Depth", ylab="CV Error",
     main="Cross Validation Error", type="n", ylim=c(0, 0.15))
points(depth_values, err_cv[,1], col="blue", pch=16)
lines(depth_values, err_cv[,1], col="blue")
arrows(depth_values, err_cv[,1]-err_cv[,2],depth_values, err_cv[,1]+err_cv[,2], 
      length=0.1, angle=90, code=3)
dev.off()

# Choose the best parameter value
depth_best <- depth_values[which.min(err_cv[,1])]
par_best <- list(depth=depth_best)

# train the model with the entire training set
tm_train <- system.time(fit_train <- train(dat_train, label_train, par_best))
save(fit_train, file="./output/fit_train.RData")

### Make prediction 
tm_test <- system.time(pred_test <- test(fit_train, dat_test))
save(pred_test, file="./output/pred_test.RData")

### Summarize Running Time
cat("Time for constructing training features=", tm_feature_train[1], "s \n")
cat("Time for constructing testing features=", tm_feature_test[1], "s \n")
cat("Time for training model=", tm_train[1], "s \n")
cat("Time for making prediction=", tm_test[1], "s \n")

