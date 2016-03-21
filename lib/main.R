#############################################
### Main execution script for experiments ###
#############################################

### Author: Team 9
### Using Yuting Ma's code as starter
### Project 3
### ADS Spring 2016

library(EBImage)
library(stringr)

# change var names in feature function

#       Set up variables and files
#############################################

#setwd("C:/Users/DELL/Documents/R/cycle3cvd-team9/")
img_dir <- 'C:/Users/ygu/Desktop/columbia/images'
#img_feature_dir <- 'C:/Users/DELL/Documents/R/cycle3cvd-team9/data/feature/'
file_names <- dir(img_dir)
set.seed(8)
train_index <- sample(1:length(file_names), round(length(file_names)*.7, 0))

#               Class Labels
#############################################
breed_name <- rep(NA, length(file_names))

for (i in 1:length(file_names)){
  tt <- unlist(strsplit(file_names[i], "_"))
  tt <- tt[-length(tt)]
  breed_name[i] = paste(tt, collapse = "_", sep = "")
}

cat_breed <- c("Abyssinian", "Bengal", "Birman", "Bombay", "British_Shorthair", "Egyptian_Mau",
               "Maine_Coon", "Persian", "Ragdoll", "Russian_Blue", "Siamese", "Sphynx")
label <- as.numeric(breed_name %in% cat_breed)
train_label <- as.factor(label[train_index])
test_label <- as.factor(label[-train_index])

#            Construct Features
#############################################
source("lib/feature.R")


baseline_feature_time<-system.time(baseline_features<-feature('C:/Users/ygu/Desktop/columbia/images/', 
                                                              'C:/Users/ygu/Desktop/columbia/'))
#816
#baseline_features <- readRDS('C:/Users/DELL/Documents/R/cycle3cvd-team9/data/features.rds')
baseline_train_features <- baseline_features[train_index,]
baseline_test_features <- baseline_features[-train_index,]

#                Train Baseline
#############################################
source("C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/lib/ksvm/train.R")
# baseline on new+old features
baseline_train_timeNew<-system.time(baseline_modelNew<-train(baseline_train_features,train_label)) #10min
baseline_train_timeOld<-system.time(baseline_modelOld<-train(baseline_train_features[,1:800],train_label)) 
#19 sec

#                Test Baseline
#############################################
source("C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/lib/ksvm/test.R")
baseline_predict_timeNew<-system.time(baseline_predictionsNew<-test(baseline_modelNew,
                                                                    baseline_test_features))
#4sec
baseline_resultsNew = table(pred = baseline_predictionsNew, true = test_label)
baseline_error_rateNew = (baseline_resultsNew[2] + baseline_resultsNew[3]) / sum(baseline_resultsNew) #46%

baseline_predict_timeOld<-system.time(baseline_predictionsOld<-test(baseline_modelOld,
                                                                    baseline_test_features[,1:800]))
#4sec
baseline_resultsOld = table(pred = baseline_predictionsOld, true = test_label)
baseline_error_rateOld = (baseline_resultsOld[2] + baseline_resultsOld[3]) / sum(baseline_resultsOld) #32%


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

