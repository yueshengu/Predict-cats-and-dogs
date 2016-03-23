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

img_dir <- 'C:/Users/ygu/Desktop/columbia/images'
# img_feature_dir <- 'C:/Users/DELL/Documents/R/cycle3cvd-team9/data/feature/'
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


# baseline_feature_time<-system.time(baseline_features<-feature('C:/Users/ygu/Desktop/columbia/images/', 
#                                                               'C:/Users/ygu/Desktop/columbia/'))

# feature_eval<-baseline_features
# save(feature_eval,file="C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/output/feature_eval.RData")

eval_feature_time<-system.time(feature_eval<-
                                 feature('C:/Users/ygu/Desktop/columbia/validate/',
                                         'C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/output/'))

feature_eval[is.na(feature_eval)]<-0

save(feature_eval,file='C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/output/feature_eval.RData')


a<-list.files('C:/Users/ygu/Desktop/columbia/validate/')

load('C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/output/feature_eval.RData')
baseline_features <- feature_eval
baseline_train_features <- baseline_features[train_index,]
baseline_test_features <- baseline_features[-train_index,]

#                Train Models
#############################################
source("C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/lib/train.R")
# baseline on new+old features
train_timeOld<-system.time(modelOld<-train(baseline_train_features[,1:800],train_label)) #17 min
save(modelOld,file="C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/modelOld.RData")

train_timeNew<-system.time(modelNew<-train(baseline_train_features,train_label)) #28min
save(modelNew,file="C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/modelNew.RData")


#                Vallidation on train data
#############################################
source("C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/lib/test.R")

modelOld$SVM #32%

trainFeaturesTmp<-data.frame(baseline_train_features)
colnames(trainFeaturesTmp)<-paste0('x',1:ncol(trainFeaturesTmp))
adv_resultsOldTrain=
  table(pred=round(predict(modelOld$GBM,data.frame(trainFeaturesTmp[,1:800]),n.trees=modelOld$GBMTree,
                           type='response'),0),
        true = train_label)
adv_error_rateOldTrain= (adv_resultsOldTrain[2] + adv_resultsOldTrain[3]) / sum(adv_resultsOldTrain) #15%



modelNew$SVM #46%

adv_resultsNewTrain=
  table(pred=round(predict(modelNew$GBM,data.frame(trainFeaturesTmp),n.trees=modelNew$GBMTree,
                           type='response'),0),
        true = train_label)
adv_error_rateNewTrain= (adv_resultsNewTrain[2] + adv_resultsNewTrain[3]) / sum(adv_resultsNewTrain) #16%




#                Vallidation on test data
#############################################
source("C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/lib/test.R")

predict_timeOld<-system.time(predictionsOld<-test(modelOld,baseline_test_features[,1:800])) #3sec
baseline_resultsOld = table(pred = predictionsOld$baseline, true = test_label)
baseline_error_rateOld = (baseline_resultsOld[2] + baseline_resultsOld[3]) / sum(baseline_resultsOld) #33%

adv_resultsOld = table(pred = predictionsOld$adv, true = test_label)
adv_error_rateOld = (adv_resultsOld[2] + adv_resultsOld[3]) / sum(adv_resultsOld) #29%



predict_timeNew<-system.time(predictionsNew<-test(modelNew,baseline_test_features)) #4sec
baseline_resultsNew = table(pred = predictionsNew$baseline, true = test_label)
baseline_error_rateNew = (baseline_resultsNew[2] + baseline_resultsNew[3]) / sum(baseline_resultsNew) #46%

adv_resultsNew = table(pred = predictionsNew$adv, true = test_label)
adv_error_rateNew = (adv_resultsNew[2] + adv_resultsNew[3]) / sum(adv_resultsNew) #29%



predict_timeEval<-system.time(predictionsEval<-test(modelNew,feature_eval)) #4sec
write.csv(predictionsEval[[1]],file="C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/baseline.csv")
write.csv(predictionsEval[[2]],file="C:/Users/ygu/Desktop/columbia/cycle3cvd-team9/adv.csv")

#              Summarize Baseline
#############################################
cat("Time for constructing the baseline training features = ", baseline_train_feature_time[1], "s \n")
cat("Time for constructing the baseline testing features = ", baseline_test_feature_time[1], "s \n")
cat("Time for training the baseline linear svm with color histograms = ", baseline_train_time[1], "s \n")
cat("Time for making baseline predictions = ", baseline_predict_time[1], "s \n")


cat("Time for constructing training features=", tm_feature_train[1], "s \n")
cat("Time for constructing testing features=", tm_feature_test[1], "s \n")
cat("Time for training model=", tm_train[1], "s \n")
cat("Time for making prediction=", tm_test[1], "s \n")

