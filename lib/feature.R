#############################################################
### Construct visual features for training/testing images ###
#############################################################

if(!require(EBImage)){
  source("http://bioconductor.org/biocLite.R")
  biocLite("EBImage")
  library(EBImage)
}


feature <- function(img_dir, feature_dir){  
  
  n_files <- length(list.files(img_dir))
  
  numbers<-as.numeric(gsub('.*_|.jpg','',list.files(img_dir)))
  
  fileName<-data.frame(number=numbers,name=list.files(img_dir))
  fileName<-fileName[order(fileName$number),]
  #browser()
  feature_eval<- matrix(nrow=0, ncol=888)
  
  x = makeBrush(9, shape="gaussian", sigma=5)
  x <- x / sum(x)
  
  for(i in 1:n_files){
    
    cat(i,'\n')
    if(i==1106){
      feature_eval<-rbind(feature_eval, rep(0,888))
    }else{
      img<-readImage(paste0(img_dir, fileName$name[i]))
      mat <- imageData(img)
      nR <- 10
      nG <- 8
      nB <- 10
      
      rBin <- seq(0, 1, length.out=nR)
      gBin <- seq(0, 1, length.out=nG)
      bBin <- seq(0, 1, length.out=nB)
      freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), 
                                      factor(findInterval(mat[,,2], gBin), levels=1:nG), 
                                      factor(findInterval(mat[,,3], bBin), levels=1:nB)))
      rgb_feature <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
      
      # identify 8 largest objects (in terms of area) and record infos extracted using 
      # computeFeatures.shape and computeFeatures.moment
      if(i==403){
        feature_eval <- rbind(feature_eval, c(rgb_feature,rep(0,88)))
      }else{
        objects<-bwlabel(thresh(filter2(channel(img,'gray'), x), 10, 10, 0.01))
        objectsInfo<-cbind(computeFeatures.shape(objects),computeFeatures.moment(objects))
        
        areaObject10<-rev(sort(objectsInfo[,1]))[8]
        top10Objects<-objectsInfo[objectsInfo[,1]>=areaObject10,]
        
        top10Objects[,1]<-top10Objects[,1]/nrow(img)/ncol(img)
        top10Objects[,2:8]<-top10Objects[,2:8]/(nrow(img)+ncol(img))
        
        object_feature<-c(top10Objects)
        
        feature_eval <- rbind(feature_eval, c(rgb_feature,object_feature))
        }
      
      }
    
  }

  save(feature_eval,file=paste0(feature_dir,"feature_eval.RData"))
  return(feature_eval)
}
