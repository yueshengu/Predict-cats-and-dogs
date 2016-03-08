#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Author: Yuting Ma
### Project 3
### ADS Spring 2016

#feature <- function(img_dir, img_name, data_name=NULL){
feature <- function(img_dir, feature_dir){  
  ### Construct process features for training/testing images
  ### Sample simple feature: Extract raw pixel values os features
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  #library("EBImage")
  
  n_files <- length(list.files(img_dir))
  feature_matrix <- matrix(nrow=0, ncol=800)
  
    for(i in 1:n_files){
    #  tryCatch(
    #    {
    mat <- imageData(readImage(paste0(img_dir, list.files(img_dir)[i])))
    #    }, 
    #    error=function(cond) {
    #          message(paste("You were at image ", file_names[i]))
    #          #message("Here's the original error message:")
    #          #message(cond)
    #          # Choose a return value in case of error
    #          return(NA)
    #    },
    #    warning=function(cond) {
    #          message(paste("You were at image ", file_names[i]))
    #          #message("Here's the original error message:")
    #          #message(cond)
    #          # Choose a return value in case of error
    #          return(NA)
    #    }
    #  )
    #}
      nR <- 10
      nG <- 8
      nB <- 10
      # Caution: the bins should be consistent across all images!
      rBin <- seq(0, 1, length.out=nR)
      gBin <- seq(0, 1, length.out=nG)
      bBin <- seq(0, 1, length.out=nB)
      freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), 
                                    factor(findInterval(mat[,,2], gBin), levels=1:nG), 
                                    factor(findInterval(mat[,,3], bBin), levels=1:nB)))
      rgb_feature <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
      feature_matrix <- rbind(feature_matrix, as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)))
      #save(rgb_feature, file=paste0(feature_dir, str_sub(list.files(img_dir)[i], 1, -5), ".RData"))
      saveRDS(as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)), file=paste0(feature_dir, str_sub(list.files(img_dir)[i], 1, -5), ".rds"))
    }
    return(feature_matrix)
  ### determine img dimensions
  #img0 <-  readImage(paste0(img_dir, img_name, "_", 1, ".jpg"))
  #mat1 <- as.matrix(img0)
  #n_r <- nrow(img0)
  #n_c <- ncol(img0)
  
  ### store vectorized pixel values of images
  #dat <- array(dim=c(n_files, n_r*n_c)) 
  #for(i in 1:n_files){
  #  img <- readImage(paste0(img_dir, img_name, "_", i, ".jpg"))
  #  dat[i,] <- as.vector(img)
  #}
  
  ### output constructed features
  #if(!is.null(data_name)){
  #  save(dat, file=paste0("./output/feature_", data_name, ".RData"))
  #}
  #return(dat)
}
