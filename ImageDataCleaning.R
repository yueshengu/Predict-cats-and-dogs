source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")

library(EBImage)

img<-readImage("C:/Users/ygu/Desktop/columbia/Abyssinian_1.jpg")
display(img)
display(img[,,1])
display(img[,,2])
display(img[,,3])

fHigh <- matrix(1, nc = 3, nr = 3)
fHigh[2, 2] <- -8
Image.fHigh <- filter2(img, fHigh)
display(Image.fHigh)
display(Image.fHigh[,,1])
display(Image.fHigh[,,2])
display(Image.fHigh[,,3])


moments = computeFeatures.moment(Image.fHigh[,,2])
ftp = computeFeatures.shape(Image.fHigh[,,2])



par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
#par(mar=c(5, 4, 4, 2) + 0.1)
plot.new()
plot.window(c(1,600),c(1,400))
usr<-par("usr")    
rasterImage(Image.fHigh[,,3], usr[1], usr[3], usr[2], usr[4])
#text(.1,.1,'a',col='red',cex=4)
sapply(1:nrow(moments),function(i){
  text((moments[i,1]),(400-moments[i,2]),i,cex=1,col='red')
})
moments2<-moments[moments[,3]>=34,]
sapply(1:nrow(moments2),function(i){
  text((moments2[i,1]),(400-moments2[i,2]),i,cex=1,col='blue')
})
moments3<-moments[moments[,3]<34,]
sapply(1:nrow(moments3),function(i){
  text((moments3[i,1]),(400-moments3[i,2]),i,cex=1,col='green')
})
