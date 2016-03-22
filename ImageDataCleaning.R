source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
biocLite("SIFT.Hsapiens.dbSNP137")

library(EBImage)


img<-readImage("C:/Users/ygu/Desktop/columbia/images/Abyssinian_1.jpg")
display(img,method="raster")
display(img[,,1])
display(img[,,2])
display(img[,,3])


laplacian <- matrix(1, nc=5, nr=5)
laplacian[3,3] = -24
laplacian
y <- filter2(channel(img,'gray'), laplacian)
display(y, method="raster")

pBlur <- gblur(channel(img,'gray'), sigma=1)
pBlur
display(pBlur, method="raster")

p2 <- thresh(pBlur, 10, 10, 0.01)
display(p2, method="raster")

x = makeBrush(9, shape="gaussian", sigma=5)
x <- x / sum(x)

y <- filter2(img, x)
display(y, method="raster")

display(thresh(y, 10, 10, 0.02), method="raster")
display(channel(thresh(y, 10, 10, 0.01),'gray'), method="raster")

a<-bwlabel(thresh(filter2(channel(img,'gray'), x), 10, 10, 0.01))
ac<-computeFeatures.shape(a)
ac<-ac[rev(order(ac[,1])),]
a[!a%in%as.numeric(rownames(ac)[1:8])]<-0
display(a,method='raster')
# 
fHigh <- matrix(1, nc = 3, nr = 3)
fHigh[2, 2] <- -8
Image.fHigh <- filter2(img, fHigh)
display(Image.fHigh)
display(Image.fHigh[,,1])
display(Image.fHigh[,,2])
display(Image.fHigh[,,3])

display(img>otsu(channel(img,'gray'), levels=4))

img.0<-thresh(channel(img,'gray'),10,10, .05)
img.0<-opening(img.0, makeBrush(5, shape='disc'))
display(img.0>otsu(img.0, levels=4))

imTF<-img.0>otsu(img.0, levels=4)
imOtsu<-img.0
imOtsu[!imTF]<-0
otsu1<-otsu2<-bwlabel(imOtsu)
otsu1[otsu1>1]<-0
display(otsu1)
otsu2[otsu2>2|otsu2==1]<-0
display(otsu2)


#display(img.0>otsu(img.0, levels=100))
display(img[,,2]>otsu(channel(img,'gray'), levels=4)) #
display(img[,,3]>otsu(channel(img,'gray'), levels=4))

img.1<-1-img
display(img.1[,,1]>otsu(channel(img.1,'gray'), levels=4))
display(img.1[,,2]>otsu(channel(img.1,'gray'), levels=4))
display(img.1[,,3]>otsu(channel(img.1,'gray'), levels=4))

display(img[,,1]>otsu(img, levels=7))
display(img[,,2]>otsu(img, levels=7))#
display(img[,,3]>otsu(img, levels=7))



img2<-readImage("C:/Users/ygu/Desktop/columbia/images/scottish_terrier_142.jpg")
img2.0<-thresh(channel(img2,'gray'),10,10, .05)
img2.0<-opening(img2.0, makeBrush(5, shape='disc'))
display(img2.0>otsu(img2.0, levels=4))

display(thresh(filter2(channel(img2,'gray'), x), 10, 10, 0.04), method="raster")
a<-bwlabel(thresh(filter2(channel(img2,'gray'), x), 10, 10, 0.01))
ac<-computeFeatures.shape(a)
ac<-ac[rev(order(ac[,1])),]
a[!a%in%as.numeric(rownames(ac)[1:8])]<-0
display(a,method='raster')

img3<-readImage("C:/Users/ygu/Desktop/columbia/images/pug_32.jpg")
display(img3,method='raster')
img3.0<-thresh(channel(img3,'gray'),10,10, .05)
img3.0<-opening(img3.0, makeBrush(5, shape='disc'))
display(img3.0>otsu(img3.0, levels=4))

display(thresh(filter2(channel(img3,'gray'), x), 10, 10, 0.01), method="raster")
a<-bwlabel(thresh(filter2(channel(img3,'gray'), x), 10, 10, 0.01))
ac<-computeFeatures.shape(a)
ac<-ac[rev(order(ac[,1])),]
a[!a%in%as.numeric(rownames(ac)[1:10])]<-0
display(a,method='raster')


img4<-readImage("C:/Users/ygu/Desktop/columbia/images/sphynx_175.jpg")
display(img4,method='raster')
img4.0<-thresh(channel(img4,'gray'),10,10, .05)
img4.0<-opening(img4.0, makeBrush(5, shape='disc'))
display(img4.0>otsu(img4.0, levels=4))

display(thresh(filter2(channel(img4,'gray'), x), 10, 10, 0.04), method="raster")
a<-bwlabel(thresh(filter2(channel(img4,'gray'), x), 10, 10, 0.01))
ac<-computeFeatures.shape(a)
ac<-ac[rev(order(ac[,1])),]
a[!a%in%as.numeric(rownames(ac)[1:10])]<-0
display(a,method='raster')

computeFeatures.moment(a)
computeFeatures.shape(a)

display(img2)
display(img2[,,1]>otsu(channel(img2,'gray'), levels=4))
display(img2[,,2]>otsu(channel(img2,'gray'), levels=4)) #
display(img2[,,3]>otsu(channel(img2,'gray'), levels=4))
display(img2[,,1]>otsu(img2, levels=7))
display(img2[,,2]>otsu(img2, levels=7))#
display(img2[,,3]>otsu(img2, levels=7))



display(img>otsu(x, levels=256))


display(img)
imTF<-img[,,1]>otsu(x, levels=4)
imOtsu<-img[,,1]
imOtsu[!imTF]<-0

otsu1<-bwlabel(imOtsu)
otsu1[otsu1>1]<-0
display(otsu1)

momentsOtsu=computeFeatures.moment(bwlabel(imOtsu))
ftpOtsu=computeFeatures.shape(bwlabel(imOtsu))
OtsuFeatures<-cbind(momentsOtsu,ftpOtsu)


plot.new()
plot.window(c(1,600),c(1,400))
usr<-par("usr")    
rasterImage(imOtsu, usr[1], usr[3], usr[2], usr[4])
OtsuFeatures2<-OtsuFeatures[OtsuFeatures[,6]>10,]
sapply(1:nrow(OtsuFeatures2),function(i){
  text((OtsuFeatures2[i,1]),(400-OtsuFeatures2[i,2]),i,cex=1,col='green')
})

# z <- stackObjects(channel(bwlabel(imOtsu),'gray'), channel(bwlabel(imOtsu),'gray'))
# display(z, title='Stacked objects', all=TRUE)






#par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
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

plot.new()
plot.window(c(1,600),c(1,400))
usr<-par("usr")    
rasterImage(Image.fHigh[,,3], usr[1], usr[3], usr[2], usr[4])
moments3<-moments[moments[,3]<34&moments[,3]>0,]
sapply(1:nrow(moments3),function(i){
  text((moments3[i,1]),(400-moments3[i,2]),i,cex=1,col='green')
})




im2<-channel(Image.fHigh[,,1],"gray")
stackObjects(im2,img[,,1])
display(stackObjects(im2,img[,,1]),all=T)
cmaskt = closing( gblur(tub, 1) > 0.105, makeBrush(5, shape='disc') )

cmask  = propagate(tub, seeds=nmask, mask=cmaskt, lambda = 0.001)


#############################################
x = readImage(system.file('images', 'shapes.png', package='EBImage'))
x = x[110:512,1:130]
y = bwlabel(x)
display(normalize(y), title='Objects')
z = stackObjects(y, normalize(y))
display(z, title='Stacked objects')

## load images
nuc = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
cel = readImage(system.file('images', 'cells.tif', package='EBImage'))
img = rgbImage(green=cel, blue=nuc)
display(img, title='Cells')

## segment nuclei
nmask = thresh(nuc, 10, 10, 0.05)
nmask = opening(nmask, makeBrush(5, shape='disc'))
nmask = fillHull(bwlabel(nmask))

## segment cells, using propagate and nuclei as 'seeds'
ctmask = opening(cel>0.1, makeBrush(5, shape='disc'))
cmask = propagate(cel, nmask, ctmask)

## using paintObjects to highlight objects
res = paintObjects(cmask, img, col='#ff00ff')
res = paintObjects(nmask, res, col='#ffff00')
display(res, title='Segmented cells')

## stacked cells
st = stackObjects(cmask, img)
display(st, all=T)
################################################

install.packages('imager')
library(imager)

summary(boats)
class(boats)
dim(boats)

cat1<-readJPEG("C:/Users/ygu/Desktop/columbia/Abyssinian_1.jpg")
cat<-as.cimg(cat1)
#cat<-load.image("C:/Users/ygu/Desktop/columbia/Abyssinian_1.JPEG")

grad <- imgradient(cat,"xy")
str(grad)
layout(t(1:2))
plot(grad$x,main="Gradient along x")
plot(grad$y,main="Gradient along y")

grad.sq <- grad %>% llply(function(v) v^2)
layout(t(1:2))
plot(sqrt(grad.sq$x),main="Gradient magnitude along x")
plot(sqrt(grad.sq$y),main="Gradient magnitude along y")

grad.sq <- add(grad.sq) #Add (d/dx)^2 and (d/dy)^2
plot(imrotate(sqrt(grad.sq),90))

edges <- imsplit(grad.sq,"c") %>% add
plot(imrotate(sqrt(edges),90),main="Detected edges")

# detect.edges <- function(im,sigma=1)
# {
#   isoblur(im,sigma) %>% imgradient("xy") %>% llply(function(v) v^2) %>% add %>% imsplit("c") %>% add
# }
# 
# detect.edges(cat,1) %>% sqrt %>%imrotate(90) %>% plot


pmap <- 1/(1+edges) #Priority inv. proportional to gradient magnitude
imrotate(pmap*100,90) %>%plot(main="Priority map") #Nice metal plate effect! 

seeds <- imfill(width(pmap),height(pmap)) #Empty image
seeds[1,1,1,1] <- 1 #Background pixel 
seeds[150,400,1,1] <- 2 #Foreground pixel

wt <- watershed(seeds,pmap)
plot(wt,main="Watershed segmentation")

mask <- add.colour(wt) #We copy along the three colour channels
layout(t(1:2))
plot(cat*(mask==1),main="Background")
plot(cat*(mask==2),main="Foreground")


imd2<-function(width,height,x,y) imdirac(c(width,height,1,1),x,y)
im2<-imd2(width(pmap),height(pmap),1,1)+2*imd2(width(pmap),height(pmap),width(pmap)/2,height(pmap)/2)+
  3*imd2(width(pmap),height(pmap),width(pmap),height(pmap))
wt2 <- watershed(im2,pmap)
plot(wt2,main="Watershed segmentation")





#In our initial image we'll place three seeds 
#(non-zero pixels) at various locations, with values 1, 2 and 3. 
#We'll use the watershed algorithm to propagate these values
imd <- function(x,y) imdirac(c(100,100,1,1),x,y)
im <- imd(20,20)+2*imd(40,40)+3*imd(80,80)
layout(t(1:3))
plot(im,main="Seed image")
#Now we build an priority map: neighbours of our seeds 
#should get high priority. 
#We'll use a distance map for that
p <- 1-distance_transform(sign(im),1) 
plot(p,main="Priority map")
watershed(im,p) %>% plot(main="Watershed transform")


NewTrain<-cbind(as.numeric(as.character(train_label)),baseline_train_features)
colnames(NewTrain)<-c('y',paste0('x',1:888))
OldTrain<-NewTrain[,1:801]
NewTest<-cbind(as.numeric(as.character(test_label)),baseline_test_features)
colnames(NewTest)<-c('y',paste0('x',1:888))
OldTest<-NewTest[,1:801]
system.time(gbmOld<-gbm(y~.,
            distribution = "bernoulli",
            data = data.frame(OldTrain),
            n.trees = 500,
            interaction.depth = 3,
            n.minobsinnode = 50,
            shrinkage = 0.1,
            bag.fraction = 1,
            train.fraction = .7,
            cv.folds=5,
            keep.data = TRUE,
            verbose =T,
            class.stratify.cv=NULL,
            n.cores = NULL))

gbmOld$valid.error[best.iter]
pred.gbmOld<-predict(gbmOld,data.frame(OldTest),n.trees=best.iter,type='response')
pred.gbmOldClass<-round(pred.gbmOld,0)

error<-table(pred = pred.gbmOldClass, true = test_label)
(error[2] + error[3]) / sum(error)

depth 1,2,3
nodes 10,50
shrinkage 0.1,0.01,0.001

data.frame(depth=rep(1:3,each=6),nodes=rep(rep(c(10,50),each=3),3),shrinkage=rep(c(.1,.01,.001),6))

y<-1:333
kfolds<-5
set.seed(8)
id<-sample(1:length(y))
foldStart<-round(0:(kfolds-1)*(length(id)/kfolds)+1,0)
foldEnd<-round(1:(kfolds)*(length(id)/kfolds),0)


baseline_train_featuresOld<-baseline_train_features[,1:800]

baseline_train_featuresOld2<-
  baseline_train_featuresOld[,apply(baseline_train_featuresOld, 2, var, na.rm=TRUE) != 0]

pca <- prcomp(baseline_train_featuresOld2, retx=TRUE, center=TRUE, scale=TRUE)
summary(pca)

















