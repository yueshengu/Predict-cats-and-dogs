source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
biocLite("SIFT.Hsapiens.dbSNP137")

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

fileNames<-dir('C:/Users/ygu/Desktop/columbia/images')
trainFileNames<-sample(fileNames,round(length(fileNames)*.7,0))

im2<-channel(Image.fHigh[,,1],"gray")
stackObjects(im2,img[,,1])
display(stackObjects(im2,img[,,1]),all=T)
cmaskt = closing( gblur(tub, 1) > 0.105, makeBrush(5, shape='disc') )

cmask  = propagate(tub, seeds=nmask, mask=cmaskt, lambda = 0.001)



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
display(st, title='Stacked objects')
