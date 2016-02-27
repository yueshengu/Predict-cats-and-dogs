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
