#画像の数字の位置に関わらず同じ数字だと判別するためのもの
#Pooling convolution

OutputHeight = OH
ImageHeight = IH
filterHeight = fH
OutputWide   = OW
ImageWide =  IW
filterWide = fW

img = matrix(1:25,nrow=5,byrow = T)
IW = ncol(img)
IH = nrow(img)
fH = 3
fW = 3

OH = IH - fH + 1
OW = IW - fW + 1

im2col <- function(img,fW,fH){
  IW = ncol(img)
  IH = nrow(img)
  OH = IH - fH + 1
  OW = IW - fW + 1
  c.image = matrix(0,ncol = fW*fH,nrow = OW*OH)
  counter = 0
  for (i.col in 1:OW){
    for(i.row in 1:OH){
    counter = counter + 1
    c.image[counter,] = as.vector(t(img[(i.row:(i.row+fH-1)),(i.col:(i.col+fW-1))]))
    }
  }
  return(c.image)
}
c.image=im2col(img,3,3)

pooling <- function(c.image,fW,fH){
  max.vec = apply(c.image,1,max)
  max.mat = matrix(max.vec,nrow=fH,ncol=fW,byrow = T)
  return(list(max.vec=max.vec,max.mat=max.mat))
}
pooling(c.image,fW,fH)

filter = matrix(c(1,1,1,rep(0,6)),3)
c.image=im2col(img,3,3)
c.filter = im2col(filter,3,3)

#一つのフィルターの場合
convolution <- function(c.image,c.filter){
  filt.vec = c.image%*%t(c.filter)
  filt.mat = matrix(filt.vec,nrow = fH,ncol =fW,byrow =T)
  return(list(filt.vec=filt.vec,filt.mat=filt.mat))
}

c.filter = matrix(sample(c(0,1),9,replace=T),nrow=10,ncol=9)
c.filter







