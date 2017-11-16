n.filter = 10 
n.channel = 3
IH = 10;IW = 10
fH = 5;fW = 5
img=array(runif(n.channel*IH*IW),c(1,n.channel,IH,IW))
filter = array(runif(n.filter*n.filter*fH*fW),c(n.filter,n.channel,fH,fW))

im2col <- function(img,fW,fH){
  IW = dim(img)[3];IH = dim(img)[4];NC = dim(img)[2] #dimで()の中の行数や列数を調べる
  OH = IH - fH + 1;OW = IW - fW + 1
  c.image = matrix(0,ncol = fW*fH*NC,nrow = OW*OH)
  counter = 0
  print(dim(c.image))
  for (i.col in 1:OW){
    for(i.row in 1:OH){
      temp=c() #初期化
      counter = counter + 1
      for (i.c in 1:NC)
        temp = c(temp,as.vector(t(img[1,i.c,(i.row:(i.row+fH-1)),(i.col:(i.col+fW-1))])))
      } #1は写真の枚数　増やしていけばいい
  }
  return(c.image)
}
im2col(img,fW,fH)


reshape.filter <- function(filter){
  n.filter = dim(filter)[1]
  n.channel = dim(filter)[2]
  n.row = dim(filter)[3]
  n.col = dim(filter)[4]
  c.filter = matrix(0,nrow=n.filter,ncol=n.channel*n.row*n.col)
  for (i.fil in 1:n.filter){
    temp = c()
    for (i.c in 1:n.channel){
      temp = c(temp,as.vector(t(filter[i.fil,i.c,,])))
    }
    c.filter[i.fil,] = temp
  }
  return(c.filter)
}
reshape.filter(filter)


pooling <- function(c.image,fW,fH){
  max.vec = apply(c.image,1,max)
  max.mat = matrix(max.vec,nrow=fH,ncol=fW,byrow = T)
  return(list(max.vec=max.vec,max.mat=max.mat))
}
pooling(c.image,fW,fH)



