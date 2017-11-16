#任意のフィルターでバッチ処理する
#arrayで三次元に出力する

convolution <- function(c.image,c.filter){
  filt.vec.n = c.image%*%t(c.filter)
  n = length(filt.vec.n[,1])
  fH = fW = length(filt.vec.n[,1])^(1/2)
  for (i in 1:n){
    filt.vec.mat = matrix(filt.vec.n[,i],nrow = fH,ncol=fW,byrow = T)
  }
  return(array(filt.vec.n,c(fH,fW,n)))
}


c.filter = matrix(sample(c(0,1),9,replace=T),nrow=10,ncol=9)
convolution(c.image,c.filter)
