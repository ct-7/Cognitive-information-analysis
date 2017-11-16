#加算レイヤ
add.forwd <- function(x,y){
  return(x + y)
}
add.bckwd <- function(x, y, dout){
  dx = dout
  dy = dout
  return(list(dx = dx, dy = dy))
}
#乗算レイヤ
multi.forwd <- function(x,y){
  return(x*y)
}
multi.bckwd <- function(x, y, dout){　#doutは上流から伝わってきた微分
  dx = dout * y #ひっくり返した値
  dy = dout * x
  return(list(dx = dx, dy = dy))
}

apple = 100; n.apple = 2; tax = 1.1
apple.pre.tax = multi.forwd(apple, n.apple)
apple.post.tax = multi.forwd(apple.pre.tax, tax)

dprice = 1
d.apple.post.tax = multi.bckwd(apple.pre.tax, tax, dprice)
d.apple = multi.bckwd(apple, n.apple, d.apple.post.tax$dx)$dx
d.n.apple = multi.bckwd(apple, n.apple, d.apple.post.tax$dx)$dy

a(100,2,150,3,1.1)
apple = 100; n.apple = 2; tax = 1.1
orange = 150; n.orange = 3;

apple.price = multi.forwd(apple, n.apple)
orange.price = multi.forwd(orange, n.orange)
all.price = add.forwd(apple.price, orange.price)
price = multi.forwd(all.price, tax)

dprice = 1
d.all.price = multi.bckwd(all.price, tax, dprice)
d.apple.price = add.bckwd(apple.price, orange.price, d.all.price$dx)$dx
d.orange.price = add.bckwd(orange, n.orange.price, d.all.price$dx)$dy
d.apple = multi.bckwd(apple, n.apple, d.apple.price)$dx
d.n.apple = multi.bckwd(apple, n.apple, d.apple.price)$dy
d.orange = multi.bckwd(orange, n.orange, d.orange.price)$dx
d.n.orange = multi.bckwd(orange, n.orange, d.orange.price)$dy

#活性化関数レイヤの実装
relu.forwd <- function(x){
  return(pmax(x,0))
}
relu.bckwd <- function(x, dout){
  dout[which(x <= 0)] = 0
  return(dout)
}
sigmoid.forwd <- function(x){
  return(1/(1+exp(-x)))
}
sigmoid.bckwd <- function(x, dout){
  y = sigmoid.forwd(x) 
  return(dout*(1-y)*y)
}

#Affine/softmaxレイヤの実装
affine.forwd <- function(x, W, b){
  return(x%*%W + matrix(1, nrow = nrow(x), ncol = 1)%*%b)
}
affine.bckwd <- function(x, W, b, dout){
  dx = dout%*%t(W)
  dW = t(x)%*%dout
  db = colSums(dout)
  return(list(dx = dx, dW = dW, db = db))
}
softmax.forwd <- function(x, target){
  max.x = apply(x,1,max) #各行の最大値を求める 　apply(データ,1(行)or2(列),関数)
  C = ncol(x) #入力データ
  x = x - max.x%*%matrix(1,nrow=1,ncol=C) #各行の最大値が０になるように計算する
  y = exp(x)/rowSums(exp(x)) #訓練データの確率
  delta = 1e-7;
  R = nrow(as.matrix(y))
  return(-sum(target*log(y + delta))/R)　#クロスエントロピーの計算
}
softmax.bckwd <- function(x, target,  dout = 1){
  max.x = apply(x, 1, max) 
  R = nrow(x)　
  C = ncol(x)
  x = x - max.x%*%matrix(1,nrow=1,ncol=C)　
  y = exp(x)/rowSums(exp(x)) 
  return((y-target)/R) #softmaxレイヤの出力と教師データの差
}
C = ncol(x)
a <- matrix(1,nrow=1,ncol=C)
t <- train.x[1:6,]
