勾配法をmomentで加える
勾配法をmomentで加える
適当なラムダとaを探す
matrix(numeric(50),1)
numeric(10)

grad.desc <- function(func, init.x, lr, n.iter){ #gradientと学習率をかけている
  x = init.x
  x.hist = init.x
  v = 0
  a = 0.8
  for (i.iter in 1:n.iter) {　　
    grad = numerical.grad(func, x)
    x = x - lr*grad　#一つの数値だけを更新するときは[1]などの記号は使わなくていい
    v= v*a- lr*grad
    x = x + v
    x.hist = rbind(x.hist,x)
  }
  return(x.hist)
}
multi.forwd <- function(x,y){
  return(x*y)
}
multi.bckwd <- function(x, y, dout){
  dx = dout * y
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

add.forwd <- function(x,y){
  return(x + y)
}
add.bckwd <- function(x, y, dout){
  dx = dout
  dy = dout
  return(list(dx = dx, dy = dy))
}

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
  max.x = apply(x,1,max)
  C = ncol(x)
  x = x - max.x%*%matrix(1,nrow=1,ncol=C)
  y = exp(x)/rowSums(exp(x))
  delta = 1e-7;
  R = nrow(as.matrix(y))
  return(-sum(target*log(y + delta))/R)
}

softmax.bckwd <- function(x, target,  dout = 1){
  max.x = apply(x, 1, max)
  R = nrow(x)
  C = ncol(x)
  x = x - max.x%*%matrix(1,nrow=1,ncol=C)
  y = exp(x)/rowSums(exp(x))
  return((y-target)/R)
}

init.network <- function(n.neurons){
  n.layer = length(n.neurons)
  W = list(); b = list()
  for (i.layer in 1:(n.layer-1)){
    W[[i.layer]] = matrix(rnorm(n.neurons[i.layer]*n.neurons[(i.layer+1)],sd = 0.1),
                          nrow=n.neurons[i.layer])
    b[[i.layer]] =  matrix(rnorm(n.neurons[(i.layer+1)],sd = 0.1), nrow = 1)
  }
  return(list(W = W,b = b))
}

sigmoid.func <- function(x){
  return(1/(1+exp(-x)))
}

softmax<- function(x){
  max.x = apply(x,1,max)
  C = ncol(x)
  x = x - max.x%*%matrix(1,nrow=1,ncol=C)
  return(exp(x)/rowSums(exp(x)))
}

relu.func <- function(x){
  y = apply(x,2,function(x) pmax(x,0))
  return(y)
}

activation <- function(A, actFun){
  if (actFun == "sigmoid"){
    return(sigmoid.func(A))
  }
  if (actFun == "relu"){
    return(relu.func(A))
  }
  if (actFun == "softmax"){
    return(softmax(A))
  }
}

feedforward <- function(network, x, actFun) {
  n.layer <- length(network$W)
  batch.size = nrow(x)
  for (i.layer in 1:n.layer){
    A = x%*%network$W[[i.layer]] 
    + matrix(1,nrow=batch.size,ncol = 1)%*%network$b[[i.layer]]
    x = activation(A, actFun[i.layer])
  }
  return(x)
}

cross.entropy = function(y, target){
  delta = 1e-7;
  R = nrow(as.matrix(y))
  return(-sum(target*log(y + delta))/R)
}

loss.network = function(params, x, t, actFun){
  y = feedforward(params,x,actFun)
  return(cross.entropy(y, t))
}

train.x = as.matrix(iris[,1:4])
train.y.temp = as.numeric(iris[,5])
train.y = matrix(0,nrow = nrow(train.x), ncol =3)
train.y[which(train.y.temp==1), 1]=1
train.y[which(train.y.temp==2), 2]=1
train.y[which(train.y.temp==3), 3]=1

params = init.network(c(4,15,3))
batch_size = 10; n.iter =1000; lr =0.1
n.train = nrow(train.x)
loss = rep(0,n.iter)

#moment

n.layer = length(n.neurons)
W = list(); b = list()
for (i.layer in 1:(n.layer-1)){
  params$W[[i.layer]] = matrix(rnorm(n.neurons[i.layer]*n.neurons[(i.layer+1)],sd = 0.1),
                        nrow=n.neurons[i.layer])
  b[[i.layer]] =  matrix(rnorm(n.neurons[(i.layer+1)],sd = 0.1), nrow = 1)
}
vW=list();vb=list()
nW.iter=nrow(params$W)
nb.iter=nrow(params$b)
for(i.iter in 1:n.iter)
  
vW=params$W
vb=params$b

vW[[1]]=matrix(0,nrow=nrow(params$W[[1]]),ncol=ncol(params$W[[1]]))
vW[[2]]=matrix(0,nrow=nrow(params$W[[2]]),ncol=ncol(params$W[[2]]))
vb[[1]]=matrix(0,nrow=nrow(params$b[[1]]),ncol=ncol(params$b[[1]]))
vb[[2]]=matrix(0,nrow=nrow(params$b[[2]]),ncol=ncol(params$b[[2]]))

a=0.1
for (i.iter in 1:n.iter){
  batch_mask = sample(1:n.train, batch_size)
  x.batch = train.x[batch_mask,]
  t.batch = train.y[batch_mask,]
  a1 = affine.forwd(x.batch,params$W[[1]],params$b[[1]])
  z1 = sigmoid.forwd(a1)
  a2 = affine.forwd(z1,params$W[[2]],params$b[[2]])
  z2 = softmax.forwd(a2,t.batch)
  dwSM = softmax.bckwd(a2, t.batch, 1)
  dwSG = sigmoid.bckwd(a1,dwA2$dx)
  dwA2 = affine.bckwd(a1,params$W[[2]],params$b[[2]],dwSM)
  dwA1 = affine.bckwd(x.batch,params$W[[1]],params$b[[1]],dwSG)
# params$W[[2]] = params$W[[2]] - lambda*dwA2$dW
# params$b[[2]] = params$b[[2]] - lambda*dwA2$db
# params$W[[1]] = params$W[[1]] - lambda*dwA1$dW
# params$b[[1]] = params$b[[1]] - lambda*dwA1$db
  vW[[2]]=vW[[2]]*a - lr*dwA2$dW
  vb[[2]]=vb[[2]]*a - lr*dwA2$db
  vW[[1]]=vW[[1]]*a - lr*dwA1$dW
  vb[[1]]=vb[[1]]*a - lr*dwA1$db
  params$W[[2]] = params$W[[2]] + vW[[2]]
  params$b[[2]] = params$b[[2]] + vb[[2]]
  params$W[[1]] = params$W[[1]] + vW[[1]]
  params$b[[1]] = params$b[[1]] + vb[[1]]
  loss[i.iter] = loss.network(params,x.batch,t.batch,c("sigmoid","softmax"))
}
plot(loss,type='l', xlab = "trial")
par=mfrow(c(1,1))


#Adagrad
hw[[2]]=matrix(0,nrow=nrow(params$W[[1]]),ncol=ncol(params$W[[1]]))
hw[[2]]=hw[[2]]+dwA2$dW*dwA2$dW
params$W[[2]]=params$W[[2]]+lr*(1/sqrt(hw[2]))*dwA2$dW

#Adam
mw[[2]]=
vw[[2]]=
mb[[2]]=
vb[[2]]=
Mw[[2]]=
Mb[[2]]=
Vw[[2]]=
Vb[[2]]=
mw[[2]]=beta1*mw[[2]]+(1-beta1)*dwA2$dW
vw[[2]]=beta1*vw[[2]]+(1-beta2)*dwA2$dW*dwA2$dW
Mw[[2]]=mw[[2]]/(1-beta1)
Vw[[2]]=vw[[2]]/(1-beta2)
params$W[[2]]=params$W[[2]]-(lr/sqrt(Vw[[2]]+e))*Mw[[2]]


params = init.network(c(4,15,3))
batch_size = 10; n.iter =5000; lambda =0.05
n.train = nrow(train.x)
loss = rep(0,n.iter)
for (i.iter in 1:n.iter){
  batch_mask = sample(1:n.train, batch_size)
  x.batch = train.x[batch_mask,]
  t.batch = train.y[batch_mask,]
  a1 = affine.forwd(x.batch,params$W[[1]],params$b[[1]])
  z1 = sigmoid.forwd(a1)
  a2 = affine.forwd(z1,params$W[[2]],params$b[[2]])
  z2 = softmax.forwd(a2,t.batch)
  dwSM = softmax.bckwd(a2, t.batch, 1)
  dwA2 = affine.bckwd(a1,params$W[[2]],params$b[[2]],dwSM)
  dwSG = sigmoid.bckwd(a1,dwA2$dx)
  dwA1 = affine.bckwd(x.batch,params$W[[1]],params$b[[1]],dwSG)
  for (j in 1:j){
   mw[[j]]=beta1*mw[[j]]+(1-beta1)*dwA2$dW
   vw[[j]]=beta1*vw[[j]]+(1-beta2)*dwA2$dW*dwA2$dW
   Mw[[j]]=mw[[j]]/(1-beta1)
   Vw[[j]]=vw[[j]]/(1-beta2)
   mb[[j]]=beta1*mb[[j]]+(1-beta1)*dwA2$db
   vb[[j]]=beta1*vb[[j]]+(1-beta2)*dwA2$db*dwA2$db
   Mb[[j]]=mb[[j]]/(1-beta1)
   Vb[[j]]=vb[[j]]/(1-beta2)
   params$W[[j]]=params$W[[j]]-(lr/sqrt(Vw[[j]]+e))*Mw[[j]]
   params$W[[1]]=params$W[[1]]-(lr/sqrt(Vw[[1]]+e))*Mw[[1]]
   params$b[[2]]=params$b[[2]]-(lr/sqrt(Vb[[2]]+e))*Mb[[2]]
   params$b[[1]]=params$b[[1]]-(lr/sqrt(Vb[[1]]+e))*Mb[[1]]
  }
  loss[i.iter] = loss.network(params,x.batch,t.batch,c("sigmoid","softmax"))
}