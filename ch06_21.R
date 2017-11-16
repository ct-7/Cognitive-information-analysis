# ReLU
relu.forwd <- function(x){
  return(pmax(x,0))
}
relu.bckwd <- function(z, dout){
  dout[which(z <= 0)] = 0　#acttivationの後のもの=z
  return(dout)
}
# sigmoid
sigmoid.forwd <- function(x){
  return(1/(1+exp(-x)))
}
sigmoid.bckwd <- function(z, dout){ #sigmoidの出力＝z
  return(dout*(1-z)*z)
}
# Affine
affine.forwd <- function(x, W, b){ #内積
  return(x%*%W + matrix(1, nrow = nrow(x), ncol = 1)%*%b)
}
affine.bckwd <- function(x, W, dout){
  dx = dout%*%t(W)
  dW = t(x)%*%dout
  db = colSums(dout)
  return(list(dx = dx, dW = dW, db = db))
}
# softmax with CrossEntropy
softmaxCe.forwd <- function(x, target){
  max.x = apply(x,1,max)
  C = ncol(x)
  x = x - max.x%*%matrix(1,nrow=1,ncol=C) #xの最大値を１
  y = exp(x)/rowSums(exp(x))
  delta = 1e-7;
  R = nrow(as.matrix(y))
  return(list(smx = y, ce = -sum(target*log(y + delta))/R))
}
softmaxCe.bckwd <- function(smx, target){
  R = nrow(smx)
  return((smx-target)/R)
}

activation <- function(A, target, actFun){
  if (actFun == "sigmoid"){
    return(sigmoid.forwd(A))
  }
  if (actFun == "relu"){
    return(relu.forwd(A))
  }
  if (actFun == "softmax"){
    return(softmax.forwd(A, target))
  }
}

#微分を計算する
Dact <- function(z, smx, target, dout, actFun){
  if (actFun == "sigmoid"){
    return(sigmoid.bckwd(z, dout))
  }
  if (actFun == "relu"){
    return(relu.bckwd(z, dout))
  }
  if (actFun == "softmax"){
    return(softmax.bckwd(smx, target))
  }
}

# function for initialization 
init.network <- function(n.neurons, actFun,sdv){
  n.layer = length(n.neurons)
  W = list(); b = list()
  for (i.layer in 1:(n.layer-1)){
   if (nargs()==2){
    if(actfun[i.layer]=="sigmoid"){
      #Xavier
      sd = 1/sqrt(n.neurons[i.layer])
    }else{
      #He
      sd = sqrt(2/n.neurons[i.layer])
    }
   }
  }
   n.layer = length(n.neurons)
   W = list(); b = list()
  for (i.layer in 1:(n.layer-1)){
    W[[i.layer]] = matrix(rnorm(n.neurons[i.layer]*n.neurons[(i.layer+1)],sd = sdv),
                          nrow=n.neurons[i.layer])
    b[[i.layer]] =  matrix(rnorm(n.neurons[(i.layer+1)],sd = sdv), nrow = 1)
  }
  return(list(W = W, b = b, actFun = actFun))
}

cu.nnet = function(train.x, train.y, net, HP = c(10,1000,0.05,0.01)){
  # HP: Hyperparameters
  # HP[1] = batch size
  # HP[2] = n iteration
  # HP[3] = learning rate
  # HP[4] = weiht decay
  n.layer <- length(net$W)
  loss = rep(0,HP[2]) #エラーに対応するloss
  A = list(); z = list(); Dact = list(); Daff = list()
  for (i.iter in 1:HP[2]){
    batch_mask = sample(1:nrow(train.x), HP[1])
    x.batch = train.x[batch_mask,]
    t.batch = train.y[batch_mask,]  
    for (i.layer in 1:n.layer){　#forward
      if (i.layer == 1){
        x = x.batch
      } else {
        x = z[[(i.layer - 1)]]
      }
      A[[i.layer]] = affine.forwd(x, net$W[[i.layer]], net$b[[i.layer]])
      z[[i.layer]] = activation(A[[i.layer]], t.batch, net$actFun[i.layer])
    }
    loss[i.iter] = z[[i.layer]]$ce
    smx = z[[i.layer]]$smx
    for (i.layerR in n.layer:1){　#bcwd
      if (i.layerR == n.layer){
        dout = 1
      } else {
        dout = Daff[[(i.layerR+1)]]$dx
      }
      Dact[[i.layerR]] = Dact(z[[i.layerR]], smx, t.batch, dout, net$actFun[i.layerR])
      if (i.layerR==1){
        x = x.batch
      } else {
        x = A[[(i.layerR-1)]]
      }
      Daff[[i.layerR]] = affine.bckwd(x, network$W[[i.layerR]], Dact[[i.layerR]])
    }
    for (i.layer in 1:n.layer){ 　#パラメータの更新
      net$W[[i.layer]] = net$W[[i.layer]] - HP[3]*Daff[[i.layer]]$dW - HP[4]net$W[[i.layer]]
      net$b[[i.layer]] = net$b[[i.layer]] - HP[3]*Daff[[i.layer]]$db - HP[4]net$b[[i.layer]]
    }
  }
  return(list(loss = loss, net = net))
}