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

relu.func <- function(x){
  y = apply(x,2,function(x) pmax(x,0))
  return(y)
}

activation <- function(A, actFun){　#Aはパラメータの値
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

softmax<- function(x){
  max.x = apply(x,1,max)
  C = ncol(x)
  x = x - max.x%*%matrix(1,nrow=1,ncol=C)
  return(exp(x)/rowSums(exp(x)))
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

#クロスエントロピー誤差
cross.entropy = function(y, target){
  delta = 1e-7;
  R = nrow(as.matrix(y))
  return(-sum(target*log(y + delta))/R)
}

loss.network = function(params, x, t, actFun){
  y = feedforward(params,x,actFun)
  return(cross.entropy(y, t))
}

numerical.grad <- function(func, params, x, t, actFun) {　#数値微分によっての勾配
  # input args
  # func:   name of objective function
  # params: list of parameters (W & b)
  # x   :   input
  # t   :   target output
  # actFun: activation function
  ##############################################
  h = 1e-4
  n.list = length(params)
  grad = params
  for (i.list in 1:n.list) {
    R = nrow(params$W[[i.list]])
    C = ncol(params$W[[i.list]])
    grad$W[[i.list]] = matrix(0, R, C)  　　　　
    grad$b[[i.list]] = matrix(0, nrow = 1, C)　#gradにWとbのパラメータリストを新しく作る
    for (i.col in 1:C) {
      for (i.row in 1:R) {　　#パラメータの更新
        temp.w = params$W[[i.list]][i.row, i.col]
        params$W[[i.list]][i.row, i.col] = temp.w + h
        plusH = do.call(func, list(params, x, t, actFun))
        params$W[[i.list]][i.row, i.col] = temp.w - h
        minusH = do.call(func, list(params, x, t, actFun))
        grad$W[[i.list]][i.row, i.col] = (plusH - minusH) / (2 * h)
        params$W[[i.list]][i.row, i.col] = temp.w
      }
      temp.b = params$b[[i.list]][i.col]
      params$b[[i.list]][i.col] = temp.b + h
      plusH = do.call(func, list(params, x, t, actFun))
      params$b[[i.list]][i.col] = temp.b - h
      minusH = do.call(func, list(params, x, t, actFun))
      grad$b[[i.list]][i.col] = (plusH - minusH) / (2 * h)
      params$b[[i.list]][i.col] = temp.b
    }
  }
  return(grad)　#数値微分によっての勾配変数を格納
}

#入力データと訓練データの取得
train.x = as.matrix(iris[,1:4])
train.y.temp = as.numeric(iris[,5])
train.y = matrix(0,nrow = nrow(train.x), ncol =3)
train.y[which(train.y.temp==1), 1]=1
train.y[which(train.y.temp==2), 2]=1
train.y[which(train.y.temp==3), 3]=1

n.neurons = c(4,15,3)
params = init.network(n.neurons)
batch_size = 50; n.iter =2000; lambda =0.05 #
n.train = nrow(train.x)
loss = rep(0,n.iter)
n.layer = length(params$W)
for (i.iter in 1:n.iter){
  batch_mask = sample(1:n.train, batch_size)
  x.batch = train.x[batch_mask,]
  t.batch = train.y[batch_mask,]
  dW = numerical.grad("loss.network",params,x.batch,t.batch,c("sigmoid","softmax"))　#勾配を格納
  for (i.layer in 1:n.layer){
    params$W[[i.layer]] = params$W[[i.layer]]  - lambda*dW$W[[i.layer]]　#
    params$b[[i.layer]] = params$b[[i.layer]]  - lambda*dW$b[[i.layer]] 
  }
  loss[i.iter] = loss.network(params,x.batch,t.batch,c("sigmoid","softmax"))
}
plot(loss,type='l')
params$W[[2]] = params$W[[1]]  - lambda*dW$W[[1]]
params$b[[2]] = params$b[[1]]  - lambda*dW$b[[1]] 
