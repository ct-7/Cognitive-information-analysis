#2乗和誤差
MSE <- function(target, y){
  return(0.5*sum((target-y)^2))
}

t = rep(0,10)
t[3]=1
y = c(0.1, 0.05, 0.6, 0, 0.05, 0.1, 0, 0.1, 0, 0)

x = seq(0,1,0.01)
plot(x,-log(x),lwd = 2)

cross.entropy = function(y, target){
  delta = 1e-7;
  R = nrow(as.matrix(y))
  return(-sum(target*log(y + delta))/R)
}

numerical.diff = function(func, x){
  h = 1e-4
  plusH = do.call(func,list(x+h))
  minusH = do.call(func,list(x-h))
  num.diff = (plusH - minusH)/(2*h)
  return(num.diff)
}

func01 = function(x){
  return(0.01*x^2+0.1*x)
}

x = seq(0,20,0.1)
y = func01(x)
plot(x,y,xlab ="x", ylab = "f(x)",type = "l",lwd =2)
ND.5 = numerical.diff('func01',5)
abline(a = func01(5)-ND.5*5, b = ND.5, col = 'red',  lwd =2)
abline(v = 5, lty = 2, col = 'red')
ND.10 = numerical.diff('func01',10)
abline(a = func01(10)-ND.10*10, b = ND.10, col = 'blue',lwd = 2)
abline(v = 10, lty = 2, col = 'blue')


func02 = function(x0, x1){
  return(x0^2 + x1^2)
}
func02.x0 = function(x0){
  return(x0^2)
}
func02.x1 = function(x1){
  return(x1^2)
}

func02R = function(x){
  return(x[1]^2 + x[2]^2)
}
numerical.grad <- function(func, x){
  h = 1e-4
  R = nrow(x)
  C = ncol(x)
  grad = matrix(0, R, C)
  for (i.col in 1:C){
    for (i.row in 1:R){
      temp.x = x[i.row,i.col]
      x[i.row, i.col] = temp.x + h
      plusH = do.call(func, list(x))
      x[i.row, i.col] = temp.x - h
      minusH = do.call(func,list(x))
      grad[i.row, i.col] = (plusH - minusH)/(2*h)
      x[i.row, i.col] = temp.x
    }
  }
  return(grad)
}

numerical.grad("func02R",matrix(c(3,4),nrow=1))
numerical.grad("func02R",matrix(c(0,4),nrow=1))
numerical.grad("func02R",matrix(c(3,0),nrow=1))

require(plot3D)
x = seq(-2,2,0.2)
y = seq(-2,2,0.2)
M = mesh(x,y)
R = nrow(M$x)
C = nrow(M$x)
scaling = 0.05
plot(c(),c(),xlim = c(-2,2),ylim=c(-2,2))
for (i.col in 1:C){
  for (i.row in 1:R){
    ng = numerical.grad("func02R",matrix(c(M$x[i.row,i.col],M$y[i.row,i.col]),nrow=1))
    arrows(M$x[i.row,i.col],M$y[i.row,i.col],
           (M$x[i.row,i.col]-ng[1]*scaling),(M$y[i.row,i.col]-ng[2]*scaling),
           length = 0.05)
  }
}

grad.desc <- function(func, init.x, lr, n.iter){
  x = init.x
  for (i.iter in 1:n.iter) {
    grad = numerical.grad(func, x)
    x = x - lr*grad
  }
  return(x)
}
x.init = matrix(c(-3,4),nrow = 1)
grad.desc("func02R",x.init,0.1,100)

x = seq(-4,4,0.2)
y = seq(-4,4,0.2)
M = mesh(x,y)
Z = as.vector(M$x^2)+as.vector(M$y^2) 
Z.mesh = matrix(Z,nrow(M$x))
contour(x,y,Z.mesh,drawlabels = F)
grad.desc2 <- function(func, init.x, lr, n.iter){
  x = init.x
  x.hist = init.x
  for (i.iter in 1:n.iter) {
    grad = numerical.grad(func, x)
    x = x - lr*grad
    x.hist = rbind(x.hist,x)
  }
  return(x.hist)
}
gd = grad.desc2("func02R",x.init,0.1,100)
points(gd,col = 'green',pch=20)

# manual implementation
w = matrix(c(0.47355232,0.85557411,0.9977393,0.03563661,0.84668094,0.69422093),nrow=2)
x = matrix(c(0.6, 0.9), nrow=1)
t = c(0,0,1)
nn.predict <- function(w,x){
  return(x%*%w)
}

loss.func = function(w, x, t){
  pred = nn.predict(w,x)
  y = softmax.func(pred)
  return(cross.entropy(y, t))
}


numerical.gradCE <- function(func, w, x, t){
  # input args
  # func: name of function
  # w   : weight
  # x   : input 
  # t   : target output
  ##############################################
  h = 1e-4
  R = nrow(w)
  C = ncol(w)
  grad = matrix(0, R, C)
  for (i.col in 1:C){
    for (i.row in 1:R){
      temp.w = w[i.row,i.col]
      w[i.row, i.col] = temp.w + h
      plusH = do.call(func, list(w,x,t))
      w[i.row, i.col] = temp.w - h
      minusH = do.call(func,list(w,x,t))
      grad[i.row, i.col] = (plusH - minusH)/(2*h)
      w[i.row, i.col] = temp.w
    }
  }
  return(grad)
}
dW = numerical.gradCE("loss.func",w,x,t)

### ch 4.5 2-layer NN  ###
init.2LN <- function(n.input, n.hidden, n.output, w.std = 0.01){
  W1 = matrix(rnorm(n.input*n.hidden,0,w.std),nrow = n.input)
  B1 = matrix(rnorm(n.hidden,0,w.std),nrow =1)
  W2 = matrix(rnorm(n.hidden*n.output,0,w.std),nrow = n.hidden)
  B2 = matrix(rnorm(n.output,0,w.std),nrow =1)
  return(list(W1 = W1, B1 = B1, W2 = W2, B2 = B2))
}

softmax.2LN <- function(x){
  max.x = apply(x,1,max)
  C = ncol(x)
  x = x - max.x%*%matrix(1,nrow=1,ncol=C)
  return(exp(x)/rowSums(exp(x)))
}

sigmoid.func <- function(x){
  return(1/(1+exp(-x)))
}

pred.2LN <- function(params, x){
  NR = nrow(x)
  a1 = x%*%params$W1 + matrix(1,nrow = NR)%*%params$B1
  z1 = sigmoid.func(a1)
  a2 = z1%*%params$W2 + matrix(1,nrow = NR)%*%params$B2
  y = softmax.2LN(a2)
  return(y)
}

loss.2LN = function(params, x, t){
  y = pred.2LN(params,x)
  return(cross.entropy(y, t))
}

numerical.grad2LN <- function(func, params, x, t) {
  # input args
  # func: name of function
  # w   : weight
  # x   : input
  # t   : target output
  ##############################################
  h = 1e-4; n.list = length(params); grad = params
  for (i.list in 1:n.list) {
    R = nrow(params[[i.list]])
    C = ncol(params[[i.list]])
    grad[[i.list]] = matrix(0, R, C)
    for (i.col in 1:C) {
      for (i.row in 1:R) {
        temp.w = params[[i.list]][i.row, i.col]
        params[[i.list]][i.row, i.col] = temp.w + h
        plusH = do.call(func, list(params, x, t))
        params[[i.list]][i.row, i.col] = temp.w - h
        minusH = do.call(func, list(params, x, t))
        grad[[i.list]][i.row, i.col] = (plusH - minusH) / (2 * h)
        params[[i.list]][i.row, i.col] = temp.w
      }
    }
  }
  return(grad)
}

## example using IRIS data set
train.x = as.matrix(iris[,1:4])
train.y.temp = as.numeric(iris[,5])
train.y = matrix(0,nrow = nrow(train.x), ncol =3)
train.y[which(train.y.temp==1), 1]=1
train.y[which(train.y.temp==2), 2]=1
train.y[which(train.y.temp==3), 3]=1

params = init.2LN(4,15,3,0.01)
batch_size = 7; n.iter =2000; lambda =0.05
n.train = nrow(train.x)
loss = rep(0,n.iter)
for (i.iter in 1:n.iter){
  batch_mask = sample(1:n.train, batch_size)
  x.batch = train.x[batch_mask,]
  t.batch = train.y[batch_mask,]
  dW = numerical.grad2LN("loss.2LN",params,x.batch,t.batch)
  params$W1 = params$W1 - lambda*dW$W1
  params$B1 = params$B1 - lambda*dW$B1
  params$W2 = params$W2 - lambda*dW$W2
  params$B2 = params$B2 - lambda*dW$B2
  loss[i.iter] = loss.2LN(params,x.batch,t.batch)
}