#勾配法計算
func02R = function(x){
  return(1/20*x[1]^2 + x[2]^2)
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

#SGD
grad.desc <- function(func, init.x, lr, n.iter){
  x = init.x
  x.hist = init.x
  for (i.iter in 1:n.iter) {
    grad = numerical.grad(func, x)
    x = x - lr*grad
    x.hist = rbind(x.hist,x)
  }
  return(x.hist)
}

install.packages("plot3D")
require(plot3D)
x = seq(-10,10,0.2)
y = seq(-10,10,0.2)
M = mesh(x,y)
Z = as.vector(1/20*M$x^2)+as.vector(M$y^2) 
Z.mesh = matrix(Z,nrow(M$x))
contour(x,y,Z.mesh,drawlabels = F,nlevels=40)

x.init = matrix(c(-7,2),nrow = 1)
gd = grad.desc("func02R",x.init,0.9,100)
lines(gd,type='o',col = 'green',pch=20)

#Momentum 慣性
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

x.init = matrix(c(-7,2),nrow = 1)
gd = grad.desc("func02R",x.init,0.9,100)
lines(gd,type='o',col = 'red',pch=20)

#Adagrad
grad.desc <- function(func, init.x, lr, n.iter){
  x = init.x
  x.hist = init.x
  h = 0
  for (i.iter in 1:n.iter) {
    grad = numerical.grad(func, x)
    h = h + grad*grad
    x = x - lr*(1/sqrt(h))*grad
    x.hist = rbind(x.hist,x)
  }
  return(x.hist)
}

x.init = matrix(c(-7,2),nrow = 1)
gd = grad.desc("func02R",x.init,0.9,100) 
#始めの学習率の値を大きしないと途中で動けなくなる
lines(gd,type='o',col = 'blue',pch=20)

#Adam
grad.desc <- function(func, init.x, lr, n.iter){
  x = init.x
  x.hist = init.x
  e = 1e-7
  m = 0
  v = 0
  beta1 = 0.9
  beta2 = 0.999
  for (i.iter in 1:n.iter) {
    grad = numerical.grad(func, x)
    m = beta1*m + (1-beta1)*grad
    v = beta2*v + (1-beta2)*grad*grad
    M = m/(1-beta1)
    V = v/(1-beta2)
    x = x - (lr/sqrt(V+e))*M
    x.hist = rbind(x.hist,x)
  }
  return(x.hist)
}

x.init = matrix(c(-7,2),nrow = 1)
gd = grad.desc("func02R",x.init,0.2,100)
lines(gd,type='o',col = 'orange',pch=20)
