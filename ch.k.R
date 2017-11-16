relu.func <- function(x){
  y = apply(x,2,function(x) pmax(x,0))
  return(y)
}

sigmoid.func <- function(x){
  return(1/(1+exp(-x)))
}

init.network <- function(n.neurons){
  n.layer = length(n.neurons)
  W = list(); b = list()
  for (i.layer in 1:(n.layer-1)){
    W[[i.layer]] = 
      matrix(rnorm(n.neurons[i.layer]*n.neurons[(i.layer+1)]),nrow=n.neurons[i.layer])
    b[[i.layer]] =  matrix(rnorm(n.neurons[(i.layer+1)]), nrow = 1)
  }
  return(list(W = W,b = b))
}

activation <- function(A, actFun){
  if (actFun == "sigmoid"){
    return(sigmoid.func(A))
  }
  if (actFun == "relu"){
    return(relu.func(A))
  }
}

feedforward <- function(network, x, actFun) {
  n.layer <- length(network$W)
  batch.size = nrow(x)
  for (i.layer in 1:n.layer){
    A = x%*%network$W[[i.layer]] + 
      matrix(1,nrow=batch.size,ncol = 1)%*%network$b[[i.layer]]
    x = activation(A, actFun[i.layer])
  }
  return(x)
}