x=seq(-5,5,0.1)

#step function
step.func <- function(x){
  return(as.numeric(x > 0))
}
#sigmoid function
sigmoid.func<-function(x){
  return(1/(1+exp(-x)))
}
#relu function
relu.func<-function(x){
  return(pmax(0,x))
}
#layer perceptionの初期化
init.3L.network <- function(n.input,n.L1,n.L2,n.output){
  W1=matrix(rnorm(n.input*n.L1),ncol=n.L1)
  W2=matrix(rnorm(n.L1*n.L2),ncol=n.Lr1)
  W3=matrix(rnorm(n.L2*n.output),ncol=n.L2)
  bi=matrix(rnorm(n.L1),ncol=n.L2)
  b2=matrix(rnorm(n.L2),ncol=n.output)
  b3=matrix(rnorm(output),ncol=n.output)
  return(list(W1=W1,b1=b1,W2=W2,b2=b2,W3=W3,b3=b3)) #属性と中身を表示する
}

#vectorの構造の変化
init.network < - function(n.neurons){
  n.layer=length(n.neurons)
  W-list();b=list()
  for(i.layer in 1:n.layer){
    w[[i.layer]]=matrix(rnorm(n.neurons[i.layer]*n.neurons[i.layer+1],
                              ncol=n.neurons[i.layer+1]))
    b[[i.layer]]=matrix(rnorm(n.neurons[(i.layer+1)]),nrow=1)
 }
  return(list(W=w,b=b))
}
network=init.network
y = forward(network, x)
x=n.neurons

#課題
#汎用化
init.network<-function(n.neurons){
  n.layer=length(n.neurons)
  W-list();b=list()
  for(i.layer in 1:n.layer){
    w[[i.layer]]=matrix(rnorm(n.neurons[i.layer]*n.neurons[i.layer+1],ncol=n.neurons[i.layer+1]))
    b[[i.layer]]=matrix(rnorm(n.neurons[(i.layer+1)]),nrow=1) }
  return(list(W=w,b=b))
}
network=init.network

forward<-function(network,x){
  A = x%*%network$W + network$b
}


#活性化関数を任意に選び使用するコード
#if文でやってみる

#活性化関数の種類
step.func <- function(x){
 return(as.numeric(x > 0))
}
sigmoid.func<-function(x){
  return(1/(1+exp(-x)))
}
relu.func<-function(x){
  return(pmax(0,x))
}
y.step=step.func(x)
y.sigm=sigmoid.func(x)
y.relu=relu.func(x)
if(y.step){
  y.step=A
  return(A)
}else if(y.sigmoid){
  y.sigmoid=A
  return(A)
}else if(y.relu){
  y.relu=A
  return(A)
}

#networkの初期化
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

# reluの変形
relu.func <- function(x){
  y = apply(x,2,function(x) pmax(x,0))
  return(y)
}