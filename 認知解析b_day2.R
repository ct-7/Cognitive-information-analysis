#引数指定#
f1 = function(x){
 x^2 + 2*x
}
x.hist = x
v = 0
alpha = 0.2 #lamda
gamma = 0

###Momentumアルゴリズム
Momentum <- function(x,gamma,alpha){
  #while関数を使った方が良い
   v = gamma * v - alpha*(2*x + 2)
   x = x + u
   x.hist=c(x.hist,x)
  return(x.hist)
}


###数値微分
numerical_different = function(f, x){ 
  h = 1e-4
　plusH = do.call(f,list(x+h)) 
　minusH = do.call(f,list(x-h)) 
　num.diff = (plusH - minusH)/(2*h) 
　return(num.diff)
}
##目的関数
f2 = function(x){
  (x + 2)^5 - 50*(x + 2)^3 + 100*(x + 2)^2
}


##引数指定
f = f2
v = 0
x = 5
tol = 1e-7
alpha = 0.00002 #lamda
gamma = 0
x.history = x

###数値微分にMomentumを適用
Momentum.numerical_different<- function(f,x,gamma,alpha){
  #勾配（導関数）を初期化
  grad = 10
  
  #while関数を使った方が良い
  #導関数が0に極限に近くまで繰り返す
  while (abs(grad)>tol){  
   grad = numerical_different(f,x)
   v = gamma * v - alpha*grad
   x = x + v
   x.history = c(x.history,x)
  }
  lines(x.history,f2(x.history),type='o',pch=1,col='red',cex=1)
  return(x.history)
}
#関数実行
Momentum.numerical_different(f,x,gamma,alpha)


#数値微分
#引数指定
tol = 1e-7
grad = 1e10
x = 10
lr = 0.2
x.hist = x
numerical_gradient <- function(f,lr,x){
  
  while (abs(grad) > tol){
   grad = numerical_different(f,x)
   x = x - lambda*grad
   x.history = c(x.history,x) 
  }
  return(x.history)
}
#関数実行
numerical_gradient(f2,x,lr)

##plot
x.temp=seq(-10,10,0.1)
plot(x.temp, f2(x.temp),type='l',lwd=2)
lines(x.history,f2(x.history),type='o',pch=1,col='orange',cex=1)

