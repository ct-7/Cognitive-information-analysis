###########課題############
##コスト関数
fZ_xy <- function(x,y){
  x^4 - 16*x^2 -5*x + y^4 -16*y^2 - 5*y
}
#Zy = function(y){y^4 -16*y^2 - 5*y}
#Zx = function(x){x^4 - 16*x - 5*x}
#変数をxのリストにする
fZ = function(x){
  x[1]^4 - 16*x[1]^2 -5*x[1] + x[2]^4 -16*x[2]^2 - 5*x[2]
}

##引数指定
#xにベクトルを入れるだけで行列を作る関数
x_F <- function(x){
  x = matrix(x,nrow = 1,ncol = 2)
  return(x)
} 
step_num = 100
alpha = 0.2 #lamda lr

##多変数の数値微分による最適化
##数値勾配
numerical_gradient <- function(func,x){
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
x(c(-1,1))
numerical_gradient(fZ,x)



#引数指定
x = x(c(-1,1))
step_num = 100
alpha = 0.02
x.history = x
grad = 0
f = fZ
##勾配降下法
gradient_descent <- function(f,x,alpha,step_num){
  for (i in 1:step_num){
    grad = numerical_gradient(f,x)
    x = x - alpha*grad
    x.history = rbind(x.history,x) 
  }
  
  return(x.history)
}
#関数実行
head(gradient_descent(fZ,x,alpha,1000))
gd1 = gradient_descent(fZ,x,alpha,100)
gd2 = gradient_descent(fZ,x(c(-2,-2)),alpha,100)
gd3 = gradient_descent(fZ,x(c(1,-1)),alpha,100)
gd4 = gradient_descent(fZ,x(c(2,2)),alpha,100)

###三次元プロット
xmin = -5;xmax = 5;n_len = 100;
x.temp <- seq(xmin,xmax, length = n_len)
y.temp <- x.temp
z <- outer(x.temp,y.temp,fZ_xy);
# persp(x.temp,y.temp,z,theta = 30, phi = 30, expand = 0.5, col = rainbow(50), border=NA)
# 三次元プロット
contour(x.temp,y.temp,z,drawlabels = F)　#等高線
points(gd1,col = 'red',pch=20,cex=1.2,type="o")
points(gd2,col = 'orange',pch=20,cex=1.2,type="o")
points(gd3,col = 'green',pch=20,cex=1.2,type="o")
points(gd4,col = 'blue',pch=20,cex=1.2,type="o")
#lines(gd4,col = "black")と同じこと

###聞く
optim(par=c(-1,3), Z(x,y))

##############授業##############
#焼きなまし法
optimization <- function(x,f){
  ransuu = matrix(rnorm(col(x)),nrow=nrow(x),ncol=ncol(x))
  new.x = matrix(0,nrow=nrow(x))
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
     new.x[i,j] = x[i,j] + ransuu[i,j]
    }
  }
  return(f(new.x))
}
optimization(x,fZ)


C=1;T=1;
for (i in 2:1000){
 new.x = x + rnorm(1,0,sd = 0.1)
 delta.f = fZ(new.x) - fZ(x)
 eta = 0.99
 T = eta*T
 prob = 1/(1+exp(delta.f/eta))

 if (runif(1) < prob){
  x = new.x
  x.history = rbind(x.history,x)
 }
}

#switching関数
v = 1:10;k=2;w =3;
switching <- function(v,k,w){
  tmp.v = v[k]
  v[k] = v[w]
  v[w] = tmp.v
  return(v)
}
switching(v,3,1)


#不完全なtranslation
translation <- function(v,k,w){
  tmp.1 = v[1]
  tmp.v = v[w]
  v[w] = v[k]
  if(k<w){
    for(i in 1:(w-k)){
      v[i] = v[i+1]
    }
  }
  v[1] = tmp.1
  v[w-1] = tmp.v
  return(v)
}



v = 1:10
v[2:5]
tmp.v[k] = v[k]
tmp.v[k+1] = v[k+1]
tmp.v[k+2] = v[k+2]
tmp.v[k+3] = v[k+3]
v[w]=v[k]
v[w-1] =v[k+1]
v[w-2] =v[k+2]
v[w-3] = v[k+3]
#不完全なInversion
Inversion <- function(v,k,w){
  
}





numerical_gradient <- function(func,x){
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
lapply(x,ff,f)
ff <- function(x,f){
  tmp.x = x
  
  x = tmp.x + h 
}
x =x_F(c(-1,1))
grad = matrix(0, nrow(x), ncol(x))
tmp.x = x
x = tmp.x + h
h = 1e-4
plusH = do.call(Z,list(x))
x = tmp.x - h
minusH = do.call(Z,list(x))
grad = (plusH - minusH)/(2*h)
x = tmp.x


}
}
