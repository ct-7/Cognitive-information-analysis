#PERCEPTRON
#入出力を備えたアルゴリズム・重みとバイアスをパラメータとして設定する
#多層のパーセプトロンで非線形領域を表現できる
#重みを手作業で決めなければいけないデメリット
AND.gate <- function(x1, x2){
  w1 = 0.5
  w2 = 0.5
  theta = 0.7
  y.temp = w1*x1 + w2*x2
  if (y.temp <= theta){
    y = 0
  } else {
    y = 1
  }
  return(y)
}

AND.gate <- function(x1, x2){
  w1 = 0.5; w2 = 0.5; theta = 0.7
  return(as.numeric(w1*x1 + w2*x2 > theta))
}

NAND.gate <- function(x1, x2){
  w1 = -0.5; w2 = -0.5; theta = -0.7
  return(as.numeric(w1*x1 + w2*x2 > theta))
}

OR.gate <- function(x1, x2){
  w1 = 0.5; w2 = 0.5; theta = 0.3
  return(as.numeric(w1*x1 + w2*x2 > theta))
}

# with BIAS (b)
AND.gate <- function(x1, x2){
  w1 = 0.5
  w2 = 0.5
  b = -0.7
  y.temp = w1*x1 + w2*x2 + b
  if (y.temp <= 0){
    y = 0
  } else {
    y = 1
  }
  return(y)
}

AND.gate <- function(x1, x2){
  w1 = 0.5; w2 = 0.5; b = -0.7
  return(as.numeric(w1*x1 + w2*x2 + b > 0))
}

NAND.gate <- function(x1, x2){
  w1 = -0.5; w2 = -0.5; b = 0.7
  return(as.numeric(w1*x1 + w2*x2 + b > 0))
}

OR.gate <- function(x1, x2){
  w1 = 0.5; w2 = 0.5; b = -0.3
  return(as.numeric(w1*x1 + w2*x2 + b > 0))
}

NOR.gate <- function(x1, x2){
  w1 = -0.5; w2 = -0.5; b = 0.3
  return(as.numeric(w1*x1 + w2*x2 + b > 0))
}

plot.logic <- function(logic.oper){
  x1 = c(0,0,1,1);
  x2 = c(0,1,0,1);
  if (logic.oper == "and") {
    w1 = 0.5; w2 = 0.5; theta = 0.7;
    true.point = AND.gate(x1,x2)
  } else if (logic.oper == "or") {
    w1 = 0.5; w2 = 0.5; theta = 0.3;
    true.point = OR.gate(x1,x2)
  } else if (logic.oper == "nand") {
    w1 = -0.5; w2 = -0.5; theta = -0.7;
    true.point = NAND.gate(x1,x2)
  } else if (logic.oper == "nor"){
    w1 = -0.5; w2 = -0.5; theta = -0.3;
    true.point = NOR.gate(x1,x2)
  } else {warning("incompatible operator");stop() }
  plot(c(0,0,1,1),c(0,1,0,1),xlim = c(-0.5, 1.5), ylim = c(-0.5, 1.5), 
       pch = 20, cex= 2, col = true.point+1)
  abline(a = theta/w1, b = -w1/w2, lwd = 3)
}   

XOR.gate <- function(x1, x2){
  gate1 <- NAND.gate(x1,x2)
  gate2 <- OR.gate(x1,x2)
  y <- AND.gate(gate1,gate2)
  return(y)
}

plot.XOR <- function(){
  x1 = c(0,0,1,1);
  x2 = c(0,1,0,1);
  w11 = -0.5; w21 = -0.5; theta1 = -0.7
  w12 = 0.5; w22 = 0.5; theta2 = 0.3
  true.point = XOR.gate(x1, x2)
  plot(c(0,0,1,1),c(0,1,0,1),xlim = c(-0.5, 1.5), ylim = c(-0.5, 1.5), 
       pch = 20, cex= 2, col = true.point+1)
  abline(a = theta1/w11, b = -w11/w21, lwd = 3)
  abline(a = theta2/w12, b = -w12/w22, lwd = 3)    
}  