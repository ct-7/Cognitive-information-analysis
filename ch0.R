#x^4-16x^2-5x+y^4-16y^2-5y最小値
funZ <- function(x){
  x[1]^4-16*x[1]^2-5*x[1]+x[2]^4-16*x[2]^2-5*x[2]
}
h = 1e-4
temp.x = x[1]
temp.plus = x[1]+h
temp.minus =x[1]-h
plusH = do.call(funZ,list(x))
minusH = do.call(funZ,list(x))

#勾配法
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
