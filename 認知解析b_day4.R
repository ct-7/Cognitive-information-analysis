temp.x = seq(0,3,0.1)
temp.y = 0.3*x^2 - 1
plot(x,y,col="red",cex=0.5,pch = 20,xaxt = 'n',yaxt ='n',xlab = "",ylab = "")
par(new=T) 

sens.loc.X1 = sort(rep(seq(0,3,1),7))
sens.loc.X2 = sort(rep(seq(0.5,2.5,1),6))
sens.loc.Y1 = rep(seq(-1.5,1.5,0.5),4)
sens.loc.Y2 = rep(seq(-1.25,1.25,0.5),3)

route = cbind(temp.x,temp.y)
sens.loc = cbind(c(sens.loc.X1,sens.loc.X2),c(sens.loc.Y1,sens.loc.Y2))
plot(sens.loc)

#初期化
x.matrix = matrix(0,nrow = 32,ncol = 32)
n.data = nrow(route)
n.sens = nrow(sens.loc)

alpha = 2
for (i_loop in 1:46){
  d = as.matrix(dist(rbind(route,sens.loc[i_loop,])))
  x.matrix[i_loop,] = 1/(exp(alpha*d[n.data,1:(n.data-1)]^2))
}

d = as.matrix(dist(rbind(route,sens.loc[1,])))
1/exp(d[6,1:5]^2)
