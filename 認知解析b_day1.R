string <- c("A","B","C")
for (i.str in string){print(i.str)}

N = 1000;
count6 = 0
for (i_loop in 1:N){
 die <- sample(1:6,1)
  if(die == 6){
   count6 = count6 + 1
  }
}
count6/N


N = 1000;
count6 = rep(0,N)
for (i_loop in 1:N){
  die <- sample(1:6,1)
  if(die == 6){
    count6[i_loop] =  1
  }
}
head(count6)

#累積和
P = cumsum(count6) /(1:N)
plot(P,type='l')
abline(h = 1/6,lwd = 2,col="red")

#loopを使わなくてもできる
die.all <- sample(1:6,N,replace = T)
sample.index <- die.all == 6


#%%であまりが計算できる
v1=1633
v2=355
r = 1
while (r != 0){
  r = v1%%v2
  if (r == 0){print(paste("GCD is v2"))}
  v1 = v2
  v2 = r
}

#functionは簡単
Func <- function(v1,v2){
  r=1
  while (r != 0){
    r = v1%%v2
    if (r == 0){print(paste("GCD is v2"))}
    v1 = v2
    v2 = r
  }
}
Func(1633,355)


####勾配法
f = x[1]^4 -16*x[1]^2 -5*x[1] + x[2]^4 -16*x[2]^2 -5*x[2]
x = c(0,0)
theta = c(0,4,-16,-5,1,-16,-6)


