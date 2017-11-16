#ビンゴにかかる時間
n.time<-function(size){
  rand.v=sample(1:size^2)
  bingo=matrix(rand.v,size)
  rand.call=sample(1:size^2)
  terminate=F;counter=0
  while(terminate==F){
    counter=counter+1
    bingo[which(bingo==rand.call[counter])]=0
    Rsum = rowSums(bingo)
    Csum = colSums(bingo)
    Dsum = c(sum(diag(bingo)),sum(diag(bingo[,size:1])))
    if (any(c(Rsum,Csum,Dsum)==0)) {terminate = T}
  }
  print(counter)
}


#n回の出る平均
means<-matrix(0,100)
for(i_loop in 1:100){
   nTime<-n.time(1)
   means[i_loop]<-mean(nTime)
}
Means<-colMeans(means)

#可視化
Means<-colMeans(means)
hist(means,100,probability = T)
dens<-density(means)
lines(dens,col='skyblue',lwd=3)
sizeMean<-(Means)
plot(size,time,xlab="size of game",ylab="Average duration of game")


#bingoGame
N = 5
bingo <- matrix(sample(1:N^2),nrow = N)
bingo
ball <- sample(1:N^2)
terminate = 0
counter = 0
kekka = matrix(rep(1,N^2),nrow = N)
idx = which(bingo==ball[i.roop])
diag(bingo)
sum(diag(bingo))
bingo[,N:1]
diag(bingo[,N:1])
while(terminate==F){
  counter = counter+1
  idx =which(bingo==ball[counter])
  cs = colSums(kekka)
  rs = rowSums(kekka)
  ds = c(sum(diag(kekka)),diag(kekka[,N:1]))
  if(any(cs,rs,ds)==0){
    terminate = T
  }
}