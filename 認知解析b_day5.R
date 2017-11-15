#mutation
n.pop = 20
n.var = 10
set.seed(19)
nData = 50
nVar = 10
p.mute = 0.1
X = matrix(rnorm(nVar*nData,mean=10,sd=2),ncol=nVar)
P = matrix(sample(c(0,1),n.pop*n.var,replace = T),nrow =n.pop,ncol=n.var)
y = X%*%c(-2,0,2,0,2,0,-3,0,-2,0)+rnorm(nData,mean=0,sd=4)

mute.matrix = matrix(sample(c(0,1),n.pop*n.var,replace = T
                            ,prob=c(1-p.mute,p.mute)),
                           nrow=n.pop,ncol=n.var)
abs(P-mute.matrix)

#recombination 親の数だけ子供を作る
n.pop = nrow(P)
n.var = ncol(P)
child <- P
P2 <- P[sample(1:n.pop),]
recomb.idx = which(matrix(sample(c(0,1),n.pop*n.var,replace = T),nrow = n.pop)==1)
child[recomb.idx] = P2[recomb.idx]

child <- Ga.mute(child,0.1)


#survive
GA.survive <- function(fitP,fitC,P,child){
  sort.fit = sort(fitT,index.return = T)
  PC = rbind(P,child)
  P.new = PC[sort.fit$ix[1:n.pop],]
  return(P)
}

n.gen = 1000
for(i.gen in 1:n.gen){
  child = GA.recomb(P)
  
  
  fitP = rep(0,n.pop)
  fitC = rep(0,n.pop)
  i.c = 1
  
  for(i.c in 1:n.pop){
    temp.X = X[,P[i.c,]==1]
    fitP[i.c] = AIC(lm(y~temp.X))
    temp.X = X[,child[i.c,]==1]
    fitC[i.c] = AIC(lm(y~temp.X))
  }
  P = GA.survive(fitP,fitC,P,child)
}

combn(3,2)
