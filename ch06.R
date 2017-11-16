#forwd,bckwdの関数理解・irisのプロスペクションができるようなネットワークを作る
#4章でやった2層のネットワークをirisで分析する　bckwdでやってみる
#generalでやる任意の層でできるように変更する

#関数定義
activation <- function(A, actFun){
  if (actFun == "sigmoid"){
    return(sigmoid.func(A))
  }
  if (actFun == "relu"){
    return(relu.func(A))
  }
  if (actFun == "softmax"){
    return(softmax(A))
  }
}
#推論処理
#重みの初期化
#層がどれだけあっても対応できる・汎用化
init.network <- function(n.neurons){  #n.neurons = c(n.inputデータ数, n.hiddenの数, n.output(分類の数))
  n.layer = length(n.neurons)
  W = list(); b = list()
  for (i.layer in 1:(n.layer-1)){
    W[[i.layer]] = matrix(rnorm(n.neurons[i.layer]*n.neurons[(i.layer+1)],sd = 0.1), #rnormで正規乱数を生成・標準偏差1
                          nrow=n.neurons[i.layer])
    b[[i.layer]] =  matrix(rnorm(n.neurons[(i.layer+1)],sd = 0.1), nrow = 1)
  }
  return(list(W = W,b = b)) #W=list[1],b=list[2]とするリストを作成
}


#順伝播汎用化
feedforward <- function(network, x, actFun) {
  n.layer <- length(network$W)
  batch.size = nrow(x)
  for (i.layer in 1:n.layer){
    A = x%*%network$W[[i.layer]] 
    + matrix(1,nrow=batch.size,ncol = 1)%*%network$b[[i.layer]]
    x = activation(A, actFun[i.layer])
  }
  return(x)
}

## example using IRIS data set
#ここで入力データxと正解ラベルyを分ける
train.x = as.matrix(iris[,1:4])
train.y.temp = as.numeric(iris[,5])　#分類したい名前を数字にする１、２、３のように
train.y = matrix(0,nrow = nrow(train.x), ncol =3)　#正解ラベル
train.y[which(train.y.temp==1), 1]=1　#１の数字の時に１その値にを代入し、train.yを書き換える
train.y[which(train.y.temp==2), 2]=1
train.y[which(train.y.temp==3), 3]=1

params = init.network(c(4,15,3))　#重み変数を格納　irisは３つを分類するもの
batch_size = 10 #150
n.iter = 5000 #繰り返しの回数
lambda = 0.05　#lambdaとは 
n.train = nrow(train.x)
loss = rep(0,n.iter)　#損失関数の値を初期化
for (i.iter in 1:n.iter){
  batch_mask = sample(1:n.train, batch_size)　#5000個のデータから10個を取り出す
  x.batch = train.x[batch_mask,] #入力データを格納
  t.batch = train.y[batch_mask,] #正解ラベルを格納
  
  a1 = affine.forwd(x.batch,params$W[[1]],params$b[[1]])
  z1 = sigmoid.forwd(a1)
  a2 = affine.forwd(z1,params$W[[2]],params$b[[2]])
  z2 = softmax.forwd(a2,t.batch)
  
  dwSM = softmax.bckwd(a2, t.batch, 1)
  dwA2 = affine.bckwd(a1,params$W[[2]],params$b[[2]],dwSM)
  dwSG = sigmoid.bckwd(a1,dwA2$dx)
  dwA1 = affine.bckwd(x.batch,params$W[[1]],params$b[[1]],dwSG)
  params$W[[2]] = params$W[[2]] - lambda*dwA2$dW
  params$b[[2]] = params$b[[2]] - lambda*dwA2$db
  params$W[[1]] = params$W[[1]] - lambda*dwA1$dW
  params$b[[1]] = params$b[[1]] - lambda*dwA1$db
  loss[i.iter] = loss.network(params,x.batch,t.batch,c("sigmoid","softmax"))
}
plot(loss,type='l', xlab = "trial")


