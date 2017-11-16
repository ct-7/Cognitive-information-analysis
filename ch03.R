#Newral Network(多層パーセプトロン)
#適切な重みパラメータをデータから自動で学習できる

#活性化関数　step関数　sigmoid関数　relu関数
#入力信号の総和を出力信号に変換する関数
#入力信号の総和がどのように活性化するかということを決定する役割がある
#活性化関数をstep(階段)関数から別の関数に変更することでニューラルネットワークが作れる
#シグモイド関数は連続的な実数の値を返す
#シグモイド関数は水車のように流れてきた水の量に比例して、次へ流す水の量を調節する
step.func <- function(x){
  return(as.numeric(x > 0)) #TRUE,FALSEを数値で返す
}
x = seq(-5, 5, 0.1)　#-5から5まで0.1間隔で返す
y = step.func(x)
plot(x,y, ylab = 'y', xlab = 'a', type ="l", lwd =2)

sigmoid.func <- function(x){
  return(1/(1+exp(-x)))
}

y = sigmoid.func(x)
plot(x,y, ylab = 'y', xlab = 'a', type ="l", lwd =2)

y.step = step.func(x)
y.sigm = sigmoid.func(x)
plot(x,y.sigm, ylab = 'y', xlab = 'a', type ="l", lwd =2)
lines(x,y.sigm, lwd =2, lty = 2)

relu.func <- function(x){
  return(pmax(0,x)) #大きい方を返す関数
}
y.relu= relu.func(x)
plot(x,y.relu, ylab = 'y', xlab = 'a', type ="l", lwd =2)

#ニューラルネットワークの計算は行列の計算としてまとめて行える
#ただし、多次元配列の積では、２つの行列で対応する次元の要素数を一致させる必要がある
#行列の積
A = matrix(1:4, nrow = 2, byrow = T)
B = matrix(5:8, nrow = 2, byrow = T)
A%*%B
A = matrix(1:6, nrow = 3, byrow = T)
B= matrix(7:8, nrow = 2, byrow = T)
A%*%B

X <- c(1,0.5)
W1 <- matrix((1:6)*0.1,nrow = 2) #matrix(0.1:0.6)だと行列がうまく作れない
B1 <- c(0.1,0.2,0.3)
A1 = X%*%W1 + B1
Z1 = sigmoid.func(A1)
W2 <- matrix((1:6)*0.1,nrow = 3)
B2 <- c(0.1,0.2)
A2 = Z1%*%W2 + B2
Z2 = sigmoid.func(A2)
W3 <- matrix((1:4)*0.1,nrow = 2)
B3 <- c(0.1,0.2)
A3 = Z2%*%W3 + B3
Z3 = sigmoid.func(A3)

#3層のニューラルネットワーク
# function to initialize 3L network
init.3L.network <- function(){
  W1 = matrix((1:6)*0.1, nrow = 2)
  B1 = (1:3)*0.1
  W2 = matrix((1:6)*0.1, nrow = 3)
  B2 = c(0.1, 0.2)
  W3 = matrix((1:4)*0.1, nrow = 2)
  B3 = c(0.1, 0.2)
  return(list(W1 = W1, B1 = B1, W2 = W2, B2 = B2, W3 = W3, B3 = B3))　#wとbの値を返す＝初期化
}
# feedforward process
#各ラベルの確率を計算する
forward.3L <- function(network, x){
  A1 = x%*%network$W1 + network$B1
  Z1 = sigmoid.func(A1)
  A2 = Z1%*%network$W2 + network$B2
  Z2 = sigmoid.func(A2)
  A3 = Z2%*%network$W3 + network$B3
  Z3 = sigmoid.func(A3)
  A3 = Z3
  return(A3)
}
#入力信号が出力信号へと変換されるプロセス関数

network<-init.3L.network()
y = forward.3L(network, c(1, 0.5)) #出力を表示

#出力層の設計
#ニューラルネットワークは分類問題と回帰問題の両方に用いることができるが
#どちらの問題に用いるかで出力層の活性化関数を変更する必要がある
#回帰問題では恒等関数、分類問題ではソフトマックス関数を使う
a = c(1010,1000,990)
exp(a)/sum(exp(a))

#softmax関数
softmax.func <- function(x){
  max.x = max(x)
  return(exp(x-max.x)/sum(exp(x-max.x))) #x-max.xで何をやっているか？　
}
softmax.func(a)　#aとyの最大値は一緒だが一番値が大きいのが確率的に高いということになる

#機械学習の問題を解く手順は学習と推論の二つのフェーズに分けられる
#最初に学習フェーズでモデルの学習を行い、推論フェーズで学習したモデルを使って未知のデータ
#に対して推論（分類）を行う
#ソフトマックス関数の出力は０から１の間の実数になる
#また、ソフトマックス関数の総和は１になる。これらからソフトマックス関数の出力を確率として解釈できる
#ソフトマックス関数を用いることで問題に対して確率的な対応ができるようになる
#推論処理は、ニューラルネットワークの順方向伝播ともいう
#分類問題では、出力層のニューロンの数を分類するクラス数に設定する

train <- read.csv('http://peach.l.chiba-u.ac.jp/course_folder/MNSTtrain.csv', header=TRUE)
train <- data.matrix(train)
train.x <- train[,-1]
train.y <- train[,1]
train.x <- t(train.x/255)
download.file("http://peach.l.chiba-u.ac.jp/course_folder/trNetwork.Rdata","trNetwork.Rdata")
load("trNetwork.Rdata")
network=trNetwork

n.train = ncol(train.x)
correct.cl = 0
conf.matrix = matrix(0,10,10)
for (i.loop in 1:n.train){
  y = forward.3L(network,train.x[,i.loop])
  max.y = max.col(y)
  conf.matrix[max.y, (train.y[i.loop]+1)] = conf.matrix[max.y, (train.y[i.loop]+1)] + 1
}
accuracy = sum(diag(conf.matrix))/n.train　#分類の精度

#入力データの集まりをバッチという
#batch処理 高速に効率よく処理することができる
batch_size = 200 #バッチの数
conf.matrix = matrix(0,10,10)
for (i.loop in seq(1,n.train,batch_size)){　#seq
  y = forward.3L(network,train.x[,(i.batch:(i.batch+batch_size-1))])
  pred = max.col(y) 　#行で一番高い値を出す
  conf.matrix = conf.matrix+table(pred,(train.y[i.batch:(i.batch+batch_size-1)]+1))
}
accuracy = sum(diag(conf.matrix))/n.train