#ニューラルネットワークの学習
#学習＝訓練データから最適な重みパラメータの値を自動で獲得すること
#ニューラルネットワークの特徴はデータから学習できる点にある
#ニューラルネットワーク（ディープラーニング）は特徴量も機械が決める＝人が介入しない
#訓練データで学習を行い、最適なパラメータを見つけ、テストデータでその汎化能力を評価する
#ニューラルネットワークの学習で用いられる損失関数という指標
#値が最も小さくなる重みパラメータを探しだす
#損失関数は一般的に二乗和誤差や交差エントロピー誤差などが用いられる

#2乗和誤差
MSE <- function(target, y){ #yはsoftmax関数の出力
  return(0.5*sum((target-y)^2))
}

#クロスエントロピー誤差
cross.entropy = function(y, target){　#yはニューラルネットワークの出力
  delta = 1e-7;
  R = nrow(as.matrix(y))
  return(-sum(target*log(y + delta))/R)
}

#訓練データを使った学習＝訓練データに対する損失関数を求め、その値をできるだけ小さくするような値を探し出す
#ミニバッチ学習＝訓練データからある数だけを選び出して学習する

#微分＝ある瞬間の変化の量を表したもの
#数値微分
numerical.diff = function(func, x){
  h = 1e-4　#微小な値hを10の-4乗にして用いるとうまくいくらしい
  plusH = do.call(func,list(x+h))
  minusH = do.call(func,list(x-h))　#x+hからx-hとの差を中心差分といい誤差を減らすことができる
  num.diff = (plusH - minusH)/(2*h)
  return(num.diff)
}

func01 = function(x){
  return(0.01*x^2+0.1*x)
}
ND.5 = numerical.diff('func01',5)

x = seq(0,20,0.1)
y = func01(x)
plot(x,y,xlab ="x", ylab = "f(x)",type = "l",lwd =2)
ND.5 = numerical.diff('func01',5)
abline(a = func01(5)-ND.5*5, b = ND.5, col = 'red',  lwd =2)
abline(v = 5, lty = 2, col = 'red')
ND.10 = numerical.diff('func01',10)
abline(a = func01(10)-ND.10*10, b = ND.10, col = 'blue',lwd = 2)
abline(v = 10, lty = 2, col = 'blue')

#偏微分＝複数の変数からなる関数の微分
#変数が一つd毛の関数を定義して、その関数について微分を求める。
#ターゲットとする変数を一つに絞り、他の変数はある値に固定する
func02R = function(x){
  return(x[1]^2 + x[2]^2)
}
func02 = function(x0, x1){
  return(x0^2 + x1^2)
}
func02.x0 = function(x0){
  return(x0^2)
}
func02.x1 = function(x1){
  return(x1^2)
}

#全ての変数の偏微分をベクトルとしてまとめたものを勾配という
#多次元の関数は各変数による偏微分を並べた勾配が傾きに相当する
#勾配が示す方向は各場所に置いて関数の値を最も減らす方向
#勾配を求める自作関数
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

numerical.grad("func02R",matrix(c(3,4),nrow=1))
numerical.grad("func02R",matrix(c(0,4),nrow=1))
numerical.grad("func02R",matrix(c(3,0),nrow=1))
#勾配をうまく利用して関数の最小値を探すのが勾配法
#勾配が指す先が本当に関数の最小値なのかどうかは保証できないがその方向に進むことで関数の値を減らすことができる
#そのため、勾配の情報を手掛かりに進む方向を決めるべき
#勾配方法へ進むことを繰り返すことで関数の値を徐々に減らす＝勾配法
#勾配法は機械学習の最適化問題でよく使われる（特にニューラルネットワークの学習）

#ここではsoftmaxとクロスエントロピー誤差を利用する
w = matrix(c(0.47355232,0.85557411,0.9977393,0.03563661,0.84668094,0.69422093),nrow=2)
x = matrix(c(0.6, 0.9), nrow=1)　#入力データ
t = c(0,0,1)　#正解ラベルが入力される
#予測するためのメソッド
nn.predict <- function(w,x){
  return(x%*%w)
}

#preditfunction,softmax,crossentropyを入れないとうまくいかない
#損失関数の値を求めるメソッド
loss.func = function(w, x, t){　
  pred = nn.predict(w,x)
  y = softmax.func(pred)
  return(cross.entropy(y, t))
}

#任意に動かせる関数で勾配を求める
numerical.gradCE <- function(func, w, x, t){
  # input args
  # func: name of function
  # w   : weight
  # x   : input 
  # t   : target output
  ##############################################
  h = 1e-4
  R = nrow(w)
  C = ncol(w)
  grad = matrix(0, R, C)
  for (i.col in 1:C){
    for (i.row in 1:R){
      temp.w = w[i.row,i.col]
      w[i.row, i.col] = temp.w + h
      plusH = do.call(func, list(w,x,t))
      w[i.row, i.col] = temp.w - h
      minusH = do.call(func,list(w,x,t))
      grad[i.row, i.col] = (plusH - minusH)/(2*h)　#数値微分
      w[i.row, i.col] = temp.w
    }
  }
  return(grad)
}

numerical.gradCE(loss.func,w,x,t)
 0.2192476|0.1435624|-0.362810 
 #w11をhだけ増やすと損失関数の値は0.2だけ増加するという見方
 0.3288714|0.2153436|-0.544215　
 #損失関数を減らすという観点からはw11はマイナス方向へ更新すると良いことがわかる

#確率的勾配降下法＝SGD
#ニューラルネットワークの学習に関する基本的知識は損失関数・ミニバッチ・勾配・勾配降下法
#前提＝重みとバイアスを訓練データに適応するように調整することを学習とよぶ
#学習は４つの手順で行う
#1訓練データの中からランダムに一部のデータを選ぶ。ミニバッチの損失関数の値を減らすことを目的とする
#2勾配の算出＝損失関数の値を最も減らす方向を示す
#3パラメータの更新＝重みパラメータを勾配方向に微小量だけ更新する
#1~3を繰り返す

#パラメータの初期化
init.2LN <- function(n.input, n.hidden, n.output, w.std = 0.01){
  W1 = matrix(rnorm(n.input*n.hidden,0,w.std),nrow = n.input)
  B1 = matrix(rnorm(n.hidden,0,w.std),nrow =1)
  W2 = matrix(rnorm(n.hidden*n.output,0,w.std),nrow = n.hidden)
  B2 = matrix(rnorm(n.output,0,w.std),nrow =1)
  return(list(W1 = W1, B1 = B1, W2 = W2, B2 = B2))
}
softmax.2LN <- function(x){
  max.x = apply(x,1,max)
  C = ncol(x)
  x = x - max.x%*%matrix(1,nrow=1,ncol=C)
  return(exp(x)/rowSums(exp(x)))
}
sigmoid.func <- function(x){
  return(1/(1+exp(-x)))
}

#推論を行う
# params: list of parameters (W & b)
pred.2LN <- function(params, x){
  NR = nrow(x)
  a1 = x%*%params$W1 + matrix(1,nrow = NR)%*%params$B1
  z1 = sigmoid.func(a1)　
  a2 = z1%*%params$W2 + matrix(1,nrow = NR)%*%params$B2
  y = softmax.2LN(a2)　#一番高い確率のものが正解ラベル
  return(y)
}

#x=テストデータ　t=訓練データ
loss.2LN = function(params, x, t){
  y = pred.2LN(params,x)
  return(cross.entropy(y, t))
}　#訓練データとテストデータの誤差を出す
