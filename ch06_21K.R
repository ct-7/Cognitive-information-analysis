cu.nnet = function(train.x, train.y, OPT,net, HP = c(10,1000,0.05,0.01)){
  # HP: Hyperparameters
  # HP[1] = batch size
  # HP[2] = n iteration
  # HP[3] = learning rate
  # HP[4] = weiht decay
  n.layer <- length(net$W)
  loss = rep(0,HP[2]) #エラーに対応するloss
  A = list(); z = list(); Dact = list(); Daff = list()
  for (i.iter in 1:HP[2]){
    batch_mask = sample(1:nrow(train.x), HP[1])
    x.batch = train.x[batch_mask,]
    t.batch = train.y[batch_mask,]  
    for (i.layer in 1:n.layer){　#forward
      if (i.layer == 1){
        x = x.batch
      } else {
        x = z[[(i.layer - 1)]]
      }
      A[[i.layer]] = affine.forwd(x, net$W[[i.layer]], net$b[[i.layer]])
      z[[i.layer]] = activation(A[[i.layer]], t.batch, net$actFun[i.layer])
    }
    loss[i.iter] = z[[i.layer]]$ce
    smx = z[[i.layer]]$smx
    for (i.layerR in n.layer:1){　#bcwd
      if (i.layerR == n.layer){
        dout = 1
      } else {
        dout = Daff[[(i.layerR+1)]]$dx
      }
      Dact[[i.layerR]] = Dact(z[[i.layerR]], smx, t.batch, dout, net$actFun[i.layerR])
      if (i.layerR==1){
        x = x.batch
      } else {
        x = A[[(i.layerR-1)]]
      }
      Daff[[i.layerR]] = affine.bckwd(x, network$W[[i.layerR]], Dact[[i.layerR]])
    }
    for (i.layer in 1:n.layer){ 　#パラメータの更新
      net$W[[i.layer]] = net$W[[i.layer]] - HP[3]*Daff[[i.layer]]$dW - HP[4]*net$W[[i.layer]]
      net$b[[i.layer]] = net$b[[i.layer]] - HP[3]*Daff[[i.layer]]$db - HP[4]*net$b[[i.layer]]
    }
  }
  return(list(loss = loss, net = net))
}

init.network <- function(n.neurons, actFun, Opt, sdv){
  n.layer = length(n.neurons)
  W = list(); b = list(); 
  mW = list(); mb = list();    # momentum
  hW = list(); hb = list();    # adaGrad
  aMW = list(); aMb = list();  # adam M
  aVW = list(); aVb = list();  # adam V
  for (i.layer in 1:(n.layer-1)){
    if (nargs() == 3) {
      if (actFun[i.layer]=="sigmoid"){
        # Xavier
        sdv = 1/sqrt(n.neurons[i.layer])
      } else {
        # He - assumes ReLU
        sdv = sqrt(2/n.neurons[i.layer])
      }
    }
    W[[i.layer]] = matrix(rnorm(n.neurons[i.layer]*n.neurons[(i.layer+1)], sd = sdv),
                          nrow=n.neurons[i.layer])
    b[[i.layer]] =  matrix(rnorm(n.neurons[(i.layer+1)], sd = sdv), nrow = 1)
    if (Opt == "momentum"){
      mW[[i.layer]] = matrix(0, nrow=n.neurons[i.layer], ncol=n.neurons[(i.layer+1)])
      mb[[i.layer]] = matrix(0, nrow = 1, ncol=n.neurons[(i.layer+1)])
    } 
    if (Opt == "adagrad"){
      hW[[i.layer]] = matrix(0, nrow=n.neurons[i.layer], ncol=n.neurons[(i.layer+1)])
      hb[[i.layer]] = matrix(0, nrow = 1, ncol=n.neurons[(i.layer+1)])
    }
    if (Opt == "adam"){
      aMW[[i.layer]] = matrix(0, nrow=n.neurons[i.layer], ncol=n.neurons[(i.layer+1)])
      aMb[[i.layer]] = matrix(0, nrow = 1, ncol=n.neurons[(i.layer+1)])
      aVW[[i.layer]] = matrix(0, nrow=n.neurons[i.layer], ncol=n.neurons[(i.layer+1)])
      aVb[[i.layer]] = matrix(0, nrow = 1, ncol=n.neurons[(i.layer+1)])
    }
  }
  return(list(W = W, b = b, actFun = actFun, optimizer = Opt,
              mW=mW, mb=mb,
              hW=hW, hb=hb,
              aMW=aMW,aMb=aMb,aVW=aVW))
}


network <- init.network(c(784,50,10),"sigmoid","momentum",0.1)
OPT<-function(net, Daff, HP){
  if (net$optimizer == "momentum"){
    return(Opt.mom(net, Daff, HP))
  }
  if (net$optimizer == "adagrad"){
    return(Opt.adagrad(net, Daff, HP))
  }
  if (net$optimizer == "adam"){
    return(Opt.adam(net, Daff, HP))
  }
}

Opt.adam <- function(net, Daff, HP){
  # HP[3] = learning rate
  # HP[4] = weight decay
  # HP[5] = adagrad
  n.layer <- length(net$W)
  for (i.layer in 1:n.layer){
    net$mW[[i.layer]] = HP[5]*net$mW[[i.layer]] 
    - HP[3]*Daff[[i.layer]]$dW - HP[4]*net$W[[i.layer]]
    net$mb[[i.layer]] = HP[5]*net$mb[[i.layer]] 
    - HP[3]*Daff[[i.layer]]$db - HP[4]*net$b[[i.layer]]
    net$W[[i.layer]] = net$W[[i.layer]] + net$mW[[i.layer]]
    net$b[[i.layer]] = net$b[[i.layer]] + net$mb[[i.layer]] 
  }
  return(net=net)
}