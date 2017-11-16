init.network <- function(n.neurons, actFun, Opt, sdv){
  n.layer = length(n.neurons)
  W = list(); b = list(); 
  mW = list(); mb = list();    # momentum
  hW = list(); hb = list();    # adaGrad
  aMW = list(); aMb = list();  # adam m
  aVW = list(); aVb = list();  # adam v
  MW = list(); Mb = list();    # adam M
  VW = list(); Vb = list();    # adam V
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
      MW[[i.layer]] = matrix(0, nrow=n.neurons[i.layer], ncol=n.neurons[(i.layer+1)])
      Mb[[i.layer]] = matrix(0, nrow = 1, ncol=n.neurons[(i.layer+1)])
      VW[[i.layer]] = matrix(0, nrow=n.neurons[i.layer], ncol=n.neurons[(i.layer+1)])
      Vb[[i.layer]] = matrix(0, nrow = 1, ncol=n.neurons[(i.layer+1)])
    }
  }
  return(list(W = W, b = b, actFun = actFun, optimizer = Opt,
              mW=mW, mb=mb,
              hW=hW, hb=hb,
              aMW=aMW,aMb=aMb,aVW=aVW,aVb=aVb,
              MW=MW,Mb=Mb,VW=VW,Vb=Vb))
}

Opt.mom <- function(net, Daff, HP){
  # HP[3] = learning rate
  # HP[4] = weight decay
  # HP[5] = momentum
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

Opt.adagrad <- function(net,Daff,HP){
  # HP[3] = learning rate
  # HP[4] = weight decay
  # HP[6] = adagrad = 0
  n.layer <- length(net$W)
  for (i.layer in 1:n.layer){
    net$hW[[i.layer]] = HP[6] + Daff[[i.layer]]$dW*Daff[[i.layer]]$dW - HP[4]*net$W[[i.layer]]
    net$hb[[i.layer]] = HP[6] + Daff[[i.layer]]$db*Daff[[i.layer]]$db - HP[4]*net$b[[i.layer]]
    net$W[[i.layer]] = net$W[[i.layer]] - HP[3]*(1/sqrt( net$hW[[i.layer]]))*Daff[[i.layer]]$dW
    net$b[[i.layer]] = net$b[[i.layer]] - HP[3]*(1/sqrt( net$hb[[i.layer]]))*Daff[[i.layer]]$db
  }
  return(net=net)
}

Opt.adam <- function(net,Daff,HP){
  # HP[3] = learning rate
  # HP[4] = weight decay
  # HP[7] = beta1 (adam)
  # HP[8] = beta2 (adam)
  n.layer <- length(net$W)
  e = 1e-7
  for (i.layer in 1:n.layer){
    aMW[[i.layer]] = HP[7]*aMW[[i.layer]] + (1-HP[7])*Daff[[i.layer]]$dW
    aMb[[i.layer]] = HP[7]*aMb[[i.layer]] + (1-HP[7])*Daff[[i.layer]]$db
    aVW[[i.layer]] = HP[8]*aVW[[i.layer]] + (1-HP[8])*Daff[[i.layer]]$dW*Daff[[i.layer]]$dW
    aVb[[i.layer]] = HP[8]*aVb[[i.layer]] + (1-HP[8])*Daff[[i.layer]]$db*Daff[[i.layer]]$db
    MW[[i.layer]] = aMW[[i.layer]]/(1-HP[7])
    Mb[[i.layer]] = aMb[[i.layer]]/(1-HP[7])
    VW[[i.layer]] = aVW[[i.layer]]/(1-HP[8])
    Vb[[i.layer]] = aVb[[i.layer]]/(1-HP[8])
    net$W[[i.layer]] = net$W[[i.layer]] - (HP[3]/sqrt(VW[[i.layer]]+e)*MW[[i.layer]]
    net$b[[i.layer]] = net$b[[i.layer]] - (HP[3]/sqrt(Vb[[i.layer]]+e)*Mb[[i.layer]]
  }
  return(net=net)
}

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

network=init.network(c(780,15,3),"sigmoid","momentum",0.1)
OPT(network,Daff,HP = c(10,1000,0.05,0.01,0.1,0.999,0.9))
