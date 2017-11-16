#民族間の移動課題
soc.size=10
n.circle=10
n.sharp = 10
r.sample=sample(soc.size^2)
society =matrix(0,soc.size,soc.size)
society[r.sample[1:n.circle]] = 1
society[r.sample[(n.circle+1):(n.circle+n.sharp)]] = 2


#finding circle
c.pos = which(society==1,arr.ind = T)
for(i.c in 1:n.circle){
  r.idx = c(max(1,c.pos[i.c,1]-1),min(soc.size,c.pos[i.c,1]+1))
  c.idx = c(max(1,c.pos[i.c,2]-1),min(soc.size,c.pos[i.c,2]+1))
  neighbar = society[r.idx[1]:r.idx[2],c.idx[1]:c.idx[2]]
  neighbar.c = sum(neighbar == 1) - 1
  if (neighbar.c < 3){
    move.idx = which(society==0)
    society[sample(move.idx,1)] = 1
    society[c.pos[i.c,1],c.pos[i.c,2]] = 0
  }
}

#finding sharp
s.pos = which(society==2,arr.ind = T)
for (i.s in 1:n.sharp){
  r.idx = c(max(1,c.pos[i.s,1]-1),min(soc.size,c.pos[i.s,1]+1))
  c.idx = c(max(1,c.pos[i.s,2]-1),min(soc.size,c.pos[i.s,2]+1))
  neighbar = society[r.idx[1]:r.idx[2],c.idx[1]:c.idx[2]]
  neighbar.s = sum(neighbar == 2) - 1
  neighbar.all = sum(neighbar !=0) - 1
  prop.s = min(0,neighbar.s/neighbar.all,na.rm = T)
  if (prop.s < 1/3){
    move.idx = which(society==0)
    society[sample(move.idx,1)] = 2
    society[s.pos[i.s,1],s.pos[i.s,2]] = 0
  }
}

