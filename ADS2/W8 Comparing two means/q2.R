times=100
simulate_class=function(n,fullmark){
  class=runif(n,0,fullmark)
  mean1=sum(class)/length(class)
  #print(c)
  mean1
}
tot2=0
a2=array(dim=times)
b2=array(dim=times)
for(i in 1:times){
  a2[i]=simulate_class(26,80)
  b2[i]=simulate_class(26,100)
  if(a2[i]>b2[i])
    tot2=tot2+1
}
tot2
#a2
#b2
p2=1.0*tot2/times
p2
#a2
#b2
png(file="q2.png")
figa=hist(a2)
figb=hist(b2)
plot(figa,col=rgb(0,0,1,1/4))
plot(figb,col=rgb(1,0,0,1/4),add=T)
dev.off()

#result:p2>0.05
#one-tailed H0:u1<u2 or u1>u2
#two-tailed H0:u1=u2