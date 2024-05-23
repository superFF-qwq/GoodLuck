#a=rnorm(100000,40,8)
#a
#for(i in 1:length(a))
#  a[i]=pnorm(a[i],50,10)*100
#print(a)
#sum(a)/length(a)
#png("q4.png")
#hist(a)
#dev.off()
simulate_class=function(n,fullmark){
  class=runif(n,0,fullmark)
  for(i in 1:length(class))
      class[i]=pnorm(class[i],50,10)*100 #normative
  mean1=sum(class)/length(class)
  #print(c)
  mean1
}
times=1000
a2=array(dim=times)
b2=array(dim=times)
tot=0
for(i in 1:times){
  a2[i]=simulate_class(26,80)
  b2[i]=simulate_class(26,100)
  if(a2[i]>b2[i])
    tot=tot+1
}
p=tot/times
p
