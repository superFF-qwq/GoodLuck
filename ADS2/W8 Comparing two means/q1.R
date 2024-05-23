a=c(17,17.5,16,16.4,18.9,18.3,18.6,20,15.5,18.1)
#sum(a)
n=length(a)
#n
Mean=sum(a)/length(a)
#Mean
sd=0
for(i in 1:n)
  sd=sd+(a[i]-Mean)*(a[i]-Mean)
sd=sqrt(sd/(n-1))
#sd
#pnorm(a[1],Mean,sd)

times=1000
#print(length(c))
simulate_class=function(n,fullmark){
  class=runif(n,0,fullmark)
  mean1=sum(class)/length(class)
  #print(c)
  mean1
}

tot1=0
for(i in 1:times){
  c[i]=simulate_class(26,100)
  if(c[i]<40)
    tot=tot+1
}
p1=1.0*tot/times
p1
#c
#c=sort(c)
#c
png(file="p1.png")
hist(c)
dev.off()

#one-tailed
#H0: mean<constant or mean>constant
#two-tailed
#mean=constant
#本题的H0:mean<40 (one-tailed)
#算出来的p(probablity)要查表找到对应的p-value
#so we do not reject H0
#so there is sufficient evidence that mean<40 (H0 is true)