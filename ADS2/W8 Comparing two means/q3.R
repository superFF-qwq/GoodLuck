#normative_score=pnorm(64,50,10)
#normative_score
Mean=50
Sd=10
a=array(c(64, 63, 62, 59))
#length(a)
b=array(c(70, 63, 61, 56))
for(i in 1:length(a))
  a[i]=pnorm(a[i],Mean,Sd)*100
#a
for(i in 1:length(b))
  b[i]=pnorm(b[i],Mean,Sd)*100
#b
Mean_a=sum(a)/length(a)
Mean_b=sum(b)/length(b)
Mean_a
Mean_b