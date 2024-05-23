population=rnorm(1e6,100,5)
work=function(n){ # create a sampling distribution of size n
  a=sample(population,size=n)
  1.0*sum(a)/n;
}
calc=function(times,n){
  a=array(dim=0)
  sum=0
  for(i in 1:times){
    x=work(n)
    a[i]=x
    sum=sum+x
  }
  mn=1.0*sum/times
  sd=0.0
  for(i in 1:times)
    sd=sd+(a[i]-mn)*(a[i]-mn)
  sqrt(sd/times)
}
main=function(){
  l=5
  r=100
  times=1000
  a=array(dim=0)
  for(n in l:r)
    a[length(a)+1]=calc(times,n)
  print(a)
  png("fig1.png")
  plot(l:r,a,ylim=c(0.0,3.0),xlab="sample size",
       ylab="SD of sampling distribution")
  dev.off()
}
main()