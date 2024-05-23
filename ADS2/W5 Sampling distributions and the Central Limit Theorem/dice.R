dice=function(){
  sample(1:6,size=1)
}
main=function(){
  times=1000
  a=array(dim=0)
  for(i in 1:times)
    a[length(a)+1]=dice()
  #hist(a,breaks=0.5:6.5)
  b=array(dim=0)
  for(i in 1:times)
    b[length(b)+1]=dice()+dice()
  hist(b,breaks=0.5:12.5)
}
main()