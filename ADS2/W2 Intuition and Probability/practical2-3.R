#month=list(0,31,28,31,30,31,30,31,31,30,31,30,31)
#sum(month)
# getbirth=function(day){
#   for(i in 1:12)
#       if(day>month[i])
#         day=day-month[i]
#       else
#         list(i,day)
# }
isshared=function(n){
  data=sample(1:365,size=n,replace=T)
  #print(data)
  if(length(data)!=length(unique(data)))
    return(TRUE)
  else
    return(FALSE)
}
# birth=sample(1:365,size=26,replace=TRUE)
# if(length(birth)!=length(unique(birth)))
#   print("There are shared birthdays.")
# else
#   print("There is not a shared birthday.")
calcp=function(n){
  tot=0
  times=1000
  for(i in 1:times)
    if(isshared(n))
      tot=tot+1
  #print(n)
  #print(tot)
  return(1.0*tot/times)
}
main=function(){
  x=1:50
  y=numeric()
  for(n in 1:50)
    y=c(y,calcp(n))
  jpeg("plot.jpg")
  print(x)
  print(y)
  p=plot(x,y,type="p",
       main="Birthday problem",xlab="Class size",
       ylab="Probability of a shared birthday")
  #y=x
  #p=plot(x,y)
  dev.off()
}
main()