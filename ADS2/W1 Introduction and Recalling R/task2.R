library(stringr)
a=read.csv("ADS2week1.csv")
#print(a)
ans=""
for(j in 2:5){
  s=""
  i=1
  while(!is.na(a[i,j])&&!is.null(a[i,j])){
    if(s=="")
      s=a[i,j]
    else
      s=paste(s,a[i,j])
    i=i+1
  }
  ## method 1 ##
  #cat(s,file="a.txt",fill=TRUE,append=TRUE)
  ## method 1 ##
  if(ans=="")
    ans=s
  else
    ans=paste(ans,s,sep="\n")
  writeLines(ans,"a.txt")
}