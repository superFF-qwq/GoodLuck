main=function(){
  alt=array(data=c(33.45, 24.67, 24.16, 21.27, 26.86,
                   27.38, 27.91, 26.15, 31.63, 28.12))
  n=length(alt)
  sum=0
  for(i in alt)
    sum=sum+i
  #print(sum)
  mn=1.0*sum/n
  sd=0
  for(i in alt)
    sd=sd+(mn-i)*(mn-i)
  sd=sd/n
  sd=sqrt(sd)
  #print(mn)
  #print(sd)
  ans1=dnorm(40.2,mean=mn,sd=sd)
  print(sprintf("answer for Q1: %f",ans1))
  ans2=1-pnorm(33,mean=mn,sd=sd)
  print(sprintf("answer for Q2: %f",ans2))
  ans3=pnorm(25,mean=mn,sd=sd)-pnorm(22,mean=mn,sd=sd)
  print(sprintf("answer for Q3: %f",ans3))
  ans4=pnorm(31,mean=mn,sd=sd)-pnorm(27,mean=mn,sd=sd)
  print(sprintf("answer for Q4: %f",ans4))
  l=qnorm(0.4,mean=mn,sd=sd)
  r=qnorm(0.65,mean=mn,sd=sd)
  print(sprintf("answer for Q5: [%f,%f]",l,r))
  #ans5
  ans6=qnorm(1-0.99995,mean=mn,sd=sd)
  print(sprintf("answer for Q6: %f",ans6))
#dnorm(seq,mean,sd)函数给出给定平均值和标准偏差在每个点的概率分布的高度
#pnorm()函数给出正态分布随机数小于给定数值的概率
#qnorm()函数采用概率值，并给出其累积值与概率值匹配的数字值。
#rnorm()函数用于生成分布正常的随机数，它将样本大小作为输入，并生成许多随机数
}
main()