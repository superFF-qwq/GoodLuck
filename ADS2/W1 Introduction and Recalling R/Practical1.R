#a=matrix(data=c(17,32,7,32),nrow=2,ncol=2,dimnames=list(c("tumor","total"),c("treatment","control")))
#mode(a) #查看元素类型 numeric
#class(a) #查看数据类型 matrix array
#str(a) #把所有内容以字符串的形式输出
#b=matrix(data=c(17,32,7,32),nrow=2,ncol=2)
#rownames(b)=c("tumor","total")
#colnames(b)=c("treatment","control")
DN=matrix(data=c(57.27,72.53,64.67,65.96,55.99,64.02,65.60,55.15,72.90,59.95,78.99,77.63,80.69,83.49,80.49,80.79,82.11,80.10,81.63,78.82),
          nrow=1,)
rownames(DN)="DN"
colnames(DN)=c(paste0("KO",1:10),paste0("WT",1:10))
#print(DN)
DP=matrix(data=c(19.72,19.57,26.02,18.37,12.35,21.40,17.75,21.86,15.07,14.70,5.76,3.80,3.78,7.21,8.92,5.75,9.32,5.82,7.40,10.69),
          nrow=1)
rownames(DP)="DP"
colnames(DP)=c(paste0("KO",1:10),paste0("WT",1:10))
#print(DP)
## method1 ##
thymocytes=rbind(DN,DP) #bind the rows, columns do not change
as.data.frame(thymocytes)
## method1 ##
## method2 ##
#thymocytes=merge(DN,DP,all=TRUE)
#rownames(thymocytes)=c("DP","DN")
## method2 ##
#class(thymocytes)
SP=matrix(data=100-DP-DN,nrow=1)
rownames(SP)="SP"
colnames(SP)=colnames(DP)
thymocytes=rbind(thymocytes,SP)
Dissection=matrix(data=c(rep("Normal",10),
                         "Splenomegaly","Splenomegaly","Normal","Normal","Normal","Splenomegaly","Normal","Splenomegaly","Splenomegaly","Normal"),
                  nrow=1)
rownames(Dissection)="Dissection results"
colnames(Dissection)=colnames(thymocytes)
#Dissection=format(Dissection,justify="right")
thymocytes=rbind(thymocytes,Dissection)
#print(thymocytes)
#print(Dissection)
write.csv(thymocytes,file="thymocytes.csv")
