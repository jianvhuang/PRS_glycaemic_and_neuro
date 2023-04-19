rm(list=ls())

library(data.table)
library(openxlsx)
library(stringr)
library(ggplot2)

######################################################
# directory
home="H:/HUANGJIAN/analysis/ZZZ-manuscript-PRS_T2D-Neuro/manuscript/NatComms/"

dir.create(file.path(paste0(home,"/"), 
                     "/data/"),
           showWarnings = FALSE)
DIR.data=paste0(home,"/data/")
		   
dir.create(file.path(paste0(home,"/"), 
                     "/output/"),
           showWarnings = FALSE)
DIR.output=paste0(home,"/output/")
######################################################

######################################################
# read data
sampledata=fread(file=paste0(DIR.data,"/sampledata.csv"))
######################################################

######################################################
sampledata[,expo_Avail:=ifelse(is.na(expo1)==F|
                                 is.na(expo2)==F,1,0)]
sampledata[,outc_Avail:=ifelse(is.na(outc1)==F|
                                 is.na(outc2)==F,1,0)]
######################################################

######################################################
# descriptive analysis 
list.cov.cat=c("fac1","fac2","fac3")
list.cov.cont=c("cont1","cont2","cont3")

order=c("cont1","cont2","cont3","fac1","fac2","fac3")
		
######################################################
df4comparison=copy(sampledata)
df4comparison.gp1=df4comparison[outc_Avail==1|outc_Avail==0,]
df4comparison.gp2=df4comparison[outc_Avail==1,]
label=paste0("Main GUSTO (n=",nrow(sampledata),") vs Neurodevelopment (n=",nrow(sampledata[outc_Avail==1,]),")")

#########################
TABLE.cov.cat.gp1=NA
for (cov.cat in list.cov.cat){
  df.cov=df4comparison.gp1[,c(cov.cat),with=F]
  n.cov=table(df.cov)
  perc.cov=n.cov/nrow(df4comparison.gp1)
  
  if (nrow(n.cov)!=0){
    table.cov=data.table(data.frame(variable=cov.cat,
                                    n=n.cov,
                                    perc=perc.cov))
    setnames(table.cov,
             old=c(paste0("n.",cov.cat),paste0("perc.",cov.cat)),
             new=c(paste0("n.df.cov"),paste0("perc.df.cov")))
  } else {
    table.cov=data.table(data.frame(variable=cov.cat,
                                    n=NA,
                                    perc=NA))
  }
  
  
  TABLE.cov.cat.gp1=rbind(TABLE.cov.cat.gp1,table.cov,fill=T)
  
}
TABLE.cov.cat.gp1[,x:=NULL]
TABLE.cov.cat.gp1=TABLE.cov.cat.gp1[is.na(variable)==F,]
setnames(TABLE.cov.cat.gp1,
         old=c("n.df.cov","n.Freq","perc.Freq"),
         new=c("category","N","Perc"))
TABLE.cov.cat.gp1[,gp:="gp1"]
TABLE.cov.cat.gp1=TABLE.cov.cat.gp1[,.(variable,category,gp,N,Perc)]
#########################

#########################
TABLE.cov.cat.gp2=NA
for (cov.cat in list.cov.cat){
  df.cov=df4comparison.gp2[,c(cov.cat),with=F]
  n.cov=table(df.cov)
  perc.cov=n.cov/nrow(df4comparison.gp2)
  
  if (nrow(n.cov)!=0){
    table.cov=data.table(data.frame(variable=cov.cat,
                                    n=n.cov,
                                    perc=perc.cov))
    setnames(table.cov,
             old=c(paste0("n.",cov.cat),paste0("perc.",cov.cat)),
             new=c(paste0("n.df.cov"),paste0("perc.df.cov")))
  } else {
    table.cov=data.table(data.frame(variable=cov.cat,
                                    n=NA,
                                    perc=NA))
  }
  
  
  TABLE.cov.cat.gp2=rbind(TABLE.cov.cat.gp2,table.cov,fill=T)
  
}
TABLE.cov.cat.gp2[,x:=NULL]
TABLE.cov.cat.gp2=TABLE.cov.cat.gp2[is.na(variable)==F,]
setnames(TABLE.cov.cat.gp2,
         old=c("n.df.cov","n.Freq","perc.Freq"),
         new=c("category","N","Perc"))
TABLE.cov.cat.gp2[,gp:="gp2"]
TABLE.cov.cat.gp2=TABLE.cov.cat.gp2[,.(variable,category,gp,N,Perc)]
#########################

#########################
TABLE.cov.cont.gp1=NA
for (cov.cont in list.cov.cont){
  df.cov=df4comparison.gp1[,c(cov.cont),with=F]
  mean.cov=mean(unlist(df.cov),na.rm=T)
  sd.cov=sd(unlist(df.cov),na.rm=T)
  
  table.cov=data.table(data.frame(variable=cov.cont,
                                  N=length(unlist(df.cov)[is.na(unlist(df.cov))==F]),
                                  mean=mean.cov,
                                  sd=sd.cov))
  
  TABLE.cov.cont.gp1=rbind(TABLE.cov.cont.gp1,table.cov,fill=T)
  
}
TABLE.cov.cont.gp1[,x:=NULL]
TABLE.cov.cont.gp1=TABLE.cov.cont.gp1[is.na(variable)==F,]
setnames(TABLE.cov.cont.gp1,
         old=c("N","mean","sd"),
         new=c("N","mean","sd"))
TABLE.cov.cont.gp1[,gp:="gp1"]
TABLE.cov.cont.gp1=TABLE.cov.cont.gp1[,.(variable,gp,N,mean,sd)]
#########################

#########################
TABLE.cov.cont.gp2=NA
for (cov.cont in list.cov.cont){
  df.cov=df4comparison.gp2[,c(cov.cont),with=F]
  mean.cov=mean(unlist(df.cov),na.rm=T)
  sd.cov=sd(unlist(df.cov),na.rm=T)
  
  table.cov=data.table(data.frame(variable=cov.cont,
                                  N=length(unlist(df.cov)[is.na(unlist(df.cov))==F]),
                                  mean=mean.cov,
                                  sd=sd.cov))
  
  TABLE.cov.cont.gp2=rbind(TABLE.cov.cont.gp2,table.cov,fill=T)
  
}
TABLE.cov.cont.gp2[,x:=NULL]
TABLE.cov.cont.gp2=TABLE.cov.cont.gp2[is.na(variable)==F,]
setnames(TABLE.cov.cont.gp2,
         old=c("N","mean","sd"),
         new=c("N","mean","sd"))
TABLE.cov.cont.gp2[,gp:="gp2"]
TABLE.cov.cont.gp2=TABLE.cov.cont.gp2[,.(variable,gp,N,mean,sd)]
#########################

TABLE.cov.cont=merge(TABLE.cov.cont.gp1,TABLE.cov.cont.gp2,by="variable",all=T)

TABLE.cov.cat=merge(TABLE.cov.cat.gp1,TABLE.cov.cat.gp2,by=c("variable","category"),all=T)
TABLE.cov.cat=TABLE.cov.cat[is.na(category)==F,]
############################################

############################################
# chi-square for cov.cat
result.chisq=NA
for (cov.cat in list.cov.cat){
  dat=TABLE.cov.cat[variable==cov.cat,]
  
    chisq=chisq.test(dat[,.(N.x,N.y)])
    
    result=data.table(data.frame(variable=cov.cat,
                                 chisq.pval=chisq$p.value))
    result.chisq=rbind(result.chisq,result,fill=T)
  
}
result.chisq[,x:=NULL]
result.chisq=result.chisq[is.na(variable)==F,]
TABLE.cov.cat=merge(TABLE.cov.cat,result.chisq,by="variable",all.x=T)

# t.test for cov.cont
result.ttest=NA
for (cov.cont in list.cov.cont){
  data.gp1=unlist(df4comparison.gp1[,c(cov.cont),with=F])
  data.gp2=unlist(df4comparison.gp2[,c(cov.cont),with=F])
  
    ttest=t.test(data.gp1, data.gp2, var.equal=FALSE, alternative = "two.sided")
    
    result=data.table(data.frame(variable=cov.cont,
                                 ttest.pval=ttest$p.value))
    result.ttest=rbind(result.ttest,result,fill=T)
  
  
}
result.ttest[,x:=NULL]
result.ttest=result.ttest[is.na(variable)==F,]
TABLE.cov.cont=merge(TABLE.cov.cont,result.ttest,by="variable",all.x=T)
############################

############################
TABLE.cov=rbind(TABLE.cov.cont,TABLE.cov.cat,fill=T)

All=data.table(data.frame(variable="Overall",
                          N.x=nrow(df4comparison.gp1),
                          N.y=nrow(df4comparison.gp2)))
TABLE.cov=rbind(All,TABLE.cov,fill=T)
TABLE.cov=TABLE.cov[,.(variable,category,
                       N.x,Perc.x,mean.x,sd.x,
                       N.y,Perc.y,mean.y,sd.y,
                       chisq.pval,ttest.pval)]

order.var=c("Overall",order)
TABLE.cov[,variable:=factor(variable,levels=order.var)]
TABLE.cov=TABLE.cov[order(variable),]

write.csv(TABLE.cov,file=paste0(DIR.output,"/TABLE-Decriptive-",label,".csv"),row.names = F)
############################
######################################################
