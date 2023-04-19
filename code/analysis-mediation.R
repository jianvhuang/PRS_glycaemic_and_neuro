rm(list=ls())

library(data.table)
library(openxlsx)
library(stringr)
library(ggplot2)
library(regmedint)

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
ExpoDF.temp=copy(sampledata)
list.expo=names(ExpoDF.temp)[names(ExpoDF.temp)%like%"expo"]

ExpoDF4Quantile=NA
for (e in list.expo) {
  
  setnames(ExpoDF.temp,old=e,new="e.temp")
  
  ExpoDF4Quantile.temp=data.table(data.frame(exposure=e,
                                             Q10=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.10)),
                                             Q20=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.20)),
                                             Q25=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.25)),
                                             Q40=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.40)),
                                             Q50=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.5)),
                                             Q60=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.60)),
                                             Q75=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.75)),
                                             Q80=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.80)),
                                             Q90=quantile(ExpoDF.temp$e.temp,na.rm=T,probs =c(0.90))))
  
  setnames(ExpoDF.temp,old="e.temp",new=e)
  ExpoDF4Quantile=rbind(ExpoDF4Quantile,ExpoDF4Quantile.temp,fill=T)
}
ExpoDF.temp=NULL
ExpoDF4Quantile[,x:=NULL]
ExpoDF4Quantile=ExpoDF4Quantile[is.na(exposure)==F,]
######################################################

######################################################
# Mediator data
list.mediator=c("cont7","cont8","cont9",
                "fac5")
mediator.info=data.table(data.frame(var=list.mediator))
mediator.info[,type:=ifelse(var%in%c("fac7","fac8"),"binary","continuous")]
######################################################


######################################################
# regmedint

  ModelIndex="MODEL1"
  expo="expo1"
  medi="cont7"
  outc="outc1"
  yreg.temp="linear" # change this to logistic if outcome is binary
  list.cov=c("cont1","cont2","cont3")

  df.temp=na.omit(sampledata[,c("SubjectID",expo,medi,outc,list.cov),with=F])
  nrow(df.temp)

	# set mediator value for cde
  if (mediator.info[var==medi,]$type=="continuous") {
    value.m_cde=colMeans(df.temp[,c(medi),with=F]) # if medi is continuous, set this as mean value
    mreg.temp="linear"
  } else if (mediator.info[var==medi,]$type=="binary") {
    value.m_cde=0 # if medi is binary, set this as 0, i.e., the null status of the binary mediator
    samplesize.medi=data.table(as.data.frame(table(df.temp[,colnames(df.temp)==medi,with=F])))
    mreg.temp="logistic"
  }
  
  # set covariate value  
  cov.info=data.table(data.frame(var=list.cov))
  cov.info[,type:=ifelse(var%like%"cont","continuous",
						ifelse(var%like%"fac","binary",NA))]
  cov.info.temp=cov.info[var%in%list.cov,]
  cov.info.temp[,c_cond:=ifelse(type=="binary",0,NA)]
  for (i in 1:nrow(cov.info.temp)){
    var.temp=as.character(unlist(cov.info.temp[i,.(var)]))
    type.temp=as.character(unlist(cov.info.temp[i,.(type)]))
    
    if (type.temp=="continuous"){
      c_cond.temp=colMeans(df.temp[,colnames(df.temp)==var.temp,with=F],na.rm=T)
      cov.info.temp[,c_cond:=ifelse(var==var.temp,c_cond.temp,c_cond)]
    }
  }
  
  list.cov=cov.info.temp$var
  list.c_cond=cov.info.temp$c_cond
  
  EffectSetting=data.table(data.frame(comparison=c("Q75vsQ25"),
                                      a0=c(ExpoDF4Quantile[exposure==expo,]$Q25),
                                      a1=c(ExpoDF4Quantile[exposure==expo,]$Q75)))

	a0.temp=as.numeric(unlist(EffectSetting[,.(a0)]))
    a1.temp=as.numeric(unlist(EffectSetting[,.(a1)]))
    comparison.temp=as.character(unlist(EffectSetting[,.(comparison)]))
	
    regmedint_obj <- regmedint(data = df.temp,
                               ## Variables
                               yvar = outc,
                               avar = expo,
                               mvar = medi,
                               cvar = c(list.cov),
                               # eventvar = "event", # only required for survival outcome
                               ## Values at which effects are evaluated
                               a0 = a0.temp,
                               a1 = a1.temp,
                               m_cde = value.m_cde,
                               c_cond = list.c_cond,
                               ## Model types
                               mreg = mreg.temp, 
                               yreg = yreg.temp,
                               ## Additional specification
                               interaction = TRUE,
                               casecontrol = FALSE)
    
    output.Inter.Mod=as.data.frame(summary(regmedint_obj)$summary_myreg)
    output.Inter.Mod$variable=rownames(output.Inter.Mod)
    output.Inter.Mod$exposure=expo
    output.Inter.Mod$comparison=comparison.temp
    output.Inter.Mod$a0=a0.temp
    output.Inter.Mod$a1=a1.temp
    output.Inter.Mod$mediator=medi
    output.Inter.Mod$outcome=outc
    output.Inter.Mod$covariate=paste0(list.cov,collapse = "+")
    output.Inter.Mod$interaction="ExposureMediator"
    output.Inter.Mod$ModelIndex=ModelIndex
    output.Inter.Mod$samplesize=nrow(df.temp)
    
    if (mediator.info[var==medi,]$type=="binary") {
      output.Inter.Mod$samplesize.medi0=samplesize.medi[Var1==0,]$Freq
      output.Inter.Mod$samplesize.medi1=samplesize.medi[Var1==1,]$Freq
    } else {
      output.Inter.Mod$samplesize.medi0=NA
      output.Inter.Mod$samplesize.medi1=NA
    }
    
    output.Inter.Mod=data.table(output.Inter.Mod)
    setnames(output.Inter.Mod,
             old=c("variable","est","se","Z","p",
                   "exposure","comparison","a0","a1","mediator","outcome","covariate","interaction",
                   "ModelIndex","samplesize","samplesize.medi0","samplesize.medi1",
                   "lower","upper"),
             new=c("variable","beta","se","zvalue","pvalue",
                   "exposure","comparison","a0","a1","mediator","outcome","covariate","interaction",
                   "ModelIndex","samplesize","samplesize.medi0","samplesize.medi1",
                   "lower95","upper95"))
    
output.Inter.Mod=output.Inter.Mod[,.(exposure,comparison,a0,a1,mediator,outcome,covariate,interaction,ModelIndex,
                                                                 samplesize,samplesize.medi0,samplesize.medi1,
                                                                 variable,beta,se,zvalue,pvalue,lower95,upper95)]

write.csv(output.Inter.Mod,file=paste0(DIR.output,"/mediation-",expo,"-",medi,"-",outc,".csv"),row.names = F)






