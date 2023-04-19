rm(list=ls())

library(data.table)
library(openxlsx)
library(stringr)
library(sensemakr)
library(lmtest) # coeftest
library(sandwich) # robust standard error
library(ivreg) # one sample MR

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
# analysis
##########################
# model setting
ModelIndex.temp="MODEL1"
iv_exposure.temp="expo1"
exposure.temp="cont8"
outcome.temp="outc1"
cov.temp=c("cont1","cont2","cont3")
label.temp=paste0(iv_exposure.temp,"_",exposure.temp,"_",outcome.temp,"_",ModelIndex.temp)

var.RHS=c(exposure.temp,cov.temp)
var.RHS=var.RHS[!var.RHS==""]

df.temp=sampledata[,c(iv_exposure.temp,exposure.temp,outcome.temp,cov.temp),with=F]

samplesize.expo=unlist(df.temp[,c("expo1"),with=F])
samplesize.expo=length(samplesize.expo[is.na(samplesize.expo)==F])
  
samplesize.outc=unlist(df.temp[,c("outc1"),with=F])
samplesize.outc=length(samplesize.outc[is.na(samplesize.outc)==F])
  
df.temp=na.omit(df.temp)
samplesize.mod=nrow(df.temp)

model=as.formula(paste0(paste(outcome.temp, paste(c(var.RHS), collapse=" + "), sep=" ~ "),
                        "|",iv_exposure.temp,"+",paste(c(var.RHS[!var.RHS%like%exposure.temp]), collapse=" + ")))
##########################

##########################
# analysis
type.outcome.temp="continuous"
osmr = tryCatch({
  if (type.outcome.temp=="continuous") {
    ivreg(formula= model, data=df.temp)
    } else if (type.outcome.temp=="categorical") {
    ivreg(formula= model, data=df.temp)
      }
  }, error = function(e) {
    return(NA)
    })  
##########################

##########################
# summary table
if(is.na(osmr)[1]==T){
  ivreg.summary=data.table(data.frame(variable=exposure.temp,
                                    beta=NA,se=NA,tvalue=NA,pvalue=NA))
  } else {
    ivreg.summary=as.data.frame(summary(osmr)$coefficients)
    if (type.outcome.temp=="continuous") {
      setnames(ivreg.summary,old=colnames(ivreg.summary),new=c("beta","se","tzvalue","pvalue"))
      } else if (type.outcome.temp=="categorical") {
        setnames(ivreg.summary,old=colnames(ivreg.summary),new=c("beta","se","tzvalue","pvalue"))
        }

      ivreg.summary$variable=rownames(ivreg.summary)
	  ivreg.summary=data.table(ivreg.summary)
      
		ivreg.summary[,se.robust:=coeftest(osmr, vcov = vcovHC(osmr, type="HC3"))[,2]]
		ivreg.summary[,tzvalue.robust:=coeftest(osmr, vcov = vcovHC(osmr, type="HC3"))[,3]]
		ivreg.summary[,pvalue.robust:=coeftest(osmr, vcov = vcovHC(osmr, type="HC3"))[,4]]
			  
      }

ivreg.summary[,iv_exposure:=iv_exposure.temp]
ivreg.summary[,exposure:=exposure.temp]
ivreg.summary[,outcome:=outcome.temp]
      
ivreg.summary[,samplesize.exposure:=ifelse(variable==exposure.temp,samplesize.expo,NA)]
ivreg.summary[,samplesize.outcome:=ifelse(variable==exposure.temp,samplesize.outc,NA)]
ivreg.summary[,samplesize.model:=ifelse(variable==exposure.temp,samplesize.mod,NA)]
      
ivreg.summary[,ModelIndex:=ModelIndex.temp]
ivreg.summary[,MODEL:=gsub(" ","",Reduce(paste, deparse(model)))]
ivreg.summary[,MODEL:=gsub("[+]"," + ",MODEL)]
ivreg.summary[,MODEL:=gsub("[~]"," ~ ",MODEL)]
##########################
      
##########################
# obtain 95%CI
if(!sum(is.na(ivreg.summary$pvalue))==nrow(ivreg.summary)){
  ##############
  CI = tryCatch({
    as.data.frame(confint(osmr))
    }, error = function(e) {
      return(NA)
      })
	  
  CI_robust=as.data.frame(coefci(osmr, vcov. = vcovHC(osmr, type = 'HC3')))
  ##############
  
  ##############
      CI$variable=rownames(CI)
      CI=data.table(CI)
      setnames(CI,old=c("2.5 %","97.5 %"),new=c("lower95","upper95"))
          
      ivreg.summary=merge(ivreg.summary,CI,by="variable",all=T)
      
      CI_robust$variable=rownames(CI_robust)
      CI_robust=data.table(CI_robust)
      setnames(CI_robust,old=c("2.5 %","97.5 %"),new=c("lower95_robust","upper95_robust"))
          
      ivreg.summary=merge(ivreg.summary,CI_robust,by="variable",all=T)
  ##############  
     
   } else {
    ivreg.summary[,lower95:=NA]
    ivreg.summary[,upper95:=NA]
    ivreg.summary[,lower95_robust:=NA]
    ivreg.summary[,upper95_robust:=NA]
    }
##########################

##########################
# export output table

ivreg.summary.output=ivreg.summary[,.(exposure,samplesize.exposure,
                                             outcome,samplesize.outcome,
                                             ModelIndex,MODEL,samplesize.model,variable,beta,
                                             se,tzvalue,pvalue,
											 lower95,upper95,
                                             se.robust,tzvalue.robust,pvalue.robust,
											 lower95_robust,upper95_robust)]

write.csv(ivreg.summary.output,file=paste0(DIR.output,"/ivreg-",label.temp,".csv"),row.names = F)
##########################

################################################################################



