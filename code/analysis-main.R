rm(list=ls())

library(data.table)
library(openxlsx)
library(stringr)
library(ggplot2)
library(sensemakr)
library(lmtest) # coeftest
library(sandwich) # robust standard error

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
##################
# model setting
ModelIndex.temp="MODEL1"
exposure.temp="expo1"
outcome.temp="outc1"
cov.temp=c("cont1","cont2","cont3","fac1","fac2","fac3")
label.temp=paste0(exposure.temp,"_",outcome.temp,"_",ModelIndex.temp)

var.RHS=c(exposure.temp,cov.temp)
var.RHS=var.RHS[!var.RHS==""]

df.temp=sampledata[,c(exposure.temp,outcome.temp,cov.temp),with=F]

samplesize.expo=unlist(df.temp[,c("expo1"),with=F])
samplesize.expo=length(samplesize.expo[is.na(samplesize.expo)==F])
  
samplesize.outc=unlist(df.temp[,c("outc1"),with=F])
samplesize.outc=length(samplesize.outc[is.na(samplesize.outc)==F])
  
df.temp=na.omit(df.temp)
samplesize.mod=nrow(df.temp)

model=as.formula(paste(outcome.temp, paste(c(var.RHS), collapse=" + "), sep=" ~ "))
##################

##################
# regression
type.outcome.temp="continuous"
reg = tryCatch({
  if (type.outcome.temp=="continuous") {
    glm(formula=model,data=df.temp,family="gaussian") 
    } else if (type.outcome.temp=="categorical") {
    glm(formula=model,data=df.temp,family="binomial") 
      }
  }, error = function(e) {
    return(NA)
    })  
##################

##################
# summary table
if(is.na(reg)[1]==T){
  reg.summary=data.table(data.frame(variable=exposure.temp,
                                    beta=NA,se=NA,tvalue=NA,pvalue=NA))
  } else {
    reg.summary=as.data.frame(summary(reg)$coefficients)
    if (type.outcome.temp=="continuous") {
      setnames(reg.summary,
               old=c("Estimate","Std. Error","t value","Pr(>|t|)"),
               new=c("beta","se","tzvalue","pvalue")) 
      } else if (type.outcome.temp=="categorical") {
        setnames(reg.summary,
                 old=c("Estimate","Std. Error","z value","Pr(>|z|)"),
                 new=c("beta","se","tzvalue","pvalue")) 
        }

      reg.summary$variable=rownames(reg.summary)
      reg.summary=data.table(reg.summary)
      
      reg.summary[,se.robust:=coeftest(reg, vcov = vcovHC(reg, type="HC3"))[,2]]
      reg.summary[,tzvalue.robust:=coeftest(reg, vcov = vcovHC(reg, type="HC3"))[,3]]
      reg.summary[,pvalue.robust:=coeftest(reg, vcov = vcovHC(reg, type="HC3"))[,4]]
      
      }

reg.summary[,exposure:=exposure.temp]
reg.summary[,outcome:=outcome.temp]
      
reg.summary[,samplesize.exposure:=samplesize.expo]
reg.summary[,samplesize.outcome:=samplesize.outc]
reg.summary[,samplesize.model:=samplesize.mod]
      
reg.summary[,ModelIndex:=ModelIndex.temp]
reg.summary[,MODEL:=gsub(" ","",Reduce(paste, deparse(model)))]
reg.summary[,MODEL:=gsub("[+]"," + ",MODEL)]
reg.summary[,MODEL:=gsub("[~]"," ~ ",MODEL)]
##################


##########################
# obtain 95%CI
if(!sum(is.na(reg.summary$pvalue))==nrow(reg.summary)){
  ##############
  CI = tryCatch({
    as.data.frame(confint(reg))
    }, error = function(e) {
      return(NA)
      })
	  
  CI_robust=as.data.frame(coefci(reg, vcov. = vcovHC(reg, type = 'HC3')))
  ##############
  
  ##############
      CI$variable=rownames(CI)
      CI=data.table(CI)
      setnames(CI,old=c("2.5 %","97.5 %"),new=c("lower95","upper95"))
      reg.summary=merge(reg.summary,CI,by="variable",all=T)
      
	
      CI_robust$variable=rownames(CI_robust)
      CI_robust=data.table(CI_robust)
      setnames(CI_robust,old=c("2.5 %","97.5 %"),new=c("lower95_robust","upper95_robust"))
      reg.summary=merge(reg.summary,CI_robust,by="variable",all=T)
  ##############  
     
   } else {
    reg.summary[,lower95:=NA]
    reg.summary[,upper95:=NA]
    reg.summary[,lower95_robust:=NA]
    reg.summary[,upper95_robust:=NA]
    }
##########################

##########################
# export output table
reg.summary.output=reg.summary[,.(exposure,samplesize.exposure,
                                             outcome,samplesize.outcome,
                                             ModelIndex,MODEL,samplesize.model,variable,beta,
                                             se,tzvalue,pvalue,
                                             lower95,upper95,
                                             se.robust,tzvalue.robust,pvalue.robust,
											 lower95_robust,upper95_robust)]
											 
write.csv(reg.summary.output,file=paste0(DIR.output,"/main_analysis-",label.temp,".csv"),row.names = F)
##########################

################################################################################
