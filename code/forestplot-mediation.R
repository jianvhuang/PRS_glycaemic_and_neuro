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
df4plot.temp=fread(file=paste0(DIR.data,"/sample_mediationresult.csv"))

list.exposure=c("PRS_T2D_Child_SexStratified_CBH2226HOIR_CplusT","PRS_T2D_Paternal_Male_CBH2226HOIR_CplusT","PRS_T2D_Maternal_Female_CBH2226HOIR_CplusT",
                "PRS_T2D_Child_SexStratified_CGL2226FAST_CplusT","PRS_T2D_Paternal_Male_CGL2226FAST_CplusT","PRS_T2D_Maternal_Female_CGL2226FAST_CplusT")

list.outcome=c("Perceptual.reasoning.composite.score_z",
               "wiat3_avg_ss_z")
######################################################


############################################
n.mediator=length(unique(df4plot.temp$mediator))

n.strata=length(unique(df4plot.temp$variable))
df4plot.temp[,variable:=factor(variable,
                               levels=c("cde","pnde","tnde","pnie","tnie","te"),
                               labels=c("controlled direct effect",
                                        "pure natural direct effect",
                                        "total natural direct effect",
                                        "pure natural indirect effect",
                                        "total natural indirect effect",
                                        "total effect"))]

list.panel.x=c("PRS for HOMA-IR","PRS for fasting glucose")
names(list.panel.x)=unique(df4plot.temp$exposure)
########################################


########################################
df4plot.temp.order=df4plot.temp[order(exposure,mediator,outcome,variable),]
df4plot.temp.order[,"plotgroup"]=paste(df4plot.temp.order$mediator,df4plot.temp.order$variable)
unique(df4plot.temp.order$plotgroup)
df4plot.temp.order$plotgroup=factor(df4plot.temp.order$plotgroup,levels=c(rev(unique(df4plot.temp.order$plotgroup))))

df4plot.temp.order.yRef=NA
for (p in unique(df4plot.temp.order$exposure)){
  df4plot.temp.p=df4plot.temp.order[exposure==p,]
  df4plot.temp.p=df4plot.temp.p[order(mediator)]
  #df4plot.temp.p=df4plot.temp.p[order(outcome,stratification)]
  
  yRef=1:(length(unique(df4plot.temp.p$mediator))*n.strata+length(unique(df4plot.temp.p$mediator))-1)
  yRef=yRef[yRef%%(n.strata+1)!=0]
  
  df4plot.temp.p[,yRef:=rev(yRef)]
  df4plot.temp.order.yRef=rbind(df4plot.temp.order.yRef,df4plot.temp.p,fill=T)
}
df4plot.temp.order.yRef[,x:=NULL]
df4plot.temp.order.yRef=df4plot.temp.order.yRef[is.na(yRef)==F,]

yBreak=unique(df4plot.temp.order.yRef[variable=="total natural direct effect",]$yRef)
yLabel=unique(df4plot.temp.order.yRef[variable=="total natural direct effect",]$mediator)
########################################


########################################
df4plot.temp.order.yRef[,sig:=ifelse(pvalue<0.05,"sig.pval",ifelse(lower95_tzrobust*upper95_tzrobust>0,"sig.ci","ns"))]
df4plot.temp.order.yRef[,sig:=factor(sig,levels = c("sig.pval","sig.ci","ns"))]
df4plot.temp.order.yRef[,sigColour:=ifelse(sig%in%c("sig.pval"),"red","blue")]
df4plot.temp.order.yRef[,sigColour:=factor(sigColour,levels = c("red","blue"))]

df4plot.temp.order.yRef[,sigSize:=ifelse(sig%in%c("sig.pval","sig.ci"),4,2)]

nSize=length(unique(df4plot.temp.order.yRef$sigSize))
nSize
nColour=length(unique(df4plot.temp.order.yRef$sigColour))
nColour
########################################


########################################
xMin=min(c(df4plot.temp.order.yRef$lower95_tzrobust,df4plot.temp.order.yRef$upper95_tzrobust),na.rm=T)
xMax=max(c(df4plot.temp.order.yRef$lower95_tzrobust,df4plot.temp.order.yRef$upper95_tzrobust),na.rm=T)
if (xMin<0){
  xLimit.Min=ceiling(xMin-1)
} else {
  xLimit.Min=ceiling(xMin)
}

if (xMax<0){
  xLimit.Max=ceiling(xMax+1)
} else {
  xLimit.Max=ceiling(xMax)
}
xLimit.Max
xLimit.Min
########################################


########################################
df4plot.temp.order.yRef.ribbon=unique(df4plot.temp.order.yRef[,.(mediator,plotgroup,yRef)])
df4plot.temp.order.yRef.ribbon[,yMin:=yRef-0.5]
df4plot.temp.order.yRef.ribbon[,yMax:=yRef+0.5]
df4plot.temp.order.yRef.ribbon[,ribbonfill:=rep(c("a","b"),
                                                each=6,
                                                length.out=nrow(df4plot.temp.order.yRef.ribbon))]

df4plot.temp.order.yRef.ribbon.xMin=copy(df4plot.temp.order.yRef.ribbon[,x:=xLimit.Min])
df4plot.temp.order.yRef.ribbon.xMax=copy(df4plot.temp.order.yRef.ribbon[,x:=xLimit.Max])
df4plot.temp.order.yRef.ribbon=rbind(df4plot.temp.order.yRef.ribbon.xMin,
                                     df4plot.temp.order.yRef.ribbon.xMax)
########################################


########################################
if (nSize==2 & nColour==2) {
  forest <- ggplot(data=df4plot.temp.order.yRef,aes(x=zvalue,y=yRef))+
    facet_wrap(~ exposure,  labeller = labeller(exposure = list.panel.x)) +
    geom_point(aes(shape=variable,colour=sigColour,size=sigSize)) +
    geom_errorbarh(aes(xmin=lower95_tzrobust,xmax=upper95_tzrobust),height=0)+
    geom_vline(xintercept=0,linetype=2) +
    scale_y_continuous(limits=c(0,max(yRef)+1),
                       breaks=c(yBreak),
                       labels=c(yLabel),
                       expand=c(0.02,0)) +
    scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
                       expand=c(0.02,0))+
    # scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
    #                    breaks=c(-30,-20,-10,0,10,20,30,40),
    #                    labels=c(-30,-20,-10,0,10,20,30,40))+
    scale_colour_manual(values =c("#f8766d","#00b0f6"),guide="none") + # ; c("#00b0f6")
    scale_shape_manual(values=c(5,15,0,17,2,19)) +
    scale_size_continuous(range = c(1, 1.5),guide = "none") +
    geom_ribbon(data=df4plot.temp.order.yRef.ribbon,
                aes(x=x,ymin = yMin, ymax = yMax,fill=ribbonfill,group=yRef),alpha=0.2,inherit.aes = FALSE) +
    scale_fill_manual(values =c("NA","azure3"),guide="none") +
    xlab("Z-value") +
    ylab("") +
    theme_classic()+
    theme(plot.title = element_text(vjust=1, face="bold"),
          panel.border = element_rect(color="black",fill=NA),
          strip.text = element_text(size=15),
          axis.text = element_text(color="black",size=20),
          axis.title = element_text(color="black",size=20),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position="bottom",
          legend.key = element_blank(),
          legend.text = element_text(color="black",size=18))+
    guides(shape = guide_legend("Effect",order=1,nrow=6,ncol=1))
  
  if (n.mediator>10) {
    height.temp=n.mediator*2.5
  } else if (n.mediator<=10) {
    height.temp=n.mediator*3
  }
  
  ggsave(plot=forest,
         filename=paste0(DIR.output,"/Forestplot-sample-mediation.png"), 
         width = 30, height = height.temp, units="cm",device='png', dpi=300)
}
########################################

########################################
if (nSize==2 & nColour==1) {
  forest <- ggplot(data=df4plot.temp.order.yRef,aes(x=zvalue,y=yRef))+
    facet_wrap(~ exposure,  labeller = labeller(exposure = list.panel.x)) +
    geom_point(aes(shape=variable,colour=sigColour,size=sigSize)) +
    geom_errorbarh(aes(xmin=lower95_tzrobust,xmax=upper95_tzrobust),height=0)+
    geom_vline(xintercept=0,linetype=2) +
    scale_y_continuous(limits=c(0,max(yRef)+1),
                       breaks=c(yBreak),
                       labels=c(yLabel),
                       expand=c(0.02,0)) +
    scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
                       expand=c(0.02,0))+
    # scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
    #                    breaks=c(-30,-20,-10,0,10,20,30,40),
    #                    labels=c(-30,-20,-10,0,10,20,30,40))+
    scale_colour_manual(values =c("#00b0f6"),guide="none") + # ;  c("#f8766d","#00b0f6")
    scale_shape_manual(values=c(5,15,0,17,2,19)) +
    scale_size_continuous(range = c(1, 1.5),guide = "none") +
    geom_ribbon(data=df4plot.temp.order.yRef.ribbon,
                aes(x=x,ymin = yMin, ymax = yMax,fill=ribbonfill,group=yRef),alpha=0.2,inherit.aes = FALSE) +
    scale_fill_manual(values =c("NA","azure3"),guide="none") +
    xlab("Z-value") +
    ylab("") +
    theme_classic()+
    theme(plot.title = element_text(vjust=1, face="bold"),
          panel.border = element_rect(color="black",fill=NA),
          strip.text = element_text(size=15),
          axis.text = element_text(color="black",size=20),
          axis.title = element_text(color="black",size=20),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position="bottom",
          legend.key = element_blank(),
          legend.text = element_text(color="black",size=18))+
    guides(shape = guide_legend("Effect",order=1,nrow=6,ncol=1))
  
  if (n.mediator>10) {
    height.temp=n.mediator*2.5
  } else if (n.mediator<=10) {
    height.temp=n.mediator*3
  }
  
  ggsave(plot=forest,
         filename=paste0(DIR.output,"/Forestplot-sample-mediation.png"), 
         width = 30, height = height.temp, units="cm",device='png', dpi=300)
}
########################################


########################################
if (nSize==1 & nColour==1) {
  forest <- ggplot(data=df4plot.temp.order.yRef,aes(x=zvalue,y=yRef))+
    facet_wrap(~ exposure,  labeller = labeller(exposure = list.panel.x)) +
    geom_point(aes(shape=variable,colour=sigColour),size=1) +
    geom_errorbarh(aes(xmin=lower95_tzrobust,xmax=upper95_tzrobust),height=0)+
    geom_vline(xintercept=0,linetype=2) +
    scale_y_continuous(limits=c(0,max(yRef)+1),
                       breaks=c(yBreak),
                       labels=c(yLabel),
                       expand=c(0.02,0)) +
    scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
                       expand=c(0.02,0))+
    # scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
    #                    breaks=c(-30,-20,-10,0,10,20,30,40),
    #                    labels=c(-30,-20,-10,0,10,20,30,40))+
    scale_colour_manual(values =c("#00b0f6"),guide="none") + # c("#f8766d","#00b0f6"); c("#00b0f6")
    scale_shape_manual(values=c(5,15,0,17,2,19)) +
    scale_size_continuous(range = c(1, 1.5),guide = "none") +
    geom_ribbon(data=df4plot.temp.order.yRef.ribbon,
                aes(x=x,ymin = yMin, ymax = yMax,fill=ribbonfill,group=yRef),alpha=0.2,inherit.aes = FALSE) +
    scale_fill_manual(values =c("NA","azure3"),guide="none") +
    xlab("Z-value") +
    ylab("") +
    theme_classic()+
    theme(plot.title = element_text(vjust=1, face="bold"),
          panel.border = element_rect(color="black",fill=NA),
          strip.text = element_text(size=15),
          axis.text = element_text(color="black",size=20),
          axis.title = element_text(color="black",size=20),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position="bottom",
          legend.key = element_blank(),
          legend.text = element_text(color="black",size=18))+
    guides(shape = guide_legend("Effect",order=1,nrow=6,ncol=1))
  
  if (n.mediator>10) {
    height.temp=n.mediator*2.5
  } else if (n.mediator<=10) {
    height.temp=n.mediator*3
  }
  
  ggsave(plot=forest,
         filename=paste0(DIR.output,"/Forestplot-sample-mediation.png"), 
         width = 30, height = height.temp, units="cm",device='png', dpi=300)
}
########################################


}