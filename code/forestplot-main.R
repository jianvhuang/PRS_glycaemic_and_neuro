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
df4plot=fread(file=paste0(DIR.data,"/sample_mainresult.csv"))

strata.temp="SexStratified"
list.stratification=c("female","male","overall")
n.strata=length(list.stratification)

df4plot[,ExpoTrait:=ifelse(exposure%like%"CBH2226HOIR","CBH2226HOIR",
                           ifelse(exposure%like%"CGL2226FAST","CGL2226FAST",NA))]
df4plot[,ExpoSample:=ifelse(exposure%like%"Child","Child",
                            ifelse(exposure%like%"Maternal","Maternal",
                                   ifelse(exposure%like%"Paternal","Paternal",NA)))]
######################################################

######################################################
# order and reference
df4plot[,group4facet:=paste0(ExpoTrait,"-",ExpoSample)]

df4plot.order=NA
for (p in unique(df4plot$group4facet)){
  df4plot.p=df4plot[group4facet==p,]
  df4plot.p=df4plot.p[order(outcome,stratification)]
  
  yRef=1:(length(unique(df4plot.p$outcome))*n.strata+length(unique(df4plot.p$outcome))-1)
  yRef=yRef[yRef%%(n.strata+1)!=0]
  
  df4plot.p[,yRef:=rev(yRef)]
  df4plot.order=rbind(df4plot.order,df4plot.p,fill=T)
}
df4plot.order[,x:=NULL]
df4plot.order=df4plot.order[is.na(yRef)==F,]

strata4break=list.stratification[round(n.strata/2)]
yBreak=unique(c(df4plot.order[stratification==strata4break,]$yRef))
######################################################


######################################################
# significance
list.exposure=c("PRS_T2D_Child_SexStratified_CBH2226HOIR_CplusT","PRS_T2D_Paternal_Male_CBH2226HOIR_CplusT","PRS_T2D_Maternal_Female_CBH2226HOIR_CplusT",
                "PRS_T2D_Child_SexStratified_CGL2226FAST_CplusT","PRS_T2D_Paternal_Male_CGL2226FAST_CplusT","PRS_T2D_Maternal_Female_CGL2226FAST_CplusT")

list.outcome=c("Perceptual.reasoning.composite.score_z",
               "wiat3_avg_ss_z")

nTest=length(list.exposure)*length(list.outcome)*n.strata

df4plot.order[,sig:=ifelse(pvalue.robust<0.05/nTest,"**",
                                ifelse(pvalue.robust<0.05&pvalue.robust>=0.05/nTest,"*",""))]
df4plot.order[,sigColour:=ifelse(sig%in%c("**"),"red","blue")]
df4plot.order[,sigColour:=factor(sigColour,levels = c("red","blue"))]
df4plot.order[,sigSize:=ifelse(sig%in%c("**","*"),4,2)]

nSize=length(unique(df4plot.order$sigSize))
nColour=length(unique(df4plot.order$sigColour))
######################################################


######################################################
# axis element
xMin=min(c(df4plot.order$lower95_tzrobust,df4plot.order$upper95_tzrobust),na.rm=T)
xMax=max(c(df4plot.order$lower95_tzrobust,df4plot.order$upper95_tzrobust),na.rm=T)

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

# xLimit.Min=-6
# xLimit.Max=6

df4plot.order[,group4yaxis:=paste(outcome,stratification)]
unique(df4plot.order$group4yaxis)
df4plot.order$group4yaxis=factor(df4plot.order$group4yaxis,levels=c(rev(unique(df4plot.order$group4yaxis))))

df4plot.order.yRef.ribbon=unique(df4plot.order[,.(outcome,group4yaxis,yRef)])
df4plot.order.yRef.ribbon[,yMin:=yRef-0.5]
df4plot.order.yRef.ribbon[,yMax:=yRef+0.5]
df4plot.order.yRef.ribbon[,ribbonfill:=rep(c("a","b"),
                                                each=n.strata,
                                                length.out=nrow(df4plot.order.yRef.ribbon))]

df4plot.order.yRef.ribbon.xMin=copy(df4plot.order.yRef.ribbon[,x:=xLimit.Min])
df4plot.order.yRef.ribbon.xMax=copy(df4plot.order.yRef.ribbon[,x:=xLimit.Max])
df4plot.order.yRef.ribbon=rbind(df4plot.order.yRef.ribbon.xMin,
                                     df4plot.order.yRef.ribbon.xMax)
######################################################


######################################################
# labelling
list.facet.x=c("PRS for HOMA-IR","PRS for fasting glucose")
names(list.facet.x)=unique(df4plot.order$ExpoTrait)

list.facet.y=c("Child PRS","Maternal PRS","Paternal PRS")
names(list.facet.y)=unique(df4plot.order$ExpoSample)

df4plot.order[,stratification:=factor(stratification,
                                                    levels=c("female","male","overall"),
                                                    labels=c("Female","Male","Overall"))]

df4plot.order[,outcome:=factor(outcome,
                                             levels=c("Perceptual.reasoning.composite.score_z",
                                                      "wiat3_avg_ss_z"),
                                             labels=c("WASI-II (yr7)\nPerceptual reasoning",
                                                      "WIAT-III (yr9)\nMean score"))]

yLabel=c("WASI-II (yr7)\nPerceptual reasoning",
         "WIAT-III (yr9)\nMean score")
######################################################


######################################################
# plot
nSize
nColour

########################################
if (nSize==2 & nColour==2){
  forest <- ggplot(data=df4plot.order,aes(x=tzvalue.robust,y=yRef))+
    facet_grid(ExpoSample~ ExpoTrait,  labeller = labeller(ExpoTrait = list.facet.x, ExpoSample = list.facet.y)) +
    geom_point(aes(shape=stratification,size=sigSize,colour=sigColour)) +
    #geom_point(aes(shape=stratification,colour=sigColour),size=1) +
    geom_errorbarh(aes(xmin=lower95_tzrobust,xmax=upper95_tzrobust),height=0)+
    geom_vline(xintercept=0,linetype=2) +
    scale_y_continuous(limits=c(0,max(df4plot.order$yRef)+1),
                       breaks=c(yBreak),
                       labels=c(yLabel),
                       expand=c(0.05,0)) +
    scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
                       expand=c(0.02,0)) +
    # scale_x_continuous(limits=c(-0.52,0.2),
    #                    breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2),
    #                    labels=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2))+
    scale_colour_manual(values = c("#f8766d","#00b0f6"), guide="none") +
    # scale_colour_manual(values = c("#00b0f6"),guide="none") + 
    scale_size_continuous(range = c(1, 2),guide = "none") +
    geom_ribbon(data=df4plot.order.yRef.ribbon,
                aes(x=x,ymin = yMin, ymax = yMax,fill=ribbonfill,group=yRef),alpha=0.2,inherit.aes = FALSE) +
    scale_fill_manual(values =c("NA","azure3"),guide="none") +
    xlab("Z-value") +
    ylab("") +
    theme_classic()+
    theme(plot.title = element_text(vjust=1, face="bold"),
          axis.text = element_text(color="black",size=12),
          axis.title = element_text(color="black",size=14),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position="bottom",
          legend.key = element_blank(),
          legend.text = element_text(color="black",size=10))+
    guides(shape=guide_legend(title="Stratification",nrow=1,byrow=TRUE))
  
  ggsave(plot=forest,
         filename=paste0(DIR.output,"/ForestPlot-sample-",strata.temp,"-main.png"), 
         width = 20, height = 20, units="cm",device='png', dpi=300)
}
########################################


########################################
if (nSize==2 & nColour==1){
  forest <- ggplot(data=df4plot.order,aes(x=tzvalue.robust,y=yRef))+
    facet_grid(ExpoSample~ ExpoTrait,  labeller = labeller(ExpoTrait = list.facet.x, ExpoSample = list.facet.y)) +
    geom_point(aes(shape=stratification,size=sigSize,colour=sigColour)) +
    #geom_point(aes(shape=stratification,colour=sigColour),size=1) +
    geom_errorbarh(aes(xmin=lower95_tzrobust,xmax=upper95_tzrobust),height=0)+
    geom_vline(xintercept=0,linetype=2) +
    scale_y_continuous(limits=c(0,max(df4plot.order$yRef)+1),
                       breaks=c(yBreak),
                       labels=c(yLabel),
                       expand=c(0.05,0)) +
    scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
                       expand=c(0.02,0)) +
    # scale_x_continuous(limits=c(-0.52,0.2),
    #                    breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2),
    #                    labels=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2))+
    # scale_colour_manual(values = c("#f8766d","#00b0f6"), guide="none") +
     scale_colour_manual(values = c("#00b0f6"),guide="none") + 
    scale_size_continuous(range = c(1, 2),guide = "none") +
    geom_ribbon(data=df4plot.order.yRef.ribbon,
                aes(x=x,ymin = yMin, ymax = yMax,fill=ribbonfill,group=yRef),alpha=0.2,inherit.aes = FALSE) +
    scale_fill_manual(values =c("NA","azure3"),guide="none") +
    xlab("Z-value") +
    ylab("") +
    theme_classic()+
    theme(plot.title = element_text(vjust=1, face="bold"),
          axis.text = element_text(color="black",size=12),
          axis.title = element_text(color="black",size=14),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position="bottom",
          legend.key = element_blank(),
          legend.text = element_text(color="black",size=10))+
    guides(shape=guide_legend(title="Stratification",nrow=1,byrow=TRUE))
  
  ggsave(plot=forest,
         filename=paste0(DIR.output,"/ForestPlot-sample-",strata.temp,"-main.png"), 
         width = 20, height = 20, units="cm",device='png', dpi=300)
}
########################################



########################################
if (nSize==1 & nColour==1){
  forest <- ggplot(data=df4plot.order,aes(x=tzvalue.robust,y=yRef))+
    facet_grid(ExpoSample~ ExpoTrait,  labeller = labeller(ExpoTrait = list.facet.x, ExpoSample = list.facet.y)) +
    #geom_point(aes(shape=stratification,size=sigSize,colour=sigColour)) +
    geom_point(aes(shape=stratification,colour=sigColour),size=1) +
    geom_errorbarh(aes(xmin=lower95_tzrobust,xmax=upper95_tzrobust),height=0)+
    geom_vline(xintercept=0,linetype=2) +
    scale_y_continuous(limits=c(0,max(df4plot.order$yRef)+1),
                       breaks=c(yBreak),
                       labels=c(yLabel),
                       expand=c(0.05,0)) +
    scale_x_continuous(limits=c(xLimit.Min,xLimit.Max),
                       expand=c(0.02,0)) +
    # scale_x_continuous(limits=c(-0.52,0.2),
    #                    breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2),
    #                    labels=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2))+
    # scale_colour_manual(values = c("#f8766d","#00b0f6"), guide="none") +
    scale_colour_manual(values = c("#00b0f6"),guide="none") + 
    scale_size_continuous(range = c(1, 2),guide = "none") +
    geom_ribbon(data=df4plot.order.yRef.ribbon,
                aes(x=x,ymin = yMin, ymax = yMax,fill=ribbonfill,group=yRef),alpha=0.2,inherit.aes = FALSE) +
    scale_fill_manual(values =c("NA","azure3"),guide="none") +
    xlab("Z-value") +
    ylab("") +
    theme_classic()+
    theme(plot.title = element_text(vjust=1, face="bold"),
          axis.text = element_text(color="black",size=12),
          axis.title = element_text(color="black",size=14),
          axis.line.x = element_line(color="black", linewidth = 0.5),
          axis.line.y = element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position="bottom",
          legend.key = element_blank(),
          legend.text = element_text(color="black",size=10))+
    guides(shape=guide_legend(title="Stratification",nrow=1,byrow=TRUE))
  
  ggsave(plot=forest,
         filename=paste0(DIR.output,"/ForestPlot-sample-",strata.temp,"-main.png"), 
         width = 20, height = 20, units="cm",device='png', dpi=300)
}
########################################

######################################################




