rm(list=ls())
library(data.table)
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
df4corr=fread(file=paste0(DIR.data,"/sample_df4corr.csv"))
######################################################

######################################################
# plot
df4corr[,pheno:=ifelse(PRS%like%"CBH2226HOIR","HOMA-IR",
                       ifelse(PRS%like%"CGL2226FAST","fasting glucose",NA))]

df4corr[,sample:=ifelse(PRS%like%"Paternal","Paternal PRS",
                       ifelse(PRS%like%"Maternal","Maternal PRS",
                              ifelse(PRS%like%"Child","Child PRS",NA)))]
df4corr[,PRS:=paste0("PRS for child ",pheno,"\nbased on GWAS on ",GWAS_trait)]

df4corr.long=melt(df4corr, id.vars = c("PRS","GWAS_trait","sample"),
             measure.vars = c("corr_CBH2226HOIR", "corr_CGL2226FAST"))
df4corr.long[,label:=round(value,3)]
df4corr.long=df4corr.long[order(GWAS_trait),]

df4corr.long[,variable:=ifelse(variable=="corr_CBH2226HOIR","HOMA-IR",
                               ifelse(variable=="corr_CGL2226FAST","Fasting glucose",NA))]
df4corr.long[,variable:=factor(variable,levels = c("HOMA-IR","Fasting glucose"))]

write.csv(df4corr.long,file=paste0(DIR.output,"/descriptive-correlation_table.csv"),row.names = F)

plot=ggplot(df4corr.long, aes(y = PRS, x = variable, fill = label)) +
  geom_tile(color="gray",linewidth = 1, width = 0.95, height = 0.95) +
  facet_grid(. ~ sample) +
  geom_text(aes(label=label),size=8) +
  coord_equal() +
  scale_x_discrete(paste0("Child glycaemic traits"))+
  scale_y_discrete(paste0(""))+
  scale_fill_gradient2("Correlation",
                       low = "red",
                       mid = "white",
                       high ="green",
                       midpoint = 0,
                       na.value = "gray") +
  theme_bw()+
  theme(axis.text.x=element_text(angle=90,size=20),
        axis.text.y=element_text(size=20),
        axis.title = element_text(size=25),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        strip.text.x = element_text(size = 15))

  
  png(file=paste0(DIR.output,"/descriptive-correlation_plot.png"),
      width=35, height=25,units="cm",res=300)
  plot(plot)
  dev.off()
  


######################################################



