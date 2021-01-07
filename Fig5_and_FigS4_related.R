library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(RColorBrewer)
library(stats)
library("scales")
library(latex2exp)
library(cowplot)
library(hrbrthemes)
##Fig.5A
Fig.5A <- ggplot(coeffi,aes(x=od,y=-coeffient,fill=mic,color=mic))+
  geom_bar(position=position_dodge(1), stat="identity",width = .85,show.legend = F)+
  geom_errorbar (aes(ymin = -coeffient - sd,ymax = -coeffient + sd), 
                 width=.4,linetype="solid",alpha=1,
                 position = position_dodge(1),show.legend = F)+
  scale_color_manual("Mechanism",values=c("red","blue","green"),
                     guide = guide_legend(title.position="top",direction="horizontal"))+
  scale_fill_manual("Mechanism",values=c("red","blue","green"),
                    guide = guide_legend(title.position="top",direction="horizontal"))+
  facet_wrap(.~od,scales = "free",nrow=1,drop=TRUE)+
  theme_bw()+
  theme_classic()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.title = element_text(hjust = -0.09,vjust = -1,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(vjust =1,hjust = 1,angle =45,size=unit(10,"pt")),
        axis.text.y=element_blank(),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##Fig.5B
Fig.5B.data <- read.table("figure5_C_reverse_fraction_score.txt",sep="\t",header = T)
Fig.5B.data.melt <- melt(Fig.5B.data,c("position","wt"))
Fig.5B<- ggplot()+
  geom_bar(data=Fig.5B.data,aes(x=position,y=wt),width=1,stat="identity",show.legend = F,alpha=0.4)+
  geom_line(data=Fig.5B.data.melt,aes(x=position,y=value,color=variable),show.legend = F)+
  scale_color_manual("Mechanism",values=c("red","blue","green"),
                     guide = guide_legend(title.position="top",direction="horizontal"))+
  scale_x_continuous(limits = c(1,230),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),limits = c(-0.05,1),breaks = c(0,0.3,0.6,0.9))+
  labs(x=" ",y=" ")+
  theme_bw()+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_text(hjust=0.5,face="bold",size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.09,vjust = -5,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))+coord_flip()
##Fig.5C
Fig.5C.data<- read.table("figure5_C_reverse_fraction_cuoff_score.txt",sep="\t",header = T)
Fig.5C.data$var <- factor(Fig.5C.data$var, levels = c("misf", "idr","rna"),ordered=T)
Fig.5C <- ggplot(Fig.5C.data,aes(x=rankk/100,y=score,color=var))+
  geom_point(position=position_dodge(.1),shape=2,show.legend = F)+
  geom_errorbar(aes(ymin = score-sd,ymax = score+sd),width=.04,linetype="solid",position=position_dodge(.1),alpha=1,
                show.legend = F)+
  scale_color_manual("Mechanism",values=c("red","blue","green"),labels=c("Misfolding avoidance","Misinteraction avoidance","RNA folding requirement"))+
  scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.75,0.85))+
  labs(x=" ",y=" ")+
  scale_y_continuous(breaks = c(0.4,0.6,0.8,1),limits = c(0.2,1))+
  theme_bw()+theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -1,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(vjust =1,hjust = 1,angle =45,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
###################################################################################
###################################################################################
##FigS4.A
FigS4.A <- ggplot(URA3_Y_fig5_C_score,aes(x=rankk/100,y=score,color=var))+
  geom_point(position=position_dodge(.1),shape=2,show.legend = F)+
  geom_errorbar(aes(ymin = score-sd,ymax = score+sd),width=.04,linetype="solid",position=position_dodge(.1),alpha=1,
                show.legend = F)+
  scale_color_manual("Mechanism",values=c("red","blue","green"),labels=c("Misfolding avoidance","Misinteraction avoidance","RNA folding requirement"))+
  ylab("Strength of constraint \nfor specific mechanism")+
  xlab("Strength of overall constraint")+
  scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.75,0.85))+
  ylab(" ")+xlab(" ")+theme_bw()+
  theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -1,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(vjust =1,hjust = 1,angle =45,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS4.B
FigS4.B <- ggplot(URA3_S_fig5_C_score,aes(x=rankk/100,y=score,color=var))+
  geom_point(position=position_dodge(.1),shape=2,show.legend = F)+
  geom_errorbar(aes(ymin = score-sd,ymax = score+sd),width=.04,linetype="solid",position=position_dodge(.1),alpha=1,
                show.legend = F)+
  scale_color_manual("Mechanism",values=c("red","blue","green"),labels=c("Misfolding avoidance","Misinteraction avoidance","RNA folding requirement"))+
  scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.75,0.85))+
  ylab(" ")+
  xlab(" ")+
  theme_bw()+
  theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -1,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(vjust =1,hjust = 1,angle =45,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS4.C
FigS4.C <- ggplot(URA3_U_fig5_C_score,aes(x=rankk/100,y=score,color=var))+
  geom_point(position=position_dodge(.1),shape=2,show.legend = F)+
  geom_errorbar(aes(ymin = score-sd,ymax = score+sd),width=.04,linetype="solid",position=position_dodge(.1),alpha=1,
                show.legend = F)+
  scale_color_manual("Mechanism",values=c("red","blue","green"),labels=c("Misfolding avoidance","Misinteraction avoidance","RNA folding requirement"))+
  scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.75,0.85))+
  ylab(" ")+
  xlab(" ")+
  theme_bw()+
  theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -1,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(vjust =1,hjust = 1,angle =45,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
save(Fig.5A,Fig.5B,Fig.5C,FigS4.A,FigS4.B,FigS4.C,file = "/mnt/data2/disk/smrtanalysis/pacbio_data_new/new_illumina_data_2-14/20202015_sample_data/gfp_plot_data/Figure5.RData")

