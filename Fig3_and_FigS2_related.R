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
##Fig3A
Fig.3A<- ggplot(fig3_a_dotplot,aes(x=(tgy-1)*100,y=(agy-1)*100,color=grp,size=grp1))+
  geom_point(alpha=1,show.legend =F,shape=6)+
  geom_abline(slope = 0,intercept = 0,color="gray",linetype="longdash")+
  geom_abline(slope = 1,intercept = 0,color="gray",linetype="longdash")+
  geom_vline(xintercept = (0),color="gray",linetype="longdash")+
  scale_x_continuous(limits = c(-0.04,0.04)*100)+
  scale_y_continuous(limits = c(-0.04,0.04)*100)+
  labs(x="",y="")+
  scale_size_manual(guide=F,values=c(0.5,1),)+
  scale_color_manual("Expression effect",values = c("#E41A1C","blue","#808A87"))+
  theme_bw()+
  theme_classic()+
  theme(legend.background = element_rect(colour = "transparent", fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(-0.02,0.02)*100, y=c(0.03,-0.03)*100, 
           label=c("589","422"),
           color=c("blue","#E41A1C"),size=2.5)
##Fig3B
Fig.3B <- ggplot(fig3_Y_a_dotplot,aes(x=(tgy-1)*100,y=(agy-1)*100,color=grp,size=grp1))+
  geom_point(alpha=1,show.legend =F,shape=6)+
  geom_abline(slope = 0,intercept = 0,color="gray",linetype="longdash")+
  geom_abline(slope = 1,intercept = 0,color="gray",linetype="longdash")+
  geom_vline(xintercept = (0),color="gray",linetype="longdash")+
  scale_x_continuous(limits = c(-0.04,0.04)*100)+
  scale_y_continuous(limits = c(-0.04,0.04)*100)+
  labs(x="",y="")+
  scale_size_manual(guide=F,values=c(0.5,1),)+
  scale_color_manual("Expression effect",values = c("#E41A1C","blue","#808A87"))+
  theme_bw()+
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(-0.02,0.02)*100, y=c(0.03,-0.03)*100, 
           label=c("480","229"),
           color=c("blue","#E41A1C"),size=2.5)
##Fig3C
Fig.3C  <- ggplot(fig3_S_a_dotplot,aes(x=(tgy-1)*100,y=(agy-1)*100,color=grp,size=grp1))+
  geom_point(alpha=1,show.legend =F,shape=6)+
  geom_abline(slope = 0,intercept = 0,color="gray",linetype="longdash")+
  geom_abline(slope = 1,intercept = 0,color="gray",linetype="longdash")+
  geom_vline(xintercept = (0),color="gray",linetype="longdash")+
  scale_x_continuous(limits = c(-0.04,0.04)*100)+
  scale_y_continuous(limits = c(-0.04,0.04)*100)+
  labs(x="",y="")+
  scale_size_manual(guide=F,values=c(0.5,1),)+
  scale_color_manual("Expression effect",values = c("#E41A1C","blue","#808A87"))+
  theme_bw()+
  theme_classic()+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(-0.02,0.02)*100, y=c(0.03,-0.03)*100, 
           label=c("391","474"),
           color=c("blue","#E41A1C"),size=2.5)
##Fig3D
Fig.3D <- ggplot(fig3_U_a_dotplot,aes(x=(tgy-1)*100,y=(agy-1)*100,color=grp,size=grp1))+
  geom_point(alpha=1,show.legend =F,shape=6)+
  geom_abline(slope = 0,intercept = 0,color="gray",linetype="longdash")+
  geom_abline(slope = 1,intercept = 0,color="gray",linetype="longdash")+
  geom_vline(xintercept = (0),color="gray",linetype="longdash")+
  scale_x_continuous(limits = c(-0.04,0.04)*100)+
  scale_y_continuous(limits = c(-0.04,0.04)*100)+
  labs(x="",y="")+
  scale_size_manual(guide=F,values=c(0.5,1),)+
  scale_color_manual("Expression effect",values = c("#E41A1C","blue","#808A87"))+
  theme_bw()+
  theme_classic()+
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(-0.02,0.02)*100, y=c(0.02,-0.02)*100, 
           label=c("267","548"),
           color=c("blue","#E41A1C"),size=2.5) 
##Fig3E
Fig.3E <- ggplot() + 
  stat_bin(data=tgy_agy_bene,aes(x=V1*100,y =..count..),position="identity",
           breaks=seq(-2,2,by=0.08),geom="bar",fill="blue", alpha=0.7)+
  stat_bin(data=tgy_agy_dele,aes(x=V1*100,y =-..count..),position="identity",
           breaks=seq(-2,2,by=0.08),geom="bar",fill="green", alpha=0.7)+
  scale_x_continuous(limits = c(-0.02,0.02)*100)+
  scale_y_continuous(limits = c(-170,100),breaks=c(-150,-100,-50,0,50,100),labels = c("150","100","50","0","50","100"))+
  labs(y=" ",x=" ")+
  geom_vline(xintercept = c(0),color="gray",linetype="longdash")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(1.2,-1.15), y=c(-130,80), 
           label=c("Deleterious","Beneficial"),
           color=c("green","blue"),size=2.5)
##Fig3F
Fig.3F <- ggplot() + 
  stat_bin(data=tuy_auy_bene,aes(x=V1*100,y =..count..),position="identity",
           breaks=seq(-2,2,by=0.08),geom="bar",fill="blue", alpha=0.7)+
  stat_bin(data=tuy_auy_dele,aes(x=V1*100,y =-..count..),position="identity",
           breaks=seq(-2,2,by=0.08),geom="bar",fill="green", alpha=0.7)+
  scale_x_continuous(limits = c(-0.02,0.02)*100)+
  scale_y_continuous(limits = c(-170,100),breaks=c(-150,-100,-50,0,50,100),labels = c("150","100","50","0","50","100"))+
  labs(x=" ",y=" ")+
  geom_vline(xintercept = c(0),color="gray",linetype="longdash")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(1.3,-1.15), y=c(-130,80), 
           label=c("Deleterious","Beneficial"),
           color=c("green","blue"),size=2.5)
##Fig3G
Fig.3G <- ggplot() + 
  stat_bin(data=tus_aus_bene,aes(x=V1*100,y =..count..),position="identity",
           breaks=seq(-2,2,by=0.08),geom="bar",fill="blue", alpha=0.7)+
  stat_bin(data=tus_aus_dele,aes(x=V1*100,y =-..count..),position="identity",
           breaks=seq(-2,2,by=0.08),geom="bar",fill="green", alpha=0.7)+
  scale_x_continuous(limits = c(-0.02,0.02)*100)+
  scale_y_continuous(limits = c(-170,100),breaks=c(-150,-100,-50,0,50,100),labels = c("150","100","50","0","50","100"))+
  labs(y=" ",x=" ")+
  geom_vline(xintercept = c(0),color="gray",linetype="longdash")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(1.2,-1.15), y=c(-130,80), 
           label=c("Deleterious","Beneficial"),
           color=c("green","blue"),size=2.5)
##Fig3H
Fig.3H <-  ggplot() + 
  stat_bin(data=tuu_auu_bene,aes(x=V1*100,y =..count..),position="identity",
           breaks=seq(-2,2,by=0.08),geom="bar",fill="blue", alpha=0.7)+
  stat_bin(data=tuu_auu_dele,aes(x=V1*100,y =-..count..),position="identity",
           breaks=seq(-2,2,by=0.08),geom="bar",fill="green", alpha=0.7)+
  scale_x_continuous(limits = c(-0.02,0.02)*100)+
  scale_y_continuous(limits = c(-170,100),breaks=c(-150,-100,-50,0,50,100),labels = c("150","100","50","0","50","100"))+
  labs(x="",y="")+
  geom_vline(xintercept = c(0),color="gray",linetype="longdash")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(1.2,-1.15), y=c(-130,80), 
           label=c("Deleterious","Beneficial"),
           color=c("green","blue"),size=2.5)
##Fig3I
Fig.3I <- ggplot() + 
  geom_point(data=figure_B_data,aes(x=high*100,y=minus*100),alpha=0.1,size=0.5,shape=20)+
  geom_abline(slope = 0,intercept = 0,color="gray",linetype="longdash")+
  geom_vline(xintercept = (0),color="gray",linetype="longdash")+
  scale_x_continuous(limits = c(-0.03,0.03)*100)+
  scale_y_continuous(limits = c(-0.03,0.03)*100)+
  labs(x="",y="")+theme_bw()+theme_classic()+
  theme(axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(0.012+0.007,0.0123+0.005)*100, y=c(-0.0165,-0.027)*100,
           label=c(TeX("$\\textit{ρ}=0.50$"),TeX("$\\textit{P}<10^{-128}$")),
           color=c("black","black"),size=2.5) 
##Fig3J
Fig.3J <-  ggplot() + 
  geom_point(data=figure_Y_B_data ,aes(x=high*100,y=minus*100),alpha=0.1,size=0.5,shape=20)+
  geom_abline(slope = 0,intercept = 0,color="gray",linetype="longdash")+
  geom_vline(xintercept = (0),color="gray",linetype="longdash")+
  scale_x_continuous(limits = c(-0.03,0.03)*100)+
  scale_y_continuous(limits = c(-0.03,0.03)*100)+
  labs(x="",y="")+theme_bw()+theme_classic()+
  theme(axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(0.012+0.007,0.0123+0.005)*100, y=c(-0.0165,-0.027)*100, 
           label=c(TeX("$\\textit{ρ}=0.74$"),TeX("$\\textit{P}<10^{-300}$")),
           color=c("black","black"),size=2.5)
##Fig3K
Fig.3K <- ggplot() + 
  geom_point(data=figure_SC_B_data ,aes(x=high*100,y=minus*100),alpha=0.1,size=0.5,shape=20)+
  geom_abline(slope = 0,intercept = 0,color="gray",linetype="longdash")+
  geom_vline(xintercept = (0),color="gray",linetype="longdash")+
  scale_x_continuous(limits = c(-0.03,0.03)*100)+
  scale_y_continuous(limits = c(-0.03,0.03)*100)+
  labs(x="",y="")+theme_bw()+theme_classic()+
  theme(axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(0.012+0.007,0.0123+0.005)*100, y=c(-0.0165,-0.027)*100,
           label=c(TeX("$\\textit{ρ}=0.39$"),TeX("$\\textit{P}<10^{-56}$")),
           color=c("black","black"),size=2.5)
##Fig3L
Fig.3L <- ggplot() + 
  geom_point(data=figure_UU_B_data ,aes(x=high*100,y=minus*100),alpha=0.1,size=0.5,shape=20)+
  geom_abline(slope = 0,intercept = 0,color="gray",linetype="longdash")+
  geom_vline(xintercept = (0),color="gray",linetype="longdash")+
  scale_x_continuous(limits = c(-0.03,0.03)*100)+
  scale_y_continuous(limits = c(-0.03,0.03)*100)+
  labs(x="",y="")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))+
  annotate(geom="text", x=c(0.012+0.007,0.0123+0.005)*100, y=c(-0.0165,-0.027)*100,
           label=c(TeX("$\\textit{ρ}=0.35$"),TeX("$\\textit{P}<10^{-54}$")),
           color=c("black","black"),size=2.5) 
##Fig3M
Fig.3M <- ggplot(fake_date_mix,x=exp,y=posi)+
  geom_point(data=fake_date_mix,aes(x=exp,y=log(posi+1)),size=0.75,position = "dodge",show.legend = F,color="blue")+
  labs(y=TeX("$Effect size(x10^{-2})$"),x="Expression level")+
  geom_smooth(data=fake_date_mix,aes(x=exp,y=log(posi+1)),method = lm,size=0.8,se=F,
              fullrange=TRUE,show.legend = F,color="blue")+
  
  geom_vline(xintercept = c(0),color="gray",linetype="longdash")+
  geom_vline(xintercept = c(1.15e-1),color="black",size=0.2,linetype="solid")+
  geom_hline(yintercept = c(0),color="gray",linetype="longdash")+
  facet_wrap(.~nege,strip.position="right",scales = "free_y",nrow=2,drop=TRUE)+
  labs(x="",y="")+
  scale_x_continuous(limits = c(-0.5,2.3),breaks = c(0,1,2))+
  theme_bw()+theme_classic()+
  theme(
    strip.text.x= element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x=element_text(size=14),
    axis.title.y=element_text(size=14))
##Fig3N
Fig.3N <-  ggplot(combine_intercept,aes(x = type, y = proportion,fill=intecept)) +
  geom_bar(position = "fill", stat = "identity",show.legend = F)+
  scale_x_continuous(" ",breaks = c(1,2,3,4),labels = c("GFP in YPD","URA3 in YPD","URA3 in SC","URA3 in SC-URA"))+
  scale_fill_manual("Extrapolated fitness effects",values=c("blue","green"))+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Fraction",x=" ")+
  theme_bw()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))
##Fig3O
Fig.3O <- ggplot(combine_slope, aes(x = type, y = proportion,fill=intecept)) +
  geom_bar(position = "fill", stat = "identity",show.legend = F)+
  scale_x_continuous(" ",breaks = c(1,2,3,4),labels = c("GFP in YPD","URA3 in YPD","URA3 in SC","URA3 in SC-URA"))+
  scale_fill_manual("Expression-dependent component",values=c("blue","green"))+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Fraction",x=" ")+ theme_bw()+theme_classic()+
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")))
#######################################################################################
########################################################################################

##FigS2.B
FigS2.B <- ggplot(figure_new_e_data,aes(x=xx,y=yy,fill=grp))+
  geom_bar(width=0.2,
           stat="identity",show.legend = F,alpha=0.8)+
  scale_fill_manual(values = c("red","blue"))+
  labs(y="Model preference by AIC",title=" ")+
  scale_y_continuous(limits = c(-1400,1400),breaks = c(-1000,-500,0,500,1000),labels = c(1000,500,0,500,1000))+
  scale_x_continuous(" ",breaks = c(1,2,3,4),labels = c("GFP in YPD","URA3 in YPD","URA3 in SC","URA3 in SC-URA"))+
  theme_bw()+
  theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -3,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))+coord_flip()

##FigS2.C
FigS2.C <- fig.5_2$data%>%
  filter(Gene.Name!="TDH3")%>%
  ggplot(aes(x=env,y=me,fill=Gene.Name))+
  geom_bar(position=position_dodge(0.725), stat="identity",width = 0.7,show.legend = F)+
  geom_errorbar (aes(ymin = me - sd,ymax = me + sd), 
                 width=.2,linetype="solid",alpha=1,
                 position = position_dodge(0.72),show.legend = F)+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.09))+
  theme_bw()+theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -1,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(vjust =1,hjust = 1,angle =45,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS2.D
FigS2.D <- fig.6_2$data%>%
  filter(Gene.Name!="TDH3")%>%
  ggplot(aes(x=env,y=me,fill=Gene.Name))+
  geom_bar(position=position_dodge(0.725), stat="identity",width = 0.7,show.legend = F)+
  geom_errorbar (aes(ymin = me - sd,ymax = me + sd), 
                 width=.2,linetype="solid",alpha=1,
                 position = position_dodge(0.72),show.legend = F)+
  scale_y_continuous(expand = c(0,0),limits = c(0,3))+
  theme_bw()+theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -1,size=unit(14,"pt")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(vjust =1,hjust = 1,angle =45,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS2.E
FigS2.E <- ggplot(combine_intercept_nolog , aes(x = type, y = proportion, fill=intecept)) +
  geom_bar(position = "fill", stat = "identity",show.legend = F)+
  scale_x_continuous(" ",breaks = c(1,2,3,4),labels = c("GFP in YPD","URA3 in YPD","URA3 in SC","URA3 in SC-URA"))+
  scale_fill_manual("Extrapolated fitness effects",values=c("blue","green"),
                    guide = guide_legend(title.position="left",direction="horizontal",nrow=2),
                    labels=c("Intercept>0","Intercept<0"))+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Fraction",x=" ",title = " ")+
  theme_bw()+theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -7,size=unit(14,"pt")),
        legend.key.size = unit(6,"pt"),
        legend.position = "top",
        legend.title = element_text(hjust=0.5,face="bold",size=unit(8,"pt")),
        legend.text = element_text(size=unit(8,"pt")),
        
        axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS2.F
FigS2.F <- ggplot(combine_slope_nolog, aes(x = type, y = proportion,fill=intecept)) +
  geom_bar(position = "fill", stat = "identity",show.legend = F)+
  scale_x_continuous(" ",breaks = c(1,2,3,4),labels = c("GFP in YPD","URA3 in YPD","URA3 in SC","URA3 in SC-URA"))+
  scale_fill_manual("Expression-dependent component",values=c("blue","green"),
                    guide = guide_legend(title.position="left",direction="horizontal",nrow=2),
                    labels=c("Slope>0","Slope<0"))+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Fraction",x=" ",title = " ")+
  theme_bw()+theme_classic()+
  theme(plot.title = element_text(hjust = -0.09,vjust = -7,size=unit(14,"pt")),
        legend.key.size = unit(6,"pt"),
        legend.position = "top",
        legend.title = element_text(hjust=0.5,face="bold",size=unit(8,"pt")),
        legend.text = element_text(size=unit(8,"pt")),
        axis.text.x=element_text(angle=45,vjust=1,hjust=1,size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
save(Fig.3A,Fig.3B,Fig.3C,Fig.3D,Fig.3E,Fig.3F,Fig.3G,Fig.3H,Fig.3I,Fig.3J,Fig.3K,Fig.3L,
     Fig.3M,Fig.3N,Fig.3O,FigS2.B,FigS2.C,FigS2.D,FigS2.E,FigS2.F,file = "/mnt/data2/disk/smrtanalysis/pacbio_data_new/new_illumina_data_2-14/20202015_sample_data/gfp_plot_data/Figure3.RData")
