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
library(Rfit)
############Fig.4C
Fig.4C <- df_misfold %>%
  ggplot(aes(x=misfolding,y=(AGY-1) * 100)) +
  scale_x_continuous(" ",limit=c(0,2)) +
  scale_y_continuous(limit=c(-0.3,0.3)) +
  labs(title=" ",x=" ",y=" ")+
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  annotate("text",x=Inf,y=-Inf,hjust=1,vjust=-0.2,size=3,color="#0000FF",
           label=paste0("rho ==0.018", "~\",\"~", prettyPvalue.plotmath(0.4399) ),parse=TRUE ) +
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TGY-1) * 100)),bins=5,size=1.5) +
  geom_smooth(aes(y=((TGY-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
  annotate("text",x=Inf,y=Inf,hjust=1,vjust=1,size=3,color="#FF0033",label=paste0("rho ==-0.121", "~\",\"~", prettyPvalue.plotmath(8.657e-08) ),parse=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
#######Fig.4D
Fig.4D <- ggplot(fig4_misf_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd), 
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),shape=2,size=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r), 
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ratio", B = "Funtional constraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"),name="Quantile")+
  labs(x="",y="")+
  theme_bw()+
  theme_classic()+
  theme(strip.text.x= element_text(size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
############Fig.4E
Fig.4E <- df_idr %>%
  ggplot(aes(x=idr,y=AGY * 100)) +
  scale_x_continuous(" ",limit=c(25,60)) +
  scale_y_continuous(limit=c(-0.45,0.45)) +
  labs(y=" ")+
  geom_point(size=1,shape=0,color="#0000FF")+
  geom_errorbar(aes(ymin = AGY* 100-AGY_sd* 100,ymax = AGY* 100+AGY_sd* 100), 
                width=.5,linetype="solid",alpha=1,color="#0000FF")+
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  annotate("text",x=Inf,y=-Inf,hjust=1,vjust=-0.2,size=3,color="#0000FF",
           label=paste0("rho ==-0.245", "~\",\"~", prettyPvalue.plotmath(0.247) ),parse=TRUE ) +
  ggnewscale::new_scale_color()+
  geom_point(aes(x=idr+0.5,y=((TGY)*100)),shape=6,size=1,color="#FF0033",position= position_dodge(.5))+
  geom_errorbar(aes(x=idr+.5,ymin = TGY*100-TGY_sd*100,ymax = TGY*100+TGY_sd*100), 
                width=.5,linetype="solid",alpha=1,color="#FF0033",position= position_dodge(.5))+
  geom_smooth(aes(x=idr+.5,y=((TGY) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
  annotate("text",x=Inf,y=Inf,hjust=1,size=3,vjust=1,color="#FF0033",label=paste0("rho ==-0.453", "~\",\"~", prettyPvalue.plotmath(0.026) ),parse=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
###Fig.4F
Fig.4F <- ggplot(fig4_idr_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd), 
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),shape=2,size=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r), 
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ratio", B = "Funtional constraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"),name="Quantile")+
  labs(x="",y="")+
  theme_bw()+
  theme_classic()+
  theme(strip.text.x= element_text(size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
#Fig.4G
Fig.4G <- df_rna %>%
  ggplot(aes(x=rnafolding,y=(AGY-1) * 100)) +
  scale_x_continuous(" ",limit=c(-175,-168)) +
  scale_y_continuous(limit=c(-0.32,0.32)) +
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  annotate("text",x=Inf,y=-Inf,hjust=1,vjust=-0.2,size=3,color="#0000FF",
           label=paste0("rho ==-0.016", "~\",\"~", prettyPvalue.plotmath(0.4721) ),parse=TRUE ) +
  labs(y=" ")+
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TGY-1) * 100)),bins=5,size=1.5) +
  geom_smooth(aes(y=((TGY-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
  annotate("text",x=Inf,y=Inf,hjust=1,vjust=1,size=3,color="#FF0033",label=paste0("rho ==-0.046", "~\",\"~", prettyPvalue.plotmath(0.040) ),parse=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
###Fig.4H
Fig.4H <- ggplot(fig4_rna_sort_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd), 
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),shape=2,size=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r), 
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  labs(x=" ",y="")+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ration", B = "Funtional constraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"),name=" ")+
  theme_bw()+
  theme_classic()+
  theme(strip.text.x= element_text(size=unit(15,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##################################################
##################################################
##FigS3.A
FigS3.A <- df_tai %>%
  ggplot(aes(x=idr,y=AGY * 100)) +
  scale_x_continuous(" ",limit=c(-0.75,.75)) +
  scale_y_continuous(limit=c(-1.3,0.8)) +
  labs(y=" ")+
  geom_point(size=1,shape=0,color="#0000FF")+
  geom_errorbar(aes(ymin = AGY* 100-AGY_sd* 100,ymax = AGY* 100+AGY_sd* 100), 
                width=.01,linetype="solid",alpha=1,color="#0000FF")+
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  ggnewscale::new_scale_color()+
  geom_point(aes(x=idr+0.01,y=((TGY)*100)),shape=6,size=1,color="#FF0033",position= position_dodge(.5))+
  geom_errorbar(aes(x=idr+.01,ymin = TGY*100-TGY_sd*100,ymax = TGY*100+TGY_sd*100), 
                width=.01,linetype="solid",alpha=1,color="#FF0033",position= position_dodge(.5))+
  geom_smooth(aes(x=idr+.01,y=((TGY) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.B
FigS3.B <- ggplot(fig4_tai_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd), 
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),shape=2,size=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r), 
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ratio", B = "Funtional contsraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"),name=" ")+
  labs(y="",X=" ")+
  theme_bw()+
  theme_classic()+
  theme(strip.text.x= element_text(size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.C
FigS3.C <- df_hdy %>%
  ggplot(aes(x=misfolding,y=(AGY) * 100)) +
  scale_y_continuous(limit=c(-0.3,0.3)) +
  labs(title=" ",x=" ",y=" ")+
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=4,color="#0000FF",se=F) +
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TGY) * 100)),bins=5,size=1.5) +
  geom_smooth(aes(y=((TGY) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.D
FigS3.D <- ggplot(fig4_hyd_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd), 
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),shape=2,size=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r), 
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ratio", B = "Funtional contsraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"),name=" ")+
  labs(y="",x=" ")+
  theme_bw()+
  theme_classic()+
  theme(strip.text.x= element_text(size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.D
FigS3.D <- ggplot(fig4_hyd_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd), 
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),shape=2,size=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r), 
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ratio", B = "Funtional contsraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"),name=" ")+
  labs(y="",x=" ")+
  theme_bw()+
  theme_classic()+
  theme(strip.text.x= element_text(size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.E
FigS3.E <- URA3_df_misfold %>%
  ggplot(aes(x=misfolding,y=(AUY-1) * 100)) +
  scale_x_continuous(limit=c(0,2)) +
  scale_y_continuous(limit=c(-1.5,0.9)) +
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(x="",y="")+
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TUY-1) * 100)),bins=5,size=1.5) +
geom_smooth(aes(y=((TUY-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.F
FigS3.F <- URA3_Y_df_idr %>%
  ggplot(aes(x=idr,y=(AUY-1) * 100)) +
scale_x_continuous(" ",limit=c(7,33)) +
  scale_y_continuous(limits = c(-2,3)) +
  geom_point(size=2,shape=0,color="#0000FF")+
  geom_errorbar(aes(ymin = (AUY-1)* 100-AUY_sd* 100,ymax = (AUY-1)* 100+AUY_sd* 100), 
                width=.5,linetype="solid",alpha=1,color="#0000FF")+
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(y=" ")+
  ggnewscale::new_scale_color()+
  geom_point(aes(x=idr+0.5,y=((TUY-1)*100)),shape=6,size=2,color="#FF0033",position= position_dodge(.5))+
  geom_errorbar(aes(x=idr+.5,ymin = (TUY-1)*100-TUY_sd*100,ymax = (TUY-1)*100+TUY_sd*100), 
                width=.5,linetype="solid",alpha=1,color="#FF0033",position= position_dodge(.5))+
  geom_smooth(aes(x=idr+.5,y=((TUY-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.G
FigS3.G <- URA3_Y_df_rna %>%
  ggplot(aes(x=rnafolding,y=(AUY-1) * 100)) +
  scale_x_continuous(limit=c(-251,-243)) +
  scale_y_continuous(limit=c(-1.5,0.9)) +
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(x=" ",y=" ")+
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TUY-1) * 100)),bins=5,size=1.5) +
geom_smooth(aes(y=((TUY-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.H
FigS3.H <- ggplot(fig4_Y_misf_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd),
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),size=2,shape=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r),
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ration", B = "Funtional contsraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"))+
  labs(y="")+
  theme_bw()+theme_classic()+
  theme(strip.text.x= element_text(size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.I
FigS3.I <- ggplot(fig4_Y_idr_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd),
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),shape=2,size=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r),
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ration", B = "Funtional contsraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"),name=" ")+
  labs(y=" ")+
  theme_bw()+theme_classic()+
  theme(strip.text.x= element_text(size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.J
FigS3.J <- ggplot(fig4_Y_rna_C,aes(x=od,y=mean,fill=drug))+
  geom_bar(position=position_dodge(.52), width=.5,
           stat="identity",show.legend = F)+
  geom_errorbar(aes(ymin = mean-sd,ymax = mean+sd),
                width=.2,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  geom_point(aes(y=ratio),shape=2,size=2,show.legend = F)+
  geom_errorbar(aes(ymin = ratio-sd_r,ymax =ratio+sd_r),
                width=.1,linetype="solid",alpha=1,
                position = position_dodge(0.5),show.legend = F)+
  scale_fill_manual("Exp_level",values=c("#FF0033","#0000FF"))+
  geom_text(aes(label=p,y=pp+0.1))+
  facet_grid(is_n~.,switch="y",scales = "free_y",labeller = as_labeller(c(A = "Ration", B = "Funtional contsraint")))+
  scale_x_continuous(breaks=c(1,2,3,4),labels =c("0-25%","25-50%","50-75%","75-100%"),name=" ")+
  labs(y=" ")+
  theme_bw()+theme_classic()+
  theme(strip.text.x= element_text(size=unit(8,"pt")),
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.K
FigS3.K <- URA3_S_df_misfold %>%
  ggplot(aes(x=misfolding,y=(AUS-1) * 100)) +
  scale_x_continuous(" ",limit=c(0,2)) +
  scale_y_continuous(limit=c(-0.7,0.4)) +
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(y="")+
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TUS-1) * 100)),bins=5,size=1.5) +
geom_smooth(aes(y=((TUS-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +

theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.L
FigS3.L <-  URA3_S_df_idr %>%
  ggplot(aes(x=idr,y=(AUS-1) * 100)) +
  scale_x_continuous(" ",limit=c(7,33)) +
  geom_point(size=2,color="#0000FF",shape=0)+
  geom_errorbar(aes(ymin = (AUS-1)* 100-AUS_sd* 100,ymax = (AUS-1)* 100+AUS_sd* 100),
                width=.5,linetype="solid",alpha=1,color="#0000FF")+
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(y=" ")+
  ggnewscale::new_scale_color()+
  geom_point(aes(x=idr+0.5,y=((TUS-1)*100)),shape=6,size=2,color="#FF0033",position= position_dodge(.5))+
  geom_errorbar(aes(x=idr+.5,ymin = (TUS-1)*100-TUS_sd*100,ymax = (TUS-1)*100+TUS_sd*100), 
                width=.5,linetype="solid",alpha=1,color="#FF0033",position= position_dodge(.5))+
  geom_smooth(aes(x=idr+.5,y=((TUS-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.M
FigS3.M <- URA3_S_df_rna %>%
  ggplot(aes(x=rnafolding,y=(AUS-1) * 100)) +
  scale_x_continuous(" ",limit=c(-251,-243)) +
  scale_y_continuous(limit=c(-0.7,0.4)) +
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(y=" ")+
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TUS-1) * 100)),bins=5,size=1.5) +
  geom_smooth(aes(y=((TUS-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
  
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.N
FigS3.N <- URA3_U_df_misfold %>%
  ggplot(aes(x=misfolding,y=(AUU-1) * 100)) +
  scale_x_continuous(" ",limit=c(0,2)) +
  scale_y_continuous(limit=c(-0.7,0.4)) +
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(y=" ")+
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TUU-1) * 100)),bins=5,size=1.5) +
geom_smooth(aes(y=((TUU-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.O
FigS3.O <- URA3_U_df_idr %>%
  ggplot(aes(x=idr,y=(AUU-1) * 100)) +
  scale_x_continuous(" ",limit=c(7,33)) +
  scale_y_continuous(limits = c(-5.5,4)) +
  geom_point(size=2,color="#0000FF",shape=0)+
  geom_errorbar(aes(ymin = (AUU-1)* 100-AUU_sd* 100,ymax = (AUU-1)* 100+AUU_sd* 100), 
                width=.5,linetype="solid",alpha=1,color="#0000FF")+
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(y=" ")+
  ggnewscale::new_scale_color()+
  geom_point(aes(x=idr+0.5,y=((TUU-1)*100)),shape=6,size=2,color="#FF0033",position= position_dodge(.5))+
  geom_errorbar(aes(x=idr+.5,ymin = (TUU-1)*100-TUU_sd*100,ymax = (TUU-1)*100+TUU_sd*100), 
                width=.5,linetype="solid",alpha=1,color="#FF0033",position= position_dodge(.5))+
  geom_smooth(aes(x=idr+.5,y=((TUU-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
##FigS3.P
FigS3.P <- URA3_U_df_rna %>%
  ggplot(aes(x=rnafolding,y=(AUU-1) * 100)) +
scale_x_continuous(" ",limit=c(-251,-243)) +
  scale_y_continuous(limit=c(-0.7,0.4)) +
  scale_color_gradientn(colors=color.agp1.ramp) +
  stat_density_2d(aes(color=after_stat(level)),bins=5) +
  geom_smooth(method="lm", linetype=2,color="#0000FF",se=F) +
  labs(y=" ")+
  ggnewscale::new_scale_color()+
  scale_color_gradientn(colors=color.tdh3.ramp) +
  stat_density_2d(aes(color=after_stat(level),y=((TUU-1) * 100)),bins=5,size=1.5) +
geom_smooth(aes(y=((TUU-1) * 100)),method="lm", linetype=2,color="#FF0033",se=F,size=1.5) +
theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = -0.03,vjust = -1,size=unit(14,"pt")),
        axis.text.x=element_text(size=unit(10,"pt")),
        axis.text.y=element_text(size=unit(10,"pt")),
        axis.title.x=element_text(size=unit(12,"pt")),
        axis.title.y=element_text(size=unit(12,"pt")))
save(Fig.4C,Fig.4D,Fig.4E,Fig.4F,Fig.4G,Fig.4H,FigS3.A,FigS3.B,FigS3.C,FigS3.D,FigS3.E,FigS3.F,
     FigS3.G,FigS3.H,FigS3.I,FigS3.J,FigS3.K,FigS3.L,FigS3.M,FigS3.N,FigS3.O,FigS3.P,file = "/mnt/data2/disk/smrtanalysis/pacbio_data_new/new_illumina_data_2-14/20202015_sample_data/gfp_plot_data/Figure4.RData")
