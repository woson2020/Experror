#Fig2 code

Fig.2A <- TGY_day7_heat1 %>%
  mutate(mutInto = factor(AA,levels=c("A","T","C","G"),ordered=T)) %>%
  ggplot(aes(x=mutInto,y=pos,fill=eff_size )) +
  geom_tile() + xlab('') + ylab('') +
  scale_fill_gradient2(low="#33CC00",high="#FF9900",mid='white',midpoint = 0, limits=c(-0.01,0.01),breaks=c(-0.01,0.01,0,0.005,-0.005))+
  scale_y_continuous(expand = c(0,0),limits = c(0,733))+
  theme_bw() + labs(fill="Fitness effect") +
  theme(axis.text=element_text(size=15),
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=10),
        legend.position = 'top',
        panel.grid =element_blank(),
        panel.border= element_rect(color = 'black', size = 0.1))

Fig.2B <- AGY_day7_heat1 %>%
  mutate(mutInto = factor(AA,levels=c("A","T","C","G"),ordered=T)) %>%
  ggplot(aes(x=mutInto,y=pos,fill=eff_size )) +
  geom_tile() + xlab('') + ylab('') +
  scale_fill_gradient2(low="#33CC00",high="#FF9900",mid='white',midpoint = 0, limits=c(-0.01,0.01),breaks=c(-0.01,0.01,0,0.005,-0.005))+
  scale_y_continuous(expand = c(0,0),limits = c(0,733))+
  theme_bw() + labs(fill="Fitness effect") +
  theme(axis.text=element_text(size=15),
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=10),
        legend.position = 'top',
        panel.grid =element_blank(),
        panel.border= element_rect(color = 'black', size = 0.1))

Fig.2C <- TUY_day7_heat1 %>%
  mutate(mutInto = factor(AA,levels=c("A","T","C","G"),ordered=T)) %>%
  ggplot(aes(x=mutInto,y=pos,fill=eff_size )) +
  geom_tile() + xlab('') + ylab('') +
  scale_fill_gradient2(low="blue",high="red",mid='white',midpoint = 0)+
  scale_y_continuous(expand = c(0,0),limits = c(0,804))+
  theme_bw() + labs(fill="Fitness effect") +
  theme(axis.text=element_text(size=15),
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=10),
        legend.position = 'top',
        panel.grid =element_blank(),
        panel.border= element_rect(color = 'black', size = 0.1))


Fig.2D <- AUY_day7_heat1 %>%
  mutate(mutInto = factor(AA,levels=c("A","T","C","G"),ordered=T)) %>%
  ggplot(aes(x=mutInto,y=pos,fill=eff_size )) +
  geom_tile() + xlab('') + ylab('') +
  scale_fill_gradient2(low="blue",high="red",mid='white',midpoint = 0,
                       limits=c(-0.03,0.03),breaks=c(-0.03,-0.02,-0.01,0,0.01,0.02,0.03), 
                       labels=c(-0.03,-0.02,-0.01,0,0.01,0.02,0.03))+
  scale_y_continuous(expand = c(0,0),limits = c(0,804))+
  theme_bw() + labs(fill="Fitness effect") +
  theme(axis.text=element_text(size=15),
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=10),
        legend.position = 'top',
        panel.grid =element_blank(),
        panel.border= element_rect(color = 'black', size = 0.1))+
  scale_colour_gradient(breaks = c(-0.01,0.05))

Fig.2E <- TUS_day7_heat1 %>%
  mutate(mutInto = factor(AA,levels=c("A","T","C","G"),ordered=T)) %>%
  ggplot(aes(x=mutInto,y=pos,fill=eff_size )) +
  geom_tile() + xlab('') + ylab('') +
  scale_fill_gradient2(low="blue",high="red",mid='white',midpoint = 0)+
  scale_y_continuous(expand = c(0,0),limits = c(0,804))+
  theme_bw() + labs(fill="Fitness effect") +
  theme(axis.text=element_text(size=15),
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=10),
        legend.position = 'top',
        panel.grid =element_blank(),
        panel.border= element_rect(color = 'black', size = 0.1))

Fig.2F <- AUS_day7_heat1 %>%
  mutate(mutInto = factor(AA,levels=c("A","T","C","G"),ordered=T)) %>%
  ggplot(aes(x=mutInto,y=pos,fill=eff_size )) +
  geom_tile() + xlab('') + ylab('') +
  scale_fill_gradient2(low="blue",high="red",mid='white',midpoint = 0)+
  scale_y_continuous(expand = c(0,0),limits = c(0,804))+
  theme_bw() + labs(fill="Fitness effect") +
  theme(axis.text=element_text(size=15),
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=10),
        legend.position = 'top',
        panel.grid =element_blank(),
        panel.border= element_rect(color = 'black', size = 0.1))


Fig.2G <- TUU_day7_heat1 %>%
  mutate(mutInto = factor(AA,levels=c("A","T","C","G"),ordered=T)) %>%
  ggplot(aes(x=mutInto,y=pos,fill=eff_size )) +
  geom_tile() + xlab('') + ylab('') +
  scale_fill_gradient2(low="blue",high="red",mid='white',midpoint = 0)+
  scale_y_continuous(expand = c(0,0),limits = c(0,804))+
  theme_bw() + labs(fill="Fitness effect") +
  theme(axis.text=element_text(size=15),
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=10),
        legend.position = 'top',
        panel.grid =element_blank(),
        panel.border= element_rect(color = 'black', size = 0.1))


Fig.2H <- AUU_day7_heat1 %>%
  mutate(mutInto = factor(AA,levels=c("A","T","C","G"),ordered=T)) %>%
  ggplot(aes(x=mutInto,y=pos,fill=eff_size )) +
  geom_tile() + xlab('') + ylab('') +
  scale_fill_gradient2(low="blue",high="red",mid='white',midpoint = 0)+
  scale_y_continuous(expand = c(0,0),limits = c(0,804))+
  theme_bw() + labs(fill="Fitness effect") +
  theme(axis.text=element_text(size=15),
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15),
        legend.text=element_text(size=10),
        legend.position = 'top',
        panel.grid =element_blank(),
        panel.border= element_rect(color = 'black', size = 0.1))


Fig.2I<- ggplot() + 
  geom_abline(intercept = 0, slope = 1,color = 'grey') +
  geom_hline(aes(yintercept=0), linetype = "dashed",color = 'grey') +
  geom_vline(aes(xintercept=0), linetype = "dashed",color = 'grey') +
  geom_point(aes(x=s_nonsy_TGY, y=s_sy_TGY),size=5) +
  style.print() +
  scale_x_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02),labels = c(-0.02,0,0.02))+
  scale_y_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02))+ 
  xlab('Nonsynonymous') +ylab('Synonymous') +
  theme(axis.text.x = element_text(size=100,hjust = 0.8),
        axis.title = element_text(size=120),axis.text.y=element_blank())+
  annotate('text',parse = TRUE,label =  " ~ italic(P)< ~10^-15", x = -0.005, y = 0.017,size=50)


Fig.2J<- ggplot() + 
  geom_abline(intercept = 0, slope = 1,color = 'grey') +
  geom_hline(aes(yintercept=0), linetype = "dashed",color = 'grey') +
  geom_vline(aes(xintercept=0), linetype = "dashed",color = 'grey') +
  geom_point(aes(x=s_nonsy_AGY, y=s_sy_AGY),size=5) +
  style.print()+
  scale_x_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02),labels = c(-0.02,0,0.02))+
  scale_y_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02))+ 
  xlab('Nonsynonymous') +ylab('Synonymous') +
  theme(axis.text.x = element_text(size=100,hjust = 0.8),
        axis.title = element_text(size=120),axis.text.y=element_blank())+
  annotate('text',parse = TRUE,label =  " ~ italic(P) == 0.215", x = -0.005, y = 0.017,size=50)


Fig.2K<- ggplot() + 
  geom_abline(intercept = 0, slope = 1,color = 'grey') +
  geom_hline(aes(yintercept=0), linetype = "dashed",color = 'grey') +
  geom_vline(aes(xintercept=0), linetype = "dashed",color = 'grey') +
  geom_point(aes(x=s_nonsy_TUY, y=s_sy_TUY),size=5) +
  style.print()+
  scale_x_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02),labels = c(-0.02,0,0.02))+
  scale_y_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02))+ 
  xlab('Nonsynonymous') +ylab('Synonymous') +
  theme(axis.text.x = element_text(size=100,hjust = 0.8),
        axis.title = element_text(size=120),axis.text.y=element_blank())+
  annotate('text',parse = TRUE,label =  "~ italic(P)< ~0.02", x = -0.005, y = 0.017,size=50)


Fig.2L<- ggplot() + 
  geom_abline(intercept = 0, slope = 1,color = 'grey') +
  geom_hline(aes(yintercept=0), linetype = "dashed",color = 'grey') +
  geom_vline(aes(xintercept=0), linetype = "dashed",color = 'grey') +
  geom_point(aes(x=s_nonsy_AUY, y=s_sy_AUY),size=5) +
  style.print()+
  scale_x_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02),labels = c(-0.02,0,0.02))+
  scale_y_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02))+ 
  xlab('Nonsynonymous') +ylab('Synonymous') +
  theme(axis.text.x = element_text(size=100,hjust = 0.8),
        axis.title = element_text(size=120),axis.text.y=element_blank())+
  annotate('text',parse = TRUE,label =  "~ italic(P)< ~0.02", x = -0.005, y = 0.017,size=50)


Fig.2M<- ggplot() + 
  geom_abline(intercept = 0, slope = 1,color = 'grey') +
  geom_hline(aes(yintercept=0), linetype = "dashed",color = 'grey') +
  geom_vline(aes(xintercept=0), linetype = "dashed",color = 'grey') +
  geom_point(aes(x=s_nonsy_TUS, y=s_sy_TUS),size=5) +
  style.print()+
  scale_x_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02),labels = c(-0.02,0,0.02))+
  scale_y_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02))+ 
  xlab('Nonsynonymous') +ylab('Synonymous') +
  theme(axis.text.x = element_text(size=100,hjust = 0.8),
        axis.title = element_text(size=120),axis.text.y=element_blank())+
  annotate('text',parse = TRUE,label =  "~ italic(P)< ~10^-18", x = -0.005, y = 0.017,size=50)


Fig.2N<- ggplot() + 
  geom_abline(intercept = 0, slope = 1,color = 'grey') +
  geom_hline(aes(yintercept=0), linetype = "dashed",color = 'grey') +
  geom_vline(aes(xintercept=0), linetype = "dashed",color = 'grey') +
  geom_point(aes(x=s_nonsy_AUS, y=s_sy_AUS),size=5) +
  style.print()+
  scale_x_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02),labels = c(-0.02,0,0.02))+
  scale_y_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02))+ 
  xlab('Nonsynonymous') +ylab('Synonymous') +
  theme(axis.text.x = element_text(size=100,hjust = 0.8),
        axis.title = element_text(size=120),axis.text.y=element_blank())+
  annotate('text',parse = TRUE,label =  "~ italic(P)< ~10^-11", x = -0.005, y = 0.017,size=50)


Fig.2O<- ggplot() + 
  geom_abline(intercept = 0, slope = 1,color = 'grey') +
  geom_hline(aes(yintercept=0), linetype = "dashed",color = 'grey') +
  geom_vline(aes(xintercept=0), linetype = "dashed",color = 'grey') +
  geom_point(aes(x=s_nonsy_TUU, y=s_sy_TUU),size=5) +
  style.print()+
  scale_x_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02),labels = c(-0.02,0,0.02))+
  scale_y_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02))+ 
  xlab('Nonsynonymous') +ylab('Synonymous') +
  theme(axis.text.x = element_text(size=100,hjust = 0.8),
        axis.title = element_text(size=120),axis.text.y=element_blank())+
  annotate('text',parse = TRUE,label =  "~ italic(P)< ~10^-12", x = -0.005, y = 0.017,size=50)


Fig.2P<- ggplot() + 
  geom_abline(intercept = 0, slope = 1,color = 'grey') +
  geom_hline(aes(yintercept=0), linetype = "dashed",color = 'grey') +
  geom_vline(aes(xintercept=0), linetype = "dashed",color = 'grey') +
  geom_point(aes(x=s_nonsy_AUU, y=s_sy_AUU),size=5) +
  style.print()+
  scale_x_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02),labels = c(-0.02,0,0.02))+
  scale_y_continuous(limits = c(-0.02,0.02),breaks = c(-0.02,0,0.02))+ 
  xlab('Nonsynonymous') +ylab('Synonymous') +
  theme(axis.text.x = element_text(size=100,hjust = 0.8),
        axis.title = element_text(size=120),axis.text.y=element_blank())+
  annotate('text',parse = TRUE,label =  "~ italic(P)< ~10^-60", x = -0.005, y = 0.017,size=50)


save(Fig.2A,Fig.2B,Fig.2C,Fig.2D,Fig.2E,Fig.2F,Fig.2G,Fig.2H,Fig.2I,Fig.2J,Fig.2K,Fig.2L,Fig.2M,Fig.2N,Fig.2O,Fig.2P,
     file="~/200331_fitness/Fig2_Rdata.RData")

