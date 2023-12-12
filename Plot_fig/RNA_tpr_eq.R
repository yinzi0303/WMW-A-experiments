data_comb1_eq = data_comb[,c(1,2)]
data_comb1_2_eq = data_comb[,c(1,6)]
data_comb2_eq = data_comb[,c(1,7)]

##########################################################################################

data = melt(data_comb1_eq, id="Methods")


library(ggplot2)
p1=ggplot(data = data)+#,aes(x=Methods, y=value,group = Methods,color=Methods)
  geom_boxplot(aes(x=Methods, y=value,fill=Methods)) + 
  labs(x="", y="true positive rate", title="")+
  facet_wrap(~variable,scales="free",nrow = 1)+#ylim(0,1)+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#去?锟斤拷锟斤拷???色
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  # scale_color_brewer(palette = "Dark2")+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?写???????去?????????冶??????????呖?
    text = element_text(family = "sans"),#????????????????示
    # axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 70,vjust = 1,hjust = 1),
    # legend.position = c(.9,.2),#????图????位?茫?????图?诓??????辖?
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "none",
    axis.text = element_text(size=12.5),
    # axis.text = element_text(face = 'bold'),
    axis.title=element_text(size=12.5),
    strip.text=element_text(size=12.5),
    # legend.text = element_text(face = 'bold'),
    # legend.title = element_text(face = 'bold')
    # legend.box.background = element_rect(color="black"))#为图???????呖???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )+ guides(fill=guide_legend(ncol=2))


data = melt(data_comb1_2_eq, id="Methods")




library(ggplot2)
p1_2=ggplot(data = data)+#,aes(x=Methods, y=value,group = Methods,color=Methods)
  geom_boxplot(aes(x=Methods, y=value,fill=Methods)) + 
  labs(x="", y="", title="")+
  facet_wrap(~variable,scales="free",nrow = 1)+#ylim(0,1)+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#去?锟斤拷锟斤拷???色
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  # scale_color_brewer(palette = "Dark2")+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?写???????去?????????冶??????????呖?
    text = element_text(family = "sans"),#????????????????示
    # axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 70,vjust = 1,hjust = 1),
    # legend.position = c(.9,.2),#????图????位?茫?????图?诓??????辖?
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "none",
    axis.text = element_text(size=12.5),
    # axis.text = element_text(face = 'bold'),
    axis.title=element_text(size=12.5),
    strip.text=element_text(size=12.5),
    # legend.text = element_text(face = 'bold'),
    # legend.title = element_text(face = 'bold')
    # legend.box.background = element_rect(color="black"))#为图???????呖???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )+ guides(fill=guide_legend(ncol=2))



inx_del = which(data_comb2_eq$Methods=='WMW-A(normalized)')
data_comb2_eq_del = data_comb2_eq[-inx_del,]
data = melt(data_comb2_eq_del, id="Methods")




library(ggplot2)
p2=ggplot(data = data)+#,aes(x=Methods, y=value,group = Methods,color=Methods)
  geom_boxplot(aes(x=Methods, y=value,fill=Methods)) + 
  labs(x="", y="", title="")+
  facet_wrap(~variable,scales="free",nrow = 1)+#ylim(0,1)+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#去?锟斤拷锟斤拷???色
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  # scale_color_brewer(palette = "Dark2")+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?写???????去?????????冶??????????呖?
    text = element_text(family = "sans"),#????????????????示
    # axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 70,vjust = 1,hjust = 1),
    # legend.position = c(.9,.2),#????图????位?茫?????图?诓??????辖?
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "none",
    axis.text = element_text(size=12.5),
    # axis.text = element_text(face = 'bold'),
    axis.title=element_text(size=12.5),
    strip.text=element_text(size=12.5),
    # legend.text = element_text(face = 'bold'),
    # legend.title = element_text(face = 'bold')
    # legend.box.background = element_rect(color="black"))#为图???????呖???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
    
  )+ guides(fill=guide_legend(ncol=2))


# design <- "AAAAABBBBBCCCCC
#            AAAAABBBBBCCCCC
#            AAAAABBBBBCCCCC
#            AAAAABBBBBCCCCC
#            AAAAABBBBBCCCCC
#            AAAAABBBBBCCCCC
#            
# "
# library(patchwork)
# wrap_plots(A = p1, B = p1_2, C=p2, 
#            design = design)
