
#######################################################################################################

result<-readRDS("Real_data/MESC/result_comb.rds") 
# result = result_comb[["result"]]
result[["rate3"]][["ROTS"]] = NULL
result[["rate4"]][["ROTS"]] = NULL

# name = c("monocle","SCDE","MAST","DESeq","Limma","edgeR","ttest","ttestR","Welch","WMW","WMW-A-G","WMW-A-GMM",
#          "WMW-A-NB","WMW-A-NBM","WMW-A(normalized)","WMW-A(count)")

name_methods = c("Monocle","SCDE","MAST","DESeq","Limma","edgeR","ttest","ttestR","Welch","WMW",
                 "WMW-A-G","WMW-A-GMM","WMW-A-NB","WMW-A-NBM","WMW-A(normalized)","WMW-A(count)",
                 'DEsingle','ZINB-WaVE',"ZIAQ","consexpression","BaySeq","NOISeq","DEGnext")

list_names = c("Limma","ttest","ttestR","Welch","WMW",
               "BaySeq","consexpression","DEGnext","DESeq","edgeR","NOISeq",
               'DEsingle',"MAST","Monocle","SCDE",'ZINB-WaVE','ZIAQ',
               "WMW-A-G","WMW-A-GMM","WMW-A-NB","WMW-A-NBM","WMW-A(normalized)","WMW-A(count)")



my_list = result[["rate3"]]
names(my_list) = name_methods
my_list_tmp = my_list[list_names]


data_comb = NULL

for(r in 1:8){
  compb = matrix(NA, ncol=length(my_list_tmp),nrow = 100)
  for (i in 1:length(my_list_tmp)){
    comp = matrix(unlist(my_list_tmp[[i]][,r]), ncol=1) 
    compb[1:length(comp),i] = comp
  }
  colnames(compb) = list_names
  
  library("reshape2")
  a = melt(compb)
  a = a[,-1]
  
  if(is.null(data_comb)){
    data_comb = a
  }else{
    data_comb = cbind(data_comb,a[,2])
  }
  
}

num = 0 

for (i in 1:(length(result[[4]]))){
  num[i+1] = num[i] + dim(result[[4]][[i]])[1]
}
num = num[-1]





library(reshape2)
library(ggplot2)

colnames(data_comb) = c("Methods","m=n=5","m=5,n=10","m=5,n=15","m=5,n=20","m=n=10",
                        "m=n=15","m=n=20","m=n=25")

data_comb1 = data_comb[,c(1,2,6,7,8)]
data_comb2 = data_comb[,c(1,3,4,5,9)]

data = melt(data_comb1, id="Methods")
# idx = which(is.na(data[,3])==TRUE)
# data = data[-idx,]
idx2 = which(data$Methods=="ZIAQ")
data = data[-idx2,]

ggplot(data = data)+#,aes(x=Methods, y=value,group = Methods,color=Methods)
  geom_boxplot(aes(x=Methods, y=value,fill=Methods)) + 
  labs(x="", y="true positive rate", title="")+
  facet_wrap(~variable,scales="free",nrow = 1)+#ylim(0,0.5)+
  # xlab("n")+#
  # ylab("Power")+#
  theme_bw() +#
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  # scale_color_brewer(palette = "Dark2")+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
    text = element_text(family = "sans"),#????????????????ʾ
    axis.text = element_text(size=12.5),
    # axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 70,vjust = 1,hjust = 1),
    # legend.position = c(.9,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "none",
    # axis.text = element_text(face = 'bold'),
    axis.title=element_text(size=12.5),
    strip.text=element_text(size=12.5),
    # legend.text = element_text(face = 'bold'),
    # legend.title = element_text(face = 'bold')
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )
# + guides(fill=guide_legend(ncol=2))

ggsave("Figure/Out_fig/MESC_tpr_eq.png",width = 16,height = 5.5)


data = melt(data_comb2, id="Methods")
# idx = which(is.na(data[,3])==TRUE)
# data = data[-idx,]
idx2 = which(data$Methods=="ZIAQ")
data = data[-idx2,]

ggplot(data = data)+#,aes(x=Methods, y=value,group = Methods,color=Methods)
  geom_boxplot(aes(x=Methods, y=value,fill=Methods)) + 
  labs(x="", y="true positive rate", title="")+
  facet_wrap(~variable,scales="free",nrow = 1)+#ylim(0,0.5)+
  # xlab("n")+#
  # ylab("Power")+#
  theme_bw() +#
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  # scale_color_brewer(palette = "Dark2")+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
    text = element_text(family = "sans"),#????????????????ʾ
    axis.text = element_text(size=12.5),
    # axis.text.x = element_blank(),
    axis.text.x = element_text(angle = 70,vjust = 1,hjust = 1),
    # legend.position = c(.9,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "none",
    # axis.text = element_text(face = 'bold'),
    axis.title=element_text(size=12.5),
    strip.text=element_text(size=12.5),
    # legend.text = element_text(face = 'bold'),
    # legend.title = element_text(face = 'bold')
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )
# + guides(fill=guide_legend(ncol=2))

ggsave("Figure/Out_fig/MESC_tpr_ueq.png",width = 16,height = 5.5)


#######################################################################################################


# result<-readRDS("compresulteva5_1.rds") 
result[["rate3"]][["ROTS"]] = NULL
result[["rate4"]][["ROTS"]] = NULL

name_methods = c("Monocle","SCDE","MAST","DESeq","Limma","edgeR","ttest","ttestR","Welch","WMW",
                 "WMW-A-G","WMW-A-GMM","WMW-A-NB","WMW-A-NBM","WMW-A(normalized)","WMW-A(count)",
                 'DEsingle','ZINB-WaVE',"ZIAQ","consexpression","BaySeq","NOISeq","DEGnext")

list_names = c("Limma","ttest","ttestR","Welch","WMW",
               "BaySeq","consexpression","DEGnext","DESeq","edgeR","NOISeq",
               'DEsingle',"MAST","Monocle","SCDE",'ZINB-WaVE','ZIAQ',
               "WMW-A-G","WMW-A-GMM","WMW-A-NB","WMW-A-NBM","WMW-A(normalized)","WMW-A(count)")



my_list = result[["rate4"]]
names(my_list) = name_methods
my_list_tmp = my_list[list_names]


data_comb = NULL

for(r in 1:8){
  compb = matrix(NA, ncol=length(my_list_tmp),nrow = 100)
  for (i in 1:length(my_list_tmp)){
    comp = matrix(unlist(my_list_tmp[[i]][,r]), ncol=1) 
    compb[1:length(comp),i] = comp
  }
  colnames(compb) = list_names
  
  library("reshape2")
  a = melt(compb)
  a = a[,-1]
  
  if(is.null(data_comb)){
    data_comb = a
  }else{
    data_comb = cbind(data_comb,a[,2])
  }
  
}

library(reshape2)
library(Rmisc)

colnames(data_comb) = c("Methods","m=n=5","m=5,n=10","m=5,n=15","m=5,n=20","m=n=10",
                        "m=n=15","m=n=20","m=n=25")
data = melt(data_comb, id="Methods")

idx = which(is.na(data[,3])==TRUE)
data = data[-idx,]
idx2 = which(data$Methods=="ZIAQ")
data = data[-idx2,]

data <- summarySE(data, measurevar="value", groupvars=c("Methods","variable"))

ggplot(data, aes(x=Methods, y=value, colour=Methods,group=1))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se))+#,size = 1,width = 0.2
  # geom_line(size = 0.8,color = "black")+
  geom_point()+#shape = 16,size = 3
  ylim(0,0.5)+
  labs(x="", y="false positive rate", title="")+
  facet_wrap(~variable,scales="free",nrow = 2)+#ylim(0,1)+
  # xlab("n")+#
  # ylab("Power")+#
  theme_bw() +#ȥ����ɫ
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  # scale_color_brewer(palette = "Dark2")+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#themeдȥұ߿
    text = element_text(family = "sans"),#ʾ
    axis.text.x = element_blank(),
    # legend.position = c(.9,.2),#ͼλãͼڲϽ
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "right",
    # axis.text = element_text(face = 'bold'),
    # axis.title=element_text(face = 'bold'),
    # strip.text=element_text(face = 'bold'),
    # legend.text = element_text(face = 'bold'),
    # legend.title = element_text(face = 'bold')
    # legend.box.background = element_rect(color="black"))#Ϊͼ߿
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )



# ggplot(data = data)+#,aes(x=Methods, y=value,group = Methods,color=Methods)
#   geom_boxplot(aes(x=Methods, y=value,fill=Methods)) + 
#   labs(x="", y="true positive rate", title="")+
#   facet_wrap(~variable,scales="free",nrow = 2)+#ylim(0,1)+
#   # xlab("n")+#
#   # ylab("Power")+#
#   theme_bw() +#
#   # scale_x_continuous(breaks=seq(0, 10, 2))+
#   # scale_color_brewer(palette = "Dark2")+
#   # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     # panel.grid.major=element_line(colour=NA),
#     # panel.background = element_rect(fill = "transparent",colour = NA),
#     # plot.background = element_rect(fill = "transparent",colour = NA),
#     # panel.grid.minor = element_blank(),#theme
#     text = element_text(family = "sans"),#
#     axis.text.x = element_blank(),
#     # legend.position = c(.9,.2),#
#     # legend.position = "none",
#     # legend.title=element_blank(),
#     legend.position = "bottom",
#     # axis.text = element_text(face = 'bold'),
#     # axis.title=element_text(face = 'bold'),
#     # strip.text=element_text(face = 'bold'),
#     # legend.text = element_text(face = 'bold'),
#     # legend.title = element_text(face = 'bold')
#     # legend.box.background = element_rect(color="black"))#
#     # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
#   )


ggsave("Figure/Out_fig/MESC_fpr.png",width = 12,height = 5)



