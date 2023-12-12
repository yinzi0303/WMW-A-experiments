
library(reshape2)

data_plot = function(result, result_2, result_3, d,para){
  
  M = as.numeric(para[["M"]])
  N = as.numeric(para[["N"]])
  alphax = para[["alphax"]]
  Alphay = para[["Alphay"]]
  beta = para[["beta"]]
  Times = para[["Times"]]
  
  # power_scde = result_2$power$scde[d,]
  # power_MAST = result_2$power$MAST[d,]
  # power_monocle = result_2$power$monocle[d,]
  
  power_DESeq = result_2$power$DESeq[d,]
  power_limma = result_2$power$Limma[d,] 
  power_edgeR = result_2$power$edgeR[d,] 
  
  power_t = result_2$power$ttest[d,] 
  power_tR = result_2$power$ttestR[d,] 
  power_wc = result_2$power$welch[d,] 
  power_wmw = result_2$power$wilx[d,] 
  
  power_wmwa1 = result[["power"]][d,]
  power_wmwanb1 = result_3[["power"]][[1]][d,]
  power_wmwanb2 = result_3[["power"]][[2]][d,]
  # power_wmwag1 = result_3[["power"]][[3]][d,]
  # power_wmwag2 = result_3[["power"]][[4]][d,]
  
  # n = result[["N"]][1:10]
  # n=as.numeric(n)
  # d2 = result4[["D"]][d]-0.5
  n=N
  
  data1 = data.frame(n=N, 
                     # power_scde = power_scde, power_MAST=power_MAST, power_monocle=power_monocle, 
                     power_DESeq =power_DESeq, power_limma=power_limma, power_edgeR = power_edgeR, 
                     power_t=power_wc, power_tR=power_tR,power_wmw=power_wmw,power_wc =power_t, 
                     power_wmwa1=power_wmwa1,power_wmwanb1=power_wmwanb1,power_wmwanb2=power_wmwanb2
                     # ,power_wmwag1=power_wmwag1,power_wmwag2=power_wmwag2
  )
  # if (sum(is.na(power_scde))<1){
  #   data1$
  # }
  
  data1 <- melt(data1,id="n")
  Methods = c(
    # rep("SCDE",length(n)),rep("MAST",length(n)),rep("monocle",length(n)),
    rep("DESeq",length(n)),rep("Limma",length(n)),rep("edgeR",length(n)),
    rep("t-test",length(n)),rep("t-testR",length(n)),rep("WMW",length(n)),
    rep("Welch",length(n)),rep("WMW-A",length(n))
    ,rep("WMW-A-NB",length(n)),rep("WMW-A-NBM",length(n))
    # ,rep("WMW-A-G",length(n)),rep("WMW-A-GMM",length(n))
  )
  colnames(data1) <- c("n","Methods","Power")#????????
  
  data1$Methods = Methods
  if (identical(M,N)){
    data1$type = c("balanced(m=n)")
  }else{
    data1$type = c("unbalanced(m=5)")
  }
  
  
  # delta = c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',Alphay[d],sep = "="),sep=","))
  
  delta = paste("delta",5-d,sep = "")
  data1$delta = delta
  return(data1)
}


############################################################################################################
# setwd("C:/Users/dell/Desktop/wmwa/WMWA-R/NB")
Data1 = readRDS("Simulation/NB/powert1/data_nb1_eq.rds")
M = Data1[["M"]]
N = Data1[["N"]]
alphax = Data1[["alphax"]]
Alphay = Data1[["Alphay"]]
beta = Data1[["beta"]]
Times = Data1[["Times"]]
para = list(M=M, N=N, alphax=alphax, Alphay=Alphay, beta=beta, Times=Times)
result1 = readRDS('Simulation/NB/powert1/result_NB1_eq.rds')
result1_2 = readRDS('Simulation/NB/powert1/result_NB1_eq_comp.rds')
result1_3 = readRDS('Simulation/NB/powert1/result_NB1_eqg.rds')
data1 = data_plot(result1, result1_2, result1_3, d=4, para)
data2 = data_plot(result1, result1_2, result1_3, d=3, para)
data3 = data_plot(result1, result1_2, result1_3, d=2, para)
data4 = data_plot(result1, result1_2, result1_3, d=1, para)


Data1 = readRDS("Simulation/NB/powert1/data_nb1_ueq.rds")
M = Data1[["M"]]
N = Data1[["N"]]
alphax = Data1[["alphax"]]
Alphay = Data1[["Alphay"]]
beta = Data1[["beta"]]
Times = Data1[["Times"]]
para = list(M=M, N=N, alphax=alphax, Alphay=Alphay, beta=beta, Times=Times)
result2 = readRDS('Simulation/NB/powert1/result_NB1_ueq.rds')
result2_2 = readRDS('Simulation/NB/powert1/result_NB1_ueq_comp.rds')
result2_3 = readRDS('Simulation/NB/powert1/result_NB1_ueqg.rds')
data5 = data_plot(result2, result2_2, result2_3, d=4, para)
data6 = data_plot(result2, result2_2, result2_3, d=3, para)
data7 = data_plot(result2, result2_2, result2_3, d=2, para)
data8 = data_plot(result2, result2_2, result2_3, d=1, para)



Data1 = readRDS("Simulation/NB/powert1/data_nb1_eqt.rds")
M = Data1[["M"]]
N = Data1[["N"]]
alphax = Data1[["alphax"]]
Alphay = Data1[["Alphay"]]
beta = Data1[["beta"]]
Times = Data1[["Times"]]
para = list(M=M, N=N, alphax=alphax, Alphay=Alphay, beta=beta, Times=Times)
result1 = readRDS('Simulation/NB/powert1/result_NB1_eqt.rds')
result1_2 = readRDS('Simulation/NB/powert1/result_NB1_eqt_comp.rds')
result1_3 = readRDS('Simulation/NB/powert1/result_NB1_eqtg.rds')
dataeqt = data_plot(result1, result1_2, result1_3, d=1, para)


Data1 = readRDS("Simulation/NB/powert1/data_nb1_ueqt.rds")
M = Data1[["M"]]
N = Data1[["N"]]
alphax = Data1[["alphax"]]
Alphay = Data1[["Alphay"]]
beta = Data1[["beta"]]
Times = Data1[["Times"]]
para = list(M=M, N=N, alphax=alphax, Alphay=Alphay, beta=beta, Times=Times)
result2 = readRDS('Simulation/NB/powert1/result_NB1_ueqt.rds')
result2_2 = readRDS('Simulation/NB/powert1/result_NB1_ueqt_comp.rds')
result2_3 = readRDS('Simulation/NB/powert1/result_NB1_ueqtg.rds')
dataueqt = data_plot(result2, result2_2, result2_3, d=1, para)



library(dplyr)
data_1 = bind_rows(data1,data2,data3,data4)
data_1$num = c(rep(1:10,40))

delta_statues = c(delta1 = c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',0.48,sep = "="),sep=",")),
                  delta2=c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',0.45,sep = "="),sep=",")),
                  delta3=c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',0.42,sep = "="),sep=",")),
                  delta4=c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',0.4,sep = "="),sep=","))
)



p1=ggplot(data = data_1,aes(x=n,y=Power,group = Methods,color=Methods))+#,shape=Methods
  geom_point(size=0.9)+
  geom_line()+#aes(linetype=Methods, color=Methods)
  labs(x="n", y="Power (Negative Binomial distribution)", title="")+
  ylim(0,1)+facet_grid(type~delta,scales="free",switch="y",labeller = labeller(delta=delta_statues))+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#ȥ?����???ɫ
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  scale_color_manual(values = c("#FFCD00FF","#FF0000FF","#F8766D", "#CD9600", "#7CAE00", 
                                "#00BE67", "#00A9FF", "#8787FFFF", "#C77CFF", "#FF61CC"))+
  # scale_color_brewer(palette = "Dark2")+
  # scale_linetype_manual(values = c("twodash","twodash","twodash","twodash","twodash","solid","solid","solid"))+
  # scale_shape_manual(values = c(20,13,1,2,3,4,5,6))+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
    text = element_text(family = "sans"),#????????????????ʾ
    # axis.text.x = element_blank(),
    # legend.position = c(.9,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
    # legend.position = "none",
    # legend.title=element_blank(),
    # legend.position = "None",
    # axis.text = element_text(size=10),
    axis.title=element_text(size=10),
    # strip.text=element_text(size=10),
    # legend.text = element_text(size=10),
    # legend.title = element_text(size=10)#face = 'bold',
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )


library(dplyr)
data = bind_rows(data5,data6,data7,data8)
data$num = c(rep(1:10,40))

delta_statues = c(delta1 = c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',0.48,sep = "="),sep=",")),
                  delta2=c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',0.45,sep = "="),sep=",")),
                  delta3=c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',0.42,sep = "="),sep=",")),
                  delta4=c(paste(paste('\u03B1\u005Fx',alphax,sep = "="),paste('\u03B1\u005Fy',0.4,sep = "="),sep=","))
)

p2=ggplot(data = data,aes(x=n,y=Power,group = Methods,color=Methods))+#,shape=Methods
  geom_point(size=0.9)+
  geom_line()+#aes(linetype=Methods, color=Methods)
  labs(x="n", y="Power (Negative Binomial distribution)", title="")+
  ylim(0,1)+facet_grid(type~delta,scales="free",switch="y",labeller = labeller(delta=delta_statues))+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#ȥ?����???ɫ
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  scale_color_manual(values = c("#FFCD00FF","#FF0000FF","#F8766D", "#CD9600", "#7CAE00", 
                                "#00BE67", "#00A9FF", "#8787FFFF", "#C77CFF", "#FF61CC"))+
  # scale_color_brewer(palette = "Dark2")+
  scale_linetype_manual(values = c("twodash","twodash","twodash","twodash","twodash","solid","solid","solid"))+
  # scale_shape_manual(values = c(20,13,1,2,3,4,5,6))+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
    text = element_text(family = "sans"),#????????????????ʾ
    # axis.text.x = element_blank(),
    # legend.position = c(.9,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "None",
    # axis.text = element_text(size=10),
    axis.title=element_text(size=10),
    # strip.text=element_text(size=10),
    # legend.text = element_text(size=10),
    # legend.title = element_text(size=10)#face = 'bold',
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )



library(dplyr)
data = bind_rows(dataeqt)
data$num = c(rep(1:10,10))


delta_statues = c(delta4 = '\u03B1\u005Fx=\u03B1\u005Fy=0.5')

# library(RColorBrewer)
# colors=brewer.pal(name="Set3",8)
p3=ggplot(data = data,aes(x=n,y=Power,group = Methods,color=Methods))+#,shape=Methods
  geom_point(size=0.9)+
  geom_line()+#aes(linetype=Methods, color=Methods)
  labs(x="n", y="Type I error rate (Negative Binomial distribution)", title="")+
  ylim(0,1)+facet_grid(type~delta,scales="free",switch="y",labeller = labeller(delta=delta_statues))+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#ȥ?����???ɫ
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  scale_color_manual(values = c("#FFCD00FF","#FF0000FF","#F8766D", "#CD9600", "#7CAE00", 
                                "#00BE67", "#00A9FF", "#8787FFFF", "#C77CFF", "#FF61CC"))+
  # scale_color_brewer(palette = "Dark2")+
  scale_linetype_manual(values = c("twodash","twodash","twodash","twodash","twodash","solid","solid","solid"))+
  # scale_shape_manual(values = c(20,13,1,2,3,4,5,6))+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
    text = element_text(family = "sans"),#????????????????ʾ
    # axis.text.x = element_blank(),
    # legend.position = c(.9,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "None",
    # axis.text = element_text(size=10),
    axis.title=element_text(size=10),
    # strip.text=element_text(size=10),
    # legend.text = element_text(size=10),
    # legend.title = element_text(size=10)#face = 'bold',
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )


library(dplyr)
data = bind_rows(dataueqt)
data$num = c(rep(1:10,10))

delta_statues = c(delta4 = c(paste('\u03B1\u005Fx=','\u03B1\u005Fy=',0.5)))

p4=ggplot(data = data,aes(x=n,y=Power,group = Methods,color=Methods))+#,shape=Methods
  geom_point(size=0.9)+
  geom_line()+#aes(linetype=Methods, color=Methods)
  labs(x="n", y="Type I error rate (Negative Binomial distribution)", title="")+
  ylim(0,1)+facet_grid(type~delta,scales="free",switch="y",labeller = labeller(delta=delta_statues))+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#ȥ?����???ɫ
  # scale_x_continuous(breaks=seq(0, 10, 2))+
  scale_color_manual(values = c("#FFCD00FF","#FF0000FF","#F8766D", "#CD9600", "#7CAE00", 
                                "#00BE67", "#00A9FF", "#8787FFFF", "#C77CFF", "#FF61CC"))+
  # scale_color_brewer(palette = "Dark2")+
  # scale_linetype_manual(values = c("twodash","twodash","twodash","twodash","twodash","solid","solid","solid"))+
  # scale_shape_manual(values = c(20,13,1,2,3,4,5,6))+
  # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
  theme(
    plot.title = element_text(hjust = 0.5),
    # panel.grid.major=element_line(colour=NA),
    # panel.background = element_rect(fill = "transparent",colour = NA),
    # plot.background = element_rect(fill = "transparent",colour = NA),
    # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
    text = element_text(family = "sans"),#????????????????ʾ
    # axis.text.x = element_blank(),
    # legend.position = c(.9,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
    # legend.position = "none",
    # legend.title=element_blank(),
    legend.position = "None",
    # axis.text = element_text(size=10),
    axis.title=element_text(size=10),
    # strip.text=element_text(size=10),
    # legend.text = element_text(size=10),
    # legend.title = element_text(size=10)#face = 'bold',
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )

# install.packages("ggsci")
# library(ggsci)
# show_col(pal_ucscgb()(26))


pp1 = p1+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
pp2 = p2+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))
pp3 = p3+labs(tag = expression(paste(bold("c"))))+theme(text = element_text(family = "sans"))
pp4 = p4+labs(tag = expression(paste(bold("d"))))+theme(text = element_text(family = "sans"))

library(ggpubr)
ggarrange(pp1,pp3,pp2,pp4,common.legend = T,legend = "bottom",widths = c(4,1.3))


ggsave("Figure/Out_fig/NB_powert1_fig.png",width = 14,height =7.6)


