plot_normal_comb <- function(){
  library(R.matlab)
  
  library(reshape2)
  library(ggplot2)
  library(patchwork)
  
  # install.packages("RImagePalette")
  library(RImagePalette)
  
  
  
  
  result1 = readMat('Simulation/normal/comb/asy/result_asy_mneq.mat')
  w1 = result1[["power.wmwa.asy"]][3,1:11]
  w2 = result1[["power.wmwa2"]][3,1:11]
  n = result1[["N"]][1:11]
  
  data1 = data.frame(n=n, w1=w1, w2=w2)
  data1 <- melt(data1,id="n")
  groups = c(rep("WMW-A(asy)",11),rep("WMW-A(perm)",11))
  colnames(data1) <- c("n","groups","Power")#????????
  data1$groups = groups
  
  # lwd_pt <- .pt*72.27/96
  p1 =ggplot(data = data1,aes(x=n,y=Power,group = groups,color=groups,shape=groups))+
    geom_point(size = 3)+
    geom_line(size = 0.8)+
    labs(x="n", y="Power (Normal distribution)", title="m=n")+
    # xlab("n")+#??????????
    # ylab("Power")+#??????????
    theme_bw() +#ȥ?����???ɫ
    # scale_color_manual(values = c("#FF410DFF", "#5C88DAFF"))+#"#EE0000FF"
    scale_color_brewer(palette = "Set2")+
    # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
    theme(
      plot.title = element_text(hjust = 0.5),
      # panel.grid.major=element_line(colour=NA),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
      text = element_text(family = "serif"),#????????????????ʾ
      legend.position = c(.7,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
      legend.title=element_blank(),
      title=element_text(size = 10),
      # axis.text = element_text(face = 'bold'),
      # axis.title=element_text(face = 'bold'),
      # strip.text=element_text(face = 'bold'),
      # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
      # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
    )
  
  # scale_x_continuous(limits = c(2000,2018),breaks = seq(2000,2018,1))#???ĺ??????̶?ֵ
  #????zoom?鿴??ͼ
  
  
  
  #############################################################################################################################
  
  result2 = readMat('Simulation/normal/comb/asy/result_asy_m5.mat')
  w1 = result2[["power.wmwa.asy"]][3,]
  w2 = result2[["power.wmwa2"]][3,]
  n = result2[["N"]][1:10]
  
  data2 = data.frame(n=n, w1=w1, w2=w2)
  data2 <- melt(data2,id="n")
  groups = c(rep("WMW-A(asy)",10),rep("WMW-A(perm)",10))
  colnames(data2) <- c("n","groups","Power")#????????
  data2$groups = groups
  
  
  p2 = ggplot(data = data2,aes(x=n,y=Power,group = groups,color=groups,shape=groups))+
    geom_point(size = 3)+
    geom_line(size = 0.8)+
    labs(x="n", y="", title="m=5")+
    # xlab("n")+#??????????
    # ylab("Power")+#??????????
    theme_bw() +#ȥ?����???ɫ
    # scale_color_manual(values = c("#FF410DFF", "#5C88DAFF"))+
    scale_color_brewer(palette = "Set2")+
    # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
    theme(
      plot.title = element_text(hjust = 0.5),
      # panel.grid.major=element_line(colour=NA),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
      text = element_text(family = "sans"),#????????????????ʾ
      legend.position = c(.7,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
      legend.title=element_blank(),
      title=element_text(size = 10),
      # axis.text = element_text(face = 'bold'),
      # axis.title=element_text(face = 'bold'),
      # strip.text=element_text(face = 'bold'),
      # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
      # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
    )
  
  
  #############################################################################################################################
  
  result3 = readMat('Simulation/normal/comb/asy/result_asy_m30.mat')
  w1 = result3[["power.wmwa.asy"]][3,1:8]
  w2 = result3[["power.wmwa2"]][3,1:8]
  n = result1[["N"]][1:8]
  
  data3 = data.frame(n=n, w1=w1, w2=w2)
  data3 <- melt(data3,id="n")
  groups = c(rep("WMW-A(asy)",8),rep("WMW-A(perm)",8))
  colnames(data3) <- c("n","groups","Power")#????????
  data3$groups = groups
  
  
  
  p3 = ggplot(data = data3,aes(x=n,y=Power,group = groups,color=groups,shape=groups))+
    geom_point(size = 3)+
    geom_line(size = 0.8)+
    labs(x="n", y="", title="m=30")+
    # xlab("n")+#??????????
    # ylab("Power")+#??????????
    theme_bw() +#ȥ?����???ɫ
    scale_color_brewer(palette = "Set2")+
    # scale_color_manual(values = c("#FF410DFF", "#5C88DAFF"))+
    # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
    theme(
      plot.title = element_text(hjust = 0.5),
      # panel.grid.major=element_line(colour=NA),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
      text = element_text(family = "sans"),#????????????????ʾ
      # legend.position = 'right',
      legend.position = c(.7,.2),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
      legend.title=element_blank(),
      title=element_text(size = 10),
      # axis.text = element_text(face = 'bold'),
      # axis.title=element_text(face = 'bold'),
      # strip.text=element_text(face = 'bold'),
      # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
      # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
    )
  
  
  
  
  
  #############################################################################################################################
  
  data6 = readMat('Simulation/normal/comb/k/result_k_eq.mat')
  data6_2 = readMat("Simulation/normal/comb/k/result_k_ueq.mat")
  
  K = as.numeric(data6_2[["K"]][seq(1,19,2)])
  power_wmwa1_eq = data6[["power.wmwa1"]][3,3,][seq(1,19,2)]
  power_wmwa1_ueq = data6_2[["power.wmwa1"]][3,1,][seq(1,19,2)]
  
  data1 = data.frame(K=K, power_wmwa1_eq=power_wmwa1_eq, power_wmwa1_ueq=power_wmwa1_ueq)
  data1 <- melt(data1,id="K")
  groups = c(rep("WMW-A(m=n=5)",10),rep("WMW-A(m=5,n=10)",10))
  colnames(data1) <- c("K","groups","Power")#????????
  data1$groups = groups
  
  
  p6 =ggplot(data = data1,aes(x=K,y=Power,group = groups,color=groups,shape=groups))+
    geom_point(size = 3)+
    geom_line(size = 0.8)+
    labs(x="k", y="Power (Normal distribution)", title=expression(paste(delta,"=", 1))
    )+
    ylim(0,1)+
    # xlab("n")+#??????????
    # ylab("Power")+#??????????
    theme_bw() +#ȥ?����???ɫ
    # scale_color_manual(values = c("#FFBB78FF", "#31A354FF"))+
    scale_color_manual(values = c("#FF410DFF", "#5C88DAFF"))+
    # scale_color_brewer(palette = "Accent")+
    # theme_set(theme_bw(base_size = 20, base_line_size = 3/lwd_pt, base_rect_size = 3/lwd_pt))+
    theme(
      plot.title = element_text(hjust = 0.5),
      # panel.grid.major=element_line(colour=NA),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      # panel.grid.minor = element_blank(),#????theme?д???????ȥ?????????ұ??????????߿?
      text = element_text(family = "sans"),#????????????????ʾ
      legend.position = c(.7,.19),#????ͼ????λ?ã?????ͼ?ڲ??????Ͻ?
      legend.title=element_blank(),
      title=element_text(size = 10),
      # axis.text = element_text(face = 'bold'),
      # axis.title=element_text(face = 'bold'),
      # strip.text=element_text(face = 'bold'),
      # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
      # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
    )
  
  
  
  #############################################################################################################################
  
  
  
  #################################################################################################################
  
  library(pheatmap)
  
  result7 = readMat('Simulation/normal/comb/rate/result_comb_rate.mat')
  result7_2 = readRDS("Simulation/normal/comb/rate/result_limma_rate.rds")
  
  power_t = result7[["power.t"]][1:2,]
  power_tR = result7[["power.tR"]][1:2,]
  power_wmw = result7[["power.wc"]][1:2,]
  power_wc = result7[["power.wmw"]][1:2,]
  power_limma = result7_2[1:2,]
  power_wmwa1_d1 = result7[["power.wmwa1"]][1,,1,]
  power_wmwa1_d2 = result7[["power.wmwa1"]][2,,1,]
  
  data1 = matrix(nrow = 16, ncol = 9)
  data1[1,] = power_t[1,] 
  data1[2,] = power_tR[1,]
  data1[3,] = power_wmw[1,]
  data1[4,] = power_wc[1,]
  data1[5,] = power_limma[1,]
  data1[6:16,] = t(power_wmwa1_d1)
  data1 = t(data1)
  rownames(data1) = c(seq(0.1,0.9,0.1))
  colnames(data1) = c("t-test","t-testR","WMW","Welch","Limma",paste('WMW-A(\u03C0\u005F\u007A)',0.0,sep="="),
                      paste('WMW-A(\u03C0\u005F\u007A)',0.1,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.2,sep="="),
                      paste('WMW-A(\u03C0\u005F\u007A)',0.3,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.4,sep="="),
                      paste('WMW-A(\u03C0\u005F\u007A)',0.5,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.6,sep="="),
                      paste('WMW-A(\u03C0\u005F\u007A)',0.7,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.8,sep="="),
                      paste('WMW-A(\u03C0\u005F\u007A)',0.9,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',1.0,sep="="))
  data1 = as.data.frame(data1)#power_limma=power_limma,
  p7 = pheatmap(data1, cluster_row = FALSE, cluster_col = FALSE,legend = FALSE,
                display_numbers = TRUE, number_format = "%.3f",number_color = "black",
                cellwidth = 20, cellheight = 20,
                border_color = "black",angle_col=45,
                fontsize_number = 6.5,fontsize=10,
                fontsize_row = 10, fontsize_col = 10,
                main = expression(paste(delta,"=0.5, k=1")),
                labels_col=c("t-test","t-testR","WMW","Welch","Limma",expression(paste("WMW-A(", pi[z],"=0.0)")),expression(paste("WMW-A(", pi[z],"=0.1)"))
                             ,expression(paste("WMW-A(", pi[z],"=0.2)")),expression(paste("WMW-A(", pi[z],"=0.3)")),
                             expression(paste("WMW-A(", pi[z],"=0.4)")),expression(paste("WMW-A(", pi[z],"=0.5)")),
                             expression(paste("WMW-A(", pi[z],"=0.6)")),expression(paste("WMW-A(", pi[z],"=0.7)")),
                             expression(paste("WMW-A(", pi[z],"=0.8)")),expression(paste("WMW-A(", pi[z],"=0.9)")),
                             expression(paste("WMW-A(", pi[z],"=1.0)"))),
                labels_row =c(expression(paste(pi,"=0.1")),expression(paste(pi,"=0.2")),expression(paste(pi,"=0.3")),expression(paste(pi,"=0.4")),
                              expression(paste(pi,"=0.5")),expression(paste(pi,"=0.6")),expression(paste(pi,"=0.7")),expression(paste(pi[z],"=0.8")),
                              expression(paste(pi,"=0.9")))
  )
  
  p7$gtable$grobs[[1]]$gp = grid::gpar(fontfamily="sans")
  p7$gtable$grobs[[2]]$gp = grid::gpar(fontfamily="sans")
  p7$gtable$grobs[[3]]$gp = grid::gpar(fontfamily="sans")
  p7$gtable$grobs[[4]]$gp = grid::gpar(fontfamily="sans")
  
  
  # pp1 = p1+labs(tag = expression(paste(bold("c"))))+theme(text = element_text(family = "sans"))
  # pb = p6+labs(tag = expression(paste(bold("d"))))+theme(text = element_text(family = "sans"))
  pp7 = ggplotify::as.ggplot(p7)
  # pc = pp7+labs(tag = expression(paste(bold("e"))))+theme(text = element_text(family = "sans"))
  
  fig_comb = list(p1, p2, p3, p6, pp7)
  return(fig_comb)
  # library(ggpubr)
  # pbc=ggarrange(pb,pc,nrow = 1,widths = c(1,1.5))
}

plot_normal_pt <- function(){
  library(reshape2)
  
  data_plot = function(result, result_2,data,d,flag){
    power_t = result[["power"]][["ttest"]][d,1:10]
    power_tR = result[["power"]][["ttestR"]][d,1:10]
    power_wmw = result[["power"]][["wilx"]][d,1:10]
    power_wc = result[["power"]][["welch"]][d,1:10]
    power_limma = result[["power"]][["Limma"]][d,1:10]
    power_wmwa1 = result_2[["power"]][[1]][d,1:10]
    power_wmwag1 = result_2[["power"]][[2]][d,1:10]
    power_wmwag2 = result_2[["power"]][[3]][d,1:10]
    n = data[["N"]]
    n=as.numeric(n)
    d2 = data[["Alphay"]][d]-0.5
    
    
    data1 = data.frame(n=n, power_t=power_wc, power_tR=power_tR,power_wmw=power_wmw,power_wc =power_t, 
                       power_limma=power_limma,power_wmwa1=power_wmwa1,power_wmwag1=power_wmwag1
                       ,power_wmwag2=power_wmwag2)
    data1 <- melt(data1,id="n")
    Methods = c(rep("t-test",length(n)),rep("t-testR",length(n)),rep("WMW",length(n)),rep("Welch",length(n)),
                rep("Limma",length(n)),rep("WMW-A",length(n)),rep("WMW-A-G",length(n)),rep("WMW-A-GMM",length(n)))
    colnames(data1) <- c("n","Methods","Power")#????????
    data1$Methods = Methods
    if (flag==1){
      data1$type = "balanced(m=n)"
    }else{
      data1$type = "unbalanced(m=5)"
    }
    
    delta = paste('\u03B4',d2,sep="=")
    data1$delta = delta
    return(data1)
  }
  
  
  ############################################################################################################
  result_eq_comp = readRDS('Simulation/normal/powert/result_normal_eq_comp.rds')
  result_eq_wmwa = readRDS('Simulation/normal/powert/result_normal_eq.rds')
  rawdata = readRDS("Simulation/normal/powert/data_normal_eq.rds")
  
  data1 = data_plot(result_eq_comp, result_eq_wmwa, rawdata, d=2, flag=1)
  data2 = data_plot(result_eq_comp, result_eq_wmwa, rawdata, d=3, flag=1)
  data3 = data_plot(result_eq_comp, result_eq_wmwa, rawdata, d=4, flag=1)
  data4 = data_plot(result_eq_comp, result_eq_wmwa, rawdata, d=5, flag=1)
  datateq = data_plot(result_eq_comp, result_eq_wmwa, rawdata, d=1, flag=1)
  
  result_eq_comp = readRDS('Simulation/normal/powert/result_normal_ueq_comp.rds')
  result_eq_wmwa = readRDS('Simulation/normal/powert/result_normal_ueq.rds')
  rawdata = readRDS("Simulation/normal/powert/data_normal_ueq.rds")
  
  data5 = data_plot(result_eq_comp, result_eq_wmwa,rawdata, d=2, flag=2)
  data6 = data_plot(result_eq_comp, result_eq_wmwa,rawdata, d=3, flag=2)
  data7 = data_plot(result_eq_comp, result_eq_wmwa,rawdata, d=4, flag=2)
  data8 = data_plot(result_eq_comp, result_eq_wmwa,rawdata, d=5, flag=2)
  datatueq = data_plot(result_eq_comp, result_eq_wmwa,rawdata, d=1, flag=2)
  
  
  
  #######################################################################################################
  
  library(dplyr)
  data = bind_rows(data1,data2,data3,data4)
  data$num = c(rep(1:10,32))
  
  library(ggplot2)
  
  pt1=ggplot(data = data,aes(x=n,y=Power,group = Methods,color=Methods))+#,shape=Methods
    geom_point(size=0.9)+
    geom_line()+#aes(linetype=Methods, color=Methods)
    labs(x="n", y="Power (Normal distribution)", title="")+
    ylim(0,1)+facet_grid(type~delta,scales="free",switch="y")+
    # xlab("n")+#??????????
    # ylab("Power")+#??????????
    theme_bw() +#ȥ?����???ɫ
    # scale_x_continuous(breaks=seq(0, 10, 2))+
    scale_color_manual(values = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00A9FF", "#8787FFFF", "#C77CFF", "#FF61CC"))+
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
  
  
  library(dplyr)
  data = bind_rows(data5,data6,data7,data8)
  data$num = c(rep(1:10,32))


  pt2=ggplot(data = data,aes(x=n,y=Power,group = Methods,color=Methods))+#,shape=Methods
    geom_point(size=0.9)+
    geom_line()+#aes(linetype=Methods, color=Methods)
    labs(x="n", y="Power (Normal distribution)", title="")+
    ylim(0,1)+facet_grid(type~delta,scales="free",switch="y")+
    # xlab("n")+#??????????
    # ylab("Power")+#??????????
    theme_bw() +#ȥ?����???ɫ
    # scale_x_continuous(breaks=seq(0, 10, 2))+
    scale_color_manual(values = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00A9FF", "#8787FFFF", "#C77CFF", "#FF61CC"))+
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
  data = bind_rows(datateq)
  data$num = c(rep(1:10,8))
  
  # library(RColorBrewer)
  # colors=brewer.pal(name="Set3",8)
  pt3=ggplot(data = data,aes(x=n,y=Power,group = Methods,color=Methods))+#,shape=Methods
    geom_point(size=0.9)+
    geom_line()+#aes(linetype=Methods, color=Methods)
    labs(x="n", y="Type I error rate (Normal distribution)", title="")+
    ylim(0,1)+facet_grid(delta~type,scales="free",switch="y")+
    # xlab("n")+#??????????
    # ylab("Power")+#??????????
    theme_bw() +#ȥ?����???ɫ
    # scale_x_continuous(breaks=seq(0, 10, 2))+
    scale_color_manual(values = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00A9FF", "#8787FFFF", "#C77CFF", "#FF61CC"))+
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
  data = bind_rows(datatueq)
  data$num = c(rep(1:10,8))
  
  # library(RColorBrewer)
  # colors=brewer.pal(name="Set3",8)
  pt4=ggplot(data = data,aes(x=n,y=Power,group = Methods,color=Methods))+#,shape=Methods
    geom_point(size=0.9)+
    geom_line()+#aes(linetype=Methods, color=Methods)
    labs(x="n", y="Type I error rate (Normal distribution)", title="")+
    ylim(0,1)+facet_grid(delta~type,scales="free",switch="y")+
    # xlab("n")+#??????????
    # ylab("Power")+#??????????
    theme_bw() +#ȥ?����???ɫ
    # scale_x_continuous(breaks=seq(0, 10, 2))+
    scale_color_manual(values = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00A9FF", "#8787FFFF", "#C77CFF", "#FF61CC"))+
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
  
  
  fig_pt = list(pt1, pt2, pt3, pt4)
  return(fig_pt)
}

fig_comb = plot_normal_comb()
fig_pt = plot_normal_pt()

pa = fig_pt[[1]]
pb = fig_pt[[2]]
pc = fig_pt[[3]]
pd = fig_pt[[4]]

pe = fig_comb[[1]]
pg = fig_comb[[2]]
ph = fig_comb[[3]]
pi = fig_comb[[4]]
pj = fig_comb[[5]]

library(ggpubr)
# pegh = ggarrange(pe,pg,ph, ncol=3)

ppe = pe+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
# ppg = pg+labs(tag = expression(paste(bold("c"))))+theme(text = element_text(family = "sans"))
# pph = ph+labs(tag = expression(paste(bold("d"))))+theme(text = element_text(family = "sans"))
ppi = pi+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))
ppj = pj+labs(tag = expression(paste(bold("c"))))+theme(text = element_text(family = "sans"))



pp1 = pa+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
pp2 = pb+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))
pp3 = pc+labs(tag = expression(paste(bold("c"))))+theme(text = element_text(family = "sans"))
pp4 = pd+labs(tag = expression(paste(bold("d"))))+theme(text = element_text(family = "sans"))
library(ggpubr)
ggarrange(pp1,pp3,pp2,pp4,common.legend = T,legend = "bottom",widths = c(4,1.3))
ggsave("Figure/Out_fig/normal_powert_fig.png",width = 14,height =7.6)


library(patchwork)
design <- "AABBCC
           DDEEE#"
wrap_plots(A = ppe, B = pg, C=ph,D=ppi,E=ppj, design = design)
ggsave("Figure/Out_fig/normal_comb_fig.png", width = 10.5,height = 7.7)
