library(R.matlab)
#???ذ?
library(reshape2)#?ں?????
library(ggplot2)#??ͼ????
library(patchwork)

# install.packages("RImagePalette")
library(RImagePalette)

# col = brewer.pal(6,"Set1")

# result1 = readMat('C:/Users/dell/Desktop/wmwa/jieguo/power/gamma(equal)/result_n1.mat')
# w1 = result1[["power.wmwa.asy"]][3,1:11]
# w2 = result1[["power.wmwa2"]][3,1:11]
# n = result1[["N"]][1:11]
# 
# data1 = data.frame(n=n, w1=w1, w2=w2)
# data1 <- melt(data1,id="n")
# groups = c(rep("WMW-A(asy)",11),rep("WMW-A(perm)",11))
# colnames(data1) <- c("n","groups","Power")#????????
# data1$groups = groups
#############################################################################################################################

result1 = readMat('Simulation/NB/comb/asy/result_asy_mneq.mat')
w1 = result1[["power.wmwa.asy"]][2,1:11]
w2 = result1[["power.wmwa2"]][2,1:11]
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
  labs(x="n", y="Power (Negative Binomial distribution)", title=expression(paste(alpha[x],"=0.5, ", alpha[y], "=0.45, m=n"))
  )+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#ȥ?����???ɫ
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
    # axis.text = element_text(size = 8),
    # axis.title=element_text(size = 8),
    # strip.text=element_text(size = 8),
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )

# scale_x_continuous(limits = c(2000,2018),breaks = seq(2000,2018,1))#???ĺ??????̶?ֵ
#????zoom?鿴??ͼ



#############################################################################################################################

result2 = readMat('Simulation/NB/comb/asy/result_asy_m5.mat')
w1 = result2[["power.wmwa.asy"]][2,]
w2 = result2[["power.wmwa2"]][2,]
n = result2[["N"]][1:10]

data2 = data.frame(n=n, w1=w1, w2=w2)
data2 <- melt(data2,id="n")
groups = c(rep("WMW-A(asy)",10),rep("WMW-A(perm)",10))
colnames(data2) <- c("n","groups","Power")#????????
data2$groups = groups


p2 = ggplot(data = data2,aes(x=n,y=Power,group = groups,color=groups,shape=groups))+
  geom_point(size = 3)+
  geom_line(size = 0.8)+
  labs(x="n", y="", title=expression(paste(alpha[x],"=0.5, ", alpha[y], "=0.45, m=5"))
  )+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#ȥ?����???ɫ
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
    # axis.text = element_text(size = 8),
    # axis.title=element_text(size = 8),
    # strip.text=element_text(size = 8),
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )


#############################################################################################################################

result3 = readMat('Simulation/NB/comb/asy/result_asy_m30.mat')
w1 = result3[["power.wmwa.asy"]][2,]
w2 = result3[["power.wmwa2"]][2,]
n = as.numeric(result3[["N"]])

data3 = data.frame(n=n, w1=w1, w2=w2)
data3 <- melt(data3,id="n")
groups = c(rep("WMW-A(asy)",10),rep("WMW-A(perm)",10))
colnames(data3) <- c("n","groups","Power")#????????
data3$groups = groups



p3 = ggplot(data = data3,aes(x=n,y=Power,group = groups,color=groups,shape=groups))+
  geom_point(size = 3)+
  geom_line(size = 0.8)+
  labs(x="n", y="", title=expression(paste(alpha[x],"=0.5, ", alpha[y], "=0.45, m=30"))
  )+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#ȥ?����???ɫ
  scale_color_brewer(palette = "Set2")+
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
    # axis.text = element_text(size = 8),
    # axis.title=element_text(size = 8),
    # strip.text=element_text(size = 8),
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )


#############################################################################################################################

#############################################################################################################################





#############################################################################################################################

#############################################################################################################################

data6 = readMat('Simulation/NB/comb/k/result_k_eq.mat')
data6_2 = readMat("Simulation/NB/comb/k/result_k_ueq.mat")

K = 1:10
power_wmwa1_eq = data6[["power.wmwa1"]][2,3,]
power_wmwa1_ueq = data6_2[["power.wmwa1"]][2,1,]

data1 = data.frame(K=K, power_wmwa1_eq=power_wmwa1_eq, power_wmwa1_ueq=power_wmwa1_ueq)
data1 <- melt(data1,id="K")
groups = c(rep("WMW-A(m=n=5)",10),rep("WMW-A(m=5,n=10)",10))
colnames(data1) <- c("K","groups","Power")#????????
data1$groups = groups


p6 =ggplot(data = data1,aes(x=K,y=Power,group = groups,color=groups,shape=groups))+
  geom_point(size = 3)+
  geom_line(size = 0.8)+
  labs(x="k", y="Power (Negative Binomial distribution)", title=expression(paste(alpha[x],"=0.5, ", alpha[y], "=0.45"))
  )+
  ylim(0,1)+
  # xlab("n")+#??????????
  # ylab("Power")+#??????????
  theme_bw() +#ȥ?����???ɫ
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
    # axis.text = element_text(size = 8),
    # axis.title=element_text(size = 8),
    # strip.text=element_text(size = 8),
    # legend.box.background = element_rect(color="black"))#Ϊͼ???????߿???
    # legend.box.background = element_rect(fill=NA,color = "black",linetype = 1)
  )



#############################################################################################################################


#################################################################################################################

library(pheatmap)

result7 = readMat('Simulation/NB/comb/rate/result_comb_rate_1.mat')
result7_2 = readRDS("Simulation/NB/comb/rate/result_limma_rate_1.rds")

power_t = result7[["power.t"]][1:2,]
power_tR = result7[["power.tR"]][1:2,]
power_wmw = result7[["power.wc"]][1:2,]
power_wc = result7[["power.wmw"]][1:2,]
power_DESeq = result7_2[["power_DESeq"]][1:2,]
power_limma = result7_2[["power_limma"]][1:2,]
power_edgeR = result7_2[["power_edgeR"]][1:2,]
power_wmwa1_d1 = result7[["power.wmwa1"]][1,,1,]
power_wmwa1_d2 = result7[["power.wmwa1"]][2,,1,]

data1 = matrix(nrow = 18, ncol = 9)
data1[1,] = power_t[1,] 
data1[2,] = power_tR[1,]
data1[3,] = power_wmw[1,]
data1[4,] = power_wc[1,]
data1[5,] = power_DESeq[1,]
data1[6,] = power_limma[1,]
data1[7,] = power_edgeR[1,]
data1[8:18,] = t(power_wmwa1_d1)
data1 = t(data1)
rownames(data1) = c(seq(0.1,0.9,0.1))
colnames(data1) = c("t-test","t-testR","WMW","Welch","DESeq","edgeR","Limma",
                    paste('WMW-A(\u03C0\u005F\u007A)',0.0,sep="="),
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
              fontsize_number = 6.5,fontsize=8,
              fontsize_row = 8, fontsize_col = 8,
              main = expression(paste(paste(alpha[x],"=0.5, "),paste(alpha[y],"=0.48, k=1"))),
              labels_col=c("t-test","t-testR","WMW","Welch","DESeq","edgeR","Limma",
                           expression(paste("WMW-A(", pi[z],"=0.0)")),expression(paste("WMW-A(", pi[z],"=0.1)"))
                           ,expression(paste("WMW-A(", pi[z],"=0.2)")),expression(paste("WMW-A(", pi[z],"=0.3)")),
                           expression(paste("WMW-A(", pi[z],"=0.4)")),expression(paste("WMW-A(", pi[z],"=0.5)")),
                           expression(paste("WMW-A(", pi[z],"=0.6)")),expression(paste("WMW-A(", pi[z],"=0.7)")),
                           expression(paste("WMW-A(", pi[z],"=0.8)")),expression(paste("WMW-A(", pi[z],"=0.9)")),
                           expression(paste("WMW-A(", pi[z],"=1.0)"))),
              labels_row =c(expression(paste(pi,"=0.1")),expression(paste(pi,"=0.2")),expression(paste(pi,"=0.3")),expression(paste(pi,"=0.4")),
                            expression(paste(pi,"=0.5")),expression(paste(pi,"=0.6")),expression(paste(pi,"=0.7")),expression(paste(pi[z],"=0.8")),
                            expression(paste(pi,"=0.9")))
)

# p7$gtable$grobs[[1]]$gp = grid::gpar(fontfamily="serif")
# p7$gtable$grobs[[2]]$gp = grid::gpar(fontfamily="serif")
# p7$gtable$grobs[[3]]$gp = grid::gpar(fontfamily="serif")
# p7$gtable$grobs[[4]]$gp = grid::gpar(fontfamily="serif")




# data2 = matrix(nrow = 18, ncol = 9)
# data2[1,] = power_t[2,] 
# data2[2,] = power_tR[2,]
# data2[3,] = power_wmw[2,]
# data2[4,] = power_wc[2,]
# data2[5,] = power_DESeq[2,]
# data2[6,] = power_limma[2,]
# data2[7,] = power_edgeR[2,]
# data2[8:18,] = t(power_wmwa1_d2)
# data2 = t(data2)
# rownames(data2) = c(seq(0.1,0.9,0.1))
# colnames(data2) = c("t-test","t-testR","WMW","Welch","DESeq","edgeR","Limma",
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.0,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.1,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.2,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.3,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.4,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.5,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.6,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.7,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.8,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.9,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',1.0,sep="="))
# 
# data2 = as.data.frame(data2)#power_limma=power_limma,
# p7_2 = pheatmap(data2, cluster_row = FALSE, cluster_col = FALSE,legend = FALSE,
#                 display_numbers = TRUE, number_format = "%.3f",number_color = "black",
#                 cellwidth = 20, cellheight = 20,
#                 border_color = "black",angle_col=45,
#                 fontsize_number = 6.5,fontsize=5,
#                 fontsize_row = 5, fontsize_col = 5,
#                 main = expression(paste(delta,"=1, k=1")),
#                 labels_col=c("t-test","t-testR","WMW","Welch","DESeq","edgeR","Limma",
#                              expression(paste("WMW-A(", pi[z],"=0.0)")),expression(paste("WMW-A(", pi[z],"=0.1)"))
#                              ,expression(paste("WMW-A(", pi[z],"=0.2)")),expression(paste("WMW-A(", pi[z],"=0.3)")),
#                              expression(paste("WMW-A(", pi[z],"=0.4)")),expression(paste("WMW-A(", pi[z],"=0.5)")),
#                              expression(paste("WMW-A(", pi[z],"=0.6)")),expression(paste("WMW-A(", pi[z],"=0.7)")),
#                              expression(paste("WMW-A(", pi[z],"=0.8)")),expression(paste("WMW-A(", pi[z],"=0.9)")),
#                              expression(paste("WMW-A(", pi[z],"=1.0)"))),
#                 labels_row =c(expression(paste(pi,"=0.1")),expression(paste(pi,"=0.2")),expression(paste(pi,"=0.3")),expression(paste(pi,"=0.4")),
#                               expression(paste(pi,"=0.5")),expression(paste(pi,"=0.6")),expression(paste(pi,"=0.7")),expression(paste(pi[z],"=0.8")),
#                               expression(paste(pi,"=0.9")))
# )
# 
# p7_2$gtable$grobs[[1]]$gp = grid::gpar(fontfamily="serif")
# p7_2$gtable$grobs[[2]]$gp = grid::gpar(fontfamily="serif")
# p7_2$gtable$grobs[[3]]$gp = grid::gpar(fontfamily="serif")
# p7_2$gtable$grobs[[4]]$gp = grid::gpar(fontfamily="serif")



######################################################################################################

library(pheatmap)

result8 = readMat('Simulation/NB/comb/rate/result_comb_rate_2.mat')
result8_2 = readRDS("Simulation/NB/comb/rate/result_limma_rate_2.rds")

power_t = result8[["power.t"]][1:2,]
power_tR = result8[["power.tR"]][1:2,]
power_wmw = result8[["power.wc"]][1:2,]
power_wc = result8[["power.wmw"]][1:2,]
power_DESeq = result8_2[["power_DESeq"]][1:2,]
power_limma = result8_2[["power_limma"]][1:2,]
power_edgeR = result8_2[["power_edgeR"]][1:2,]
power_wmwa1_d1 = result8[["power.wmwa1"]][1,,1,]
power_wmwa1_d2 = result8[["power.wmwa1"]][2,,1,]

# data1 = matrix(nrow = 18, ncol = 9)
# data1[1,] = power_t[1,]
# data1[2,] = power_tR[1,]
# data1[3,] = power_wmw[1,]
# data1[4,] = power_wc[1,]
# data1[5,] = power_DESeq[1,]
# data1[6,] = power_limma[1,]
# data1[7,] = power_edgeR[1,]
# data1[8:18,] = t(power_wmwa1_d1)
# data1 = t(data1)
# rownames(data1) = c(seq(0.1,0.9,0.1))
# colnames(data1) = c("t-test","t-testR","WMW","Welch","DESeq","edgeR","Limma",
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.0,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.1,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.2,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.3,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.4,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.5,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.6,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.7,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.8,sep="="),
#                     paste('WMW-A(\u03C0\u005F\u007A)',0.9,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',1.0,sep="="))
# data1 = as.data.frame(data1)#power_limma=power_limma,
# p7 = pheatmap(data1, cluster_row = FALSE, cluster_col = FALSE,legend = FALSE,
#               display_numbers = TRUE, number_format = "%.3f",number_color = "black",
#               cellwidth = 20, cellheight = 20,
#               border_color = "black",angle_col=45,
#               fontsize_number = 6.5,fontsize=5,
#               fontsize_row = 5, fontsize_col = 5,
#               main = expression(paste(delta,"=0.5, k=1")),
#               labels_col=c("t-test","t-testR","WMW","Welch","DESeq","edgeR","Limma",
#                            expression(paste("WMW-A(", pi[z],"=0.0)")),expression(paste("WMW-A(", pi[z],"=0.1)"))
#                            ,expression(paste("WMW-A(", pi[z],"=0.2)")),expression(paste("WMW-A(", pi[z],"=0.3)")),
#                            expression(paste("WMW-A(", pi[z],"=0.4)")),expression(paste("WMW-A(", pi[z],"=0.5)")),
#                            expression(paste("WMW-A(", pi[z],"=0.6)")),expression(paste("WMW-A(", pi[z],"=0.7)")),
#                            expression(paste("WMW-A(", pi[z],"=0.8)")),expression(paste("WMW-A(", pi[z],"=0.9)")),
#                            expression(paste("WMW-A(", pi[z],"=1.0)"))),
#               labels_row =c(expression(paste(pi,"=0.1")),expression(paste(pi,"=0.2")),expression(paste(pi,"=0.3")),expression(paste(pi,"=0.4")),
#                             expression(paste(pi,"=0.5")),expression(paste(pi,"=0.6")),expression(paste(pi,"=0.7")),expression(paste(pi[z],"=0.8")),
#                             expression(paste(pi,"=0.9")))
# )
# 
# p7$gtable$grobs[[1]]$gp = grid::gpar(fontfamily="serif")
# p7$gtable$grobs[[2]]$gp = grid::gpar(fontfamily="serif")
# p7$gtable$grobs[[3]]$gp = grid::gpar(fontfamily="serif")
# p7$gtable$grobs[[4]]$gp = grid::gpar(fontfamily="serif")
# 
# 


data2 = matrix(nrow = 18, ncol = 9)
data2[1,] = power_t[2,] 
data2[2,] = power_tR[2,]
data2[3,] = power_wmw[2,]
data2[4,] = power_wc[2,]
data2[5,] = power_DESeq[2,]
data2[6,] = power_limma[2,]
data2[7,] = power_edgeR[2,]
data2[8:18,] = t(power_wmwa1_d2)
data2 = t(data2)
rownames(data2) = c(seq(0.1,0.9,0.1))
colnames(data2) = c("t-test","t-testR","WMW","Welch","DESeq","edgeR","Limma",
                    paste('WMW-A(\u03C0\u005F\u007A)',0.0,sep="="),
                    paste('WMW-A(\u03C0\u005F\u007A)',0.1,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.2,sep="="),
                    paste('WMW-A(\u03C0\u005F\u007A)',0.3,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.4,sep="="),
                    paste('WMW-A(\u03C0\u005F\u007A)',0.5,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.6,sep="="),
                    paste('WMW-A(\u03C0\u005F\u007A)',0.7,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',0.8,sep="="),
                    paste('WMW-A(\u03C0\u005F\u007A)',0.9,sep="="),paste('WMW-A(\u03C0\u005F\u007A)',1.0,sep="="))

data2 = as.data.frame(data2)#power_limma=power_limma,
p7_2 = pheatmap(data2, cluster_row = FALSE, cluster_col = FALSE,legend = FALSE,
                display_numbers = TRUE, number_format = "%.3f",number_color = "black",
                cellwidth = 20, cellheight = 20,
                border_color = "black",angle_col=45,
                fontsize_number = 6.5,fontsize=10,
                fontsize_row = 10, fontsize_col = 10,
                main = expression(paste(paste(alpha[x],"=100, "),paste(alpha[y],"=90, k=1"))),
                labels_col=c("t-test","t-testR","WMW","Welch","DESeq","edgeR","Limma",
                             expression(paste("WMW-A(", pi[z],"=0.0)")),expression(paste("WMW-A(", pi[z],"=0.1)"))
                             ,expression(paste("WMW-A(", pi[z],"=0.2)")),expression(paste("WMW-A(", pi[z],"=0.3)")),
                             expression(paste("WMW-A(", pi[z],"=0.4)")),expression(paste("WMW-A(", pi[z],"=0.5)")),
                             expression(paste("WMW-A(", pi[z],"=0.6)")),expression(paste("WMW-A(", pi[z],"=0.7)")),
                             expression(paste("WMW-A(", pi[z],"=0.8)")),expression(paste("WMW-A(", pi[z],"=0.9)")),
                             expression(paste("WMW-A(", pi[z],"=1.0)"))),
                labels_row =c(expression(paste(pi,"=0.1")),expression(paste(pi,"=0.2")),expression(paste(pi,"=0.3")),expression(paste(pi,"=0.4")),
                              expression(paste(pi,"=0.5")),expression(paste(pi,"=0.6")),expression(paste(pi,"=0.7")),expression(paste(pi[z],"=0.8")),
                              expression(paste(pi,"=0.9")))
)

p7_2$gtable$grobs[[1]]$gp = grid::gpar(fontfamily="sans")
p7_2$gtable$grobs[[2]]$gp = grid::gpar(fontfamily="sans")
p7_2$gtable$grobs[[3]]$gp = grid::gpar(fontfamily="sans")
p7_2$gtable$grobs[[4]]$gp = grid::gpar(fontfamily="sans")




pp1 = p1+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
pa = pp1|p2|p3 
pb = p6+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))

pp7 = ggplotify::as.ggplot(p7)
pc = pp7+labs(tag = expression(paste(bold("c"))))+theme(text = element_text(family = "sans"))

pp7_2 = ggplotify::as.ggplot(p7_2)
pd = pp7_2+labs(tag = expression(paste(bold("d"))))+theme(text = element_text(family = "sans"))


# library(ggpubr)
# pbc=ggarrange(pb,pc,nrow = 1,widths = c(1,1.5))
# ggarrange(pa,pbc,nrow = 2)

library(patchwork)
design <- "AABBCC
           DDEEE#"
wrap_plots(A = pp1, B = p2, C=p3,D=pb,E=pc, design = design)



ggsave("Figure/Out_fig/NB_comb_fig.png", width = 10.5,height = 7.5)

