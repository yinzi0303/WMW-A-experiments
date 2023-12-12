

palvvenn = ggplotify::as.ggplot(alvvenn)+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))

ppp1 = pp1_1+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))



library(patchwork)

palvqq = ggplotify::as.ggplot(alvqq)#+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
ggsave("../../Out_fig/alv_fig_qq.png", width = 13,plot = palvqq,height = 3.5)





design <- "AAAABBBBCCCCEEEEDDDDDDD
           AAAABBBBCCCCEEEEDDDDDDD
           FFFFGGGGHHHHUUUUVVVV###
           FFFFGGGGHHHHUUUUVVVV###
           WWWWNNNNRRRRSSSSXXXXXXX
           WWWWNNNNRRRRSSSSXXXXXXX"
wrap_plots(A = ppp1, B = pp1_2, C=pp1_3, D=pp1_4, 
           E = pp1_5, F=pp1_6, G=pp1_7, H=pp1_8, 
           U=pp1_9, V=pp1_11, W=pp1_12, X=palvdeg,
           N=pp1_13, R=pp1_14, S=palvdeg, X=palvvenn,
           design = design)
#1400,700

ggsave("../../Out_fig/alv_fig_ab.png", width = 14,height = 7)



# pbroqq1 = ggplotify::as.ggplot(broqq1)+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
# pbroqq2 = ggplotify::as.ggplot(broqq2)+theme(text = element_text(family = "sans"))
# pbroqq3 = ggplotify::as.ggplot(broqq3)+theme(text = element_text(family = "sans"))

pbrovenn = ggplotify::as.ggplot(brovenn)+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))

ppp2 = pp2_1+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))

ggplotify::as.ggplot(broqq)+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
ggsave("../../Out_fig/bro_fig_a.png", width = 14,height = 3.8)


design <- "AAAABBBBCCCCEEEEDDDDDDD
           AAAABBBBCCCCEEEEDDDDDDD
           FFFFGGGGHHHHUUUUVVVV###
           FFFFGGGGHHHHUUUUVVVV###
           WWWWNNNNRRRRSSSSXXXXXXX
           WWWWNNNNRRRRSSSSXXXXXXX"
wrap_plots(A = ppp2, B = pp2_2, C=pp2_3, D=pp2_4, 
           E = pp2_5, F=pp2_6, G=pp2_7, H=pp2_8, 
           U=pp2_9, V=pp2_11, W=pp2_12, X=palvdeg,
           N=pp2_13, R=pp2_14, S=pbrodeg, X=pbrovenn,
           design = design)#M=pgobro,
ggsave("../../Out_fig/bro_fig_supp.png", width = 14,height = 7)


ggplotify::as.ggplot(gobro)+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))
ggsave("../../Out_fig/bro_fig_d.png", width = 14,height = 6)

