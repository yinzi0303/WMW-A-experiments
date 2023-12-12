
# palvqq1 = ggplotify::as.ggplot(alvqq1)+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
# palvqq2 = ggplotify::as.ggplot(alvqq2)+theme(text = element_text(family = "sans"))
# palvqq3 = ggplotify::as.ggplot(alvqq3)+theme(text = element_text(family = "sans"))
# palvqq4 = ggplotify::as.ggplot(alvqq4)+theme(text = element_text(family = "sans"))

palvvenn = ggplotify::as.ggplot(alvvenn)+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))

ppp1 = pp1_1+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
ppp3 = pp1_3+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))


library(customLayout)
library(ggplot2)
library(gridExtra)

pnull = plot.new()

lay  <- lay_new(matrix(1:12, ncol = 4))
lay2 <- lay_new(matrix(1:3, ncol =3), widths = c(9,2,2))
lay3 <- lay_new(matrix(1:3, ncol =3), widths = c(2,2,1))
lay4 <- lay_new(matrix(1:2, ncol =2), widths = c(2,3))
cl   <- lay_bind_row(lay3, lay4, heights = c(1,1))
cl   <- lay_bind_row(lay2, cl, heights = c(1,2))
cl   <- lay_bind_col(lay, cl, widths = c(8,5))
lay_show(cl)

# lay  <- lay_new(matrix(1:12, ncol = 4))
# lay2 <- lay_new(matrix(1:3, ncol =3), widths = c(3.5,1,1))
# lay3 <- lay_new(matrix(1:3, ncol =3), widths = c(1,1,1))
# lay4 <- lay_new(matrix(1:2, ncol =2), widths = c(1,2))
# cl   <- lay_bind_row(lay3, lay4, heights = c(1,1))
# cl   <- lay_bind_row(lay2, cl, heights = c(1,2))
# cl   <- lay_bind_col(lay, cl, widths = c(4,3))
# lay_show(cl)

# lay  <- lay_new(matrix(1:12, ncol = 4))
# lay2 <- lay_new(matrix(1:3, ncol =3), widths = c(8,1,1))
# lay3 <- lay_new(matrix(1:3, ncol =3), widths = c(1,1,1.5))
# lay4 <- lay_new(matrix(1:2, ncol =2), widths = c(1,2.5))
# cl   <- lay_bind_row(lay3, lay4, heights = c(1,1))
# cl   <- lay_bind_row(lay2, cl, heights = c(1,2))
# cl   <- lay_bind_col(lay, cl, widths = c(4,3.5))
# lay_show(cl)


# plots = list(ppp1,pp1_2,pp1_3, pp1_5,pp1_6, pp1_7, pp1_8, pp1_9, pp1_11, pp1_12,pp1_13, pp1_14, 
#              pp1_4, pnull, pnull,pp1_15, pp1_16, pnull,palvdeg, palvvenn) #,ppc
plots = list(ppp3, pp1_15, pp1_8,
             pp1_5, pp1_16, pp1_9, 
             pp1_6, pp1_1, pp1_14, 
             pp1_7, pp1_2, pp1_11, 
             pp1_4, pnull, pnull,
             pp1_12, pp1_13, pnull,  
             palvdeg, palvvenn) #,ppc
p=lay_grid(plots, cl)
pp = ggplotify::as.ggplot(p)

ggsave("../../Out_fig/alv_fig_ab_new.png", width = 14,height = 7.5)




library(patchwork)
# design <- "IIIJJJKKKMMMM#
#            IIIJJJKKKMMMM#
#            IIIJJJKKKMMMM#
#            AAABBBCCCDDDDD
#            AAABBBCCCDDDDD
#            EEEFFFGGGHHH##
#            EEEFFFGGGHHH##"

# design <- "IIIIJJJJKKKKMMMMNNNNN##
#            IIIIJJJJKKKKMMMMNNNNN##
#            IIIIJJJJKKKKMMMMNNNNN##
#            IIIIJJJJKKKKMMMMNNNNN##
#            AAAABBBBCCCCDDDDRRRRRRR
#            AAAABBBBCCCCDDDDRRRRRRR
#            EEEEFFFFGGGGHHHHSSSS###
#            EEEEFFFFGGGGHHHHSSSS###
#            UUUUVVVVWWWWXXXX#######
#            UUUUVVVVWWWWXXXX#######"
# wrap_plots(A = ppp1, B = pp1_2, C=pp1_3, D=pp1_5, R=pp1_4, 
#            E = pp1_6, F=pp1_7, G=pp1_8, H=pp1_9, S=pp1_11,
#            U=pp1_12, V=pp1_13, W=pp1_14, X=palvdeg,
#            I=palvqq1,J=palvqq2,K=palvqq3,M=palvqq4,N=palvvenn,design = design)


# design <- "IIIIJJJJKKKKMMMM##
#            IIIIJJJJKKKKMMMM##
#            IIIIJJJJKKKKMMMM##
#            IIIIJJJJKKKKMMMM##
#            AAAABBBBCCCCDDDDDD
#            AAAABBBBCCCCDDDDDD
#            EEEEFFFFGGGGHHHH##
#            EEEEFFFFGGGGHHHH##
#            UUUUVVVVWWWWXXXXXX
#            UUUUVVVVWWWWXXXXXX
#            NNNNRRRRSSSSXXXXXX
#            NNNNRRRRSSSSXXXXXX"
# wrap_plots(A = ppp1, B = pp1_2, C=pp1_3, D=pp1_4, 
#            E = pp1_5, F=pp1_6, G=pp1_7, H=pp1_8, 
#            U=pp1_9, V=pp1_11, W=pp1_12, X=palvdeg,
#            N=pp1_13, R=pp1_14, S=palvdeg, X=palvvenn,
#            I=palvqq1,J=palvqq2,K=palvqq3,M=palvqq4,design = design)


palvqq = ggplotify::as.ggplot(alvqq)#+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
ggsave("../../Out_fig/alv_fig_qq.png", width = 13,plot = palvqq,height = 3.5)
# design <- "AAABBBCCCDDDDD
#            AAABBBCCCDDDDD
#            EEEFFFGGGHHH##
#            EEEFFFGGGHHH##
#            UUUVVVWWWXXXX#
#            UUUVVVWWWXXXX#
#            NNNRRRSSS#####
#            NNNRRRSSS#####"
# wrap_plots(A = ppp1, B = pp1_2, C=pp1_3, D=pp1_4, 
#            E = pp1_5, F=pp1_6, G=pp1_7, H=pp1_8, 
#            U=pp1_9, V=pp1_11, W=pp1_12, X=palvdeg,
#            N=pp1_13, R=pp1_14, S=palvdeg, X=palvvenn,
#            design = design)


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


# 
# pa/pb
# pb = (ppalvdeg|pp1|pp2|pp3)/(pp4|pp5|pp6|pp7)
# 
# 
# 
# 
# ppbrodeg = ggplotify::as.ggplot(brodeg)
# ppbrodeg = ppbrodeg+labs(tag = "B")
# pp1 = ggplotify::as.ggplot(p8)
# pp2 = ggplotify::as.ggplot(p9)
# pp3 = ggplotify::as.ggplot(p10)
# pp4 = ggplotify::as.ggplot(p11)
# pp5 = ggplotify::as.ggplot(p12)
# pp6 = ggplotify::as.ggplot(p13)
# pp7 = ggplotify::as.ggplot(p14)
# pb = (ppbrodeg|pp1|pp2|pp3)/(pp4|pp5|pp6|pp7)
# 
# 
# ggsave("C:/Users/dell/Desktop/wmwa/Z/lunwen_1g/Rfig/alv_qq_fig.png", width = 12,height = 4)
# ggsave("C:/Users/dell/Desktop/wmwa/Z/lunwen_1g/Rfig/bro_qq_fig.png", width = 12,height = 4)
# 
# ggsave("C:/Users/dell/Desktop/wmwa/Z/lunwen_1g/Rfig/bro_go_fig.png", width = 12,height = 4.5)
# 
# 
# ggsave("C:/Users/dell/Desktop/wmwa/Z/lunwen_1g/Rfig/bro_deg_fig.png", width = 18,height = 8)
# alvqq = alvgo
# broqq=brogo
# 
# 
# palv = list(alvqq,alvdeg,p1,p2,p3,p4,p5,p6,p7)
# pbro = list(broqq,brodeg,p8,p9,p10,p11,p12,p13,p14,brogo)
# palvbro = list(palv,pbro)
# saveRDS(palvbro,"palvbro.rds")
# 
# pdeg = list(alvdeg,p1,p2,p3,p4,p5,p6,p7,brodeg,p8,p9,p10,p11,p12,p13,p14)
# pqq = list(alvqq, broqq)
# brogo = gobro
