library(customLayout)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(ggpubr)


palvqq = ggplotify::as.ggplot(alvqq)#+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
ggsave("../../Out_fig/alv_fig_qq.png", width = 13,plot = palvqq,height = 3.5)




pnull = plot.new()
palvvenn = ggplotify::as.ggplot(alvvenn)+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))

ppp3 = pp1_3+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))

pa=ggarrange(pp1_3, pp1_5,pp1_6, pp1_7,pp1_4, nrow = 1,widths = c(1,1,1,1,1.7))
pa = ggplotify::as.ggplot(pa)
ppa = pa + labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
ppa


lay  <- lay_new(matrix(1:8, ncol = 4))
lay2 <- lay_new(matrix(1:2, ncol =2), widths = c(7,1))
lay3 <- lay_new(matrix(1:3, ncol =3), widths = c(2,2,1))
lay4 <- lay_new(matrix(1:2, ncol =2), widths = c(2,3))
cl   <- lay_bind_row(lay3, lay4, heights = c(1,1))
cl   <- lay_bind_col(lay, cl, widths = c(8,5))
cl   <- lay_bind_row(lay2, cl, heights = c(1.1,2))

lay_show(cl)

plots = list(ppa, pnull,pp1_15, pp1_8,
             pp1_16, pp1_9, 
             pp1_1, pp1_14, 
             pp1_2, pp1_11, 
             pp1_12, pp1_13, pnull,  
             palvdeg, palvvenn) #,ppc
p=lay_grid(plots, cl)
pp = ggplotify::as.ggplot(p)

ggsave("../../Out_fig/alv_fig_ab.png", width = 16,height = 8.5)



########################################################################

pbroqq = ggplotify::as.ggplot(broqq)+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))

pgobro = ggplotify::as.ggplot(gobro)+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))

pad=ggarrange(pbroqq, pgobro, nrow = 2, heights = c(3.8,6))
pad = ggplotify::as.ggplot(pad)
pad

ggsave("../../Out_fig/bro_fig_ab.png", width = 14,height = 9.8)










pbrovenn = ggplotify::as.ggplot(brovenn)+labs(tag = expression(paste(bold("b"))))+theme(text = element_text(family = "sans"))

ppp2_3 = pp2_3+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))

pa=ggarrange(pp2_3, pp2_5,pp2_6, pp2_7,pp2_4, nrow = 1,widths = c(1,1,1,1,1.7))
pa = ggplotify::as.ggplot(pa)
ppa = pa + labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
ppa


lay  <- lay_new(matrix(1:8, ncol = 4))
lay2 <- lay_new(matrix(1:2, ncol =2), widths = c(7,1))
lay3 <- lay_new(matrix(1:3, ncol =3), widths = c(2,2,1))
lay4 <- lay_new(matrix(1:2, ncol =2), widths = c(2,3))
cl   <- lay_bind_row(lay3, lay4, heights = c(1,1))
cl   <- lay_bind_col(lay, cl, widths = c(8,5))
cl   <- lay_bind_row(lay2, cl, heights = c(1.1,2))

lay_show(cl)

plots = list(ppa, pnull,pp2_15, pp2_8,
             pp2_16, pp2_9, 
             pp2_1, pp2_14, 
             pp2_2, pp2_11, 
             pp2_12, pp2_13, pnull,  
             palvdeg, palvvenn) #,ppc
p=lay_grid(plots, cl)
pp = ggplotify::as.ggplot(p)
ggsave("../../Out_fig/bro_fig_supp.png", width = 16,height = 8.5)



