library(lattice)
library(RColorBrewer)
library(ggplot2)
qqunif.plot<-function(pvalues, 
                      should.thin=T, thin.obs.places=2, thin.exp.places=2, 
                      xlab=expression(paste("Expected (",-log[10], " p-value)")),
                      ylab=expression(paste("Observed (",-log[10], " p-value)")),
                      title="t-test",col=1:11,key=NULL,main="QQ-plot",
                      draw.conf=T, conf.points=1000, conf.col="lightgray", conf.alpha=.05,
                      already.transformed=FALSE, pch=1, aspect="iso", prepanel=prepanel.qqunif,
                      par.settings=list(superpose.symbol=list(pch=pch,font=0.5)), ...) {
  
  
  #error checking
  if (length(pvalues)==0) stop("pvalue vector is empty, can't draw plot")
  if(!(class(pvalues)=="numeric" || 
       (class(pvalues)=="list" && all(sapply(pvalues, class)=="numeric"))))
    stop("pvalue vector is not numeric, can't draw plot")
  if (any(is.na(unlist(pvalues)))) stop("pvalue vector contains NA values, can't draw plot")
  if (already.transformed==FALSE) {
    if (any(unlist(pvalues)==0)) stop("pvalue vector contains zeros, can't draw plot")
  } else {
    if (any(unlist(pvalues)<0)) stop("-log10 pvalue vector contains negative values, can't draw plot")
  }
  
  
  grp<-NULL
  n<-1
  exp.x<-c()
  if(is.list(pvalues)) {
    nn<-sapply(pvalues, length)
    rs<-cumsum(nn)
    re<-rs-nn+1
    n<-min(nn)
    if (!is.null(names(pvalues))) {
      grp=factor(rep(names(pvalues), nn), levels=names(pvalues))
      names(pvalues)<-NULL
    } else {
      grp=factor(rep(1:length(pvalues), nn))
    }
    pvo<-pvalues
    pvalues<-numeric(sum(nn))
    exp.x<-numeric(sum(nn))
    for(i in 1:length(pvo)) {
      if (!already.transformed) {
        pvalues[rs[i]:re[i]] <- -log10(pvo[[i]])
        exp.x[rs[i]:re[i]] <- -log10((rank(pvo[[i]], ties.method="first")-.5)/nn[i])
      } else {
        pvalues[rs[i]:re[i]] <- pvo[[i]]
        exp.x[rs[i]:re[i]] <- -log10((nn[i]+1-rank(pvo[[i]], ties.method="first")-.5)/(nn[i]+1))
      }
    }
  } else {
    n <- length(pvalues)+1
    if (!already.transformed) {
      exp.x <- -log10((rank(pvalues, ties.method="first")-.5)/n)
      pvalues <- -log10(pvalues)
    } else {
      exp.x <- -log10((n-rank(pvalues, ties.method="first")-.5)/n)
    }
  }
  
  
  #this is a helper function to draw the confidence interval
  panel.qqconf<-function(n, conf.points=1000, conf.col="gray", conf.alpha=.05, ...) {
    require(grid)
    conf.points = min(conf.points, n-1);
    mpts<-matrix(nrow=conf.points*2, ncol=2)
    for(i in seq(from=1, to=conf.points)) {
      mpts[i,1]<- -log10((i-.5)/n)
      mpts[i,2]<- -log10(qbeta(1-conf.alpha/2, i, n-i))
      mpts[conf.points*2+1-i,1]<- -log10((i-.5)/n)
      mpts[conf.points*2+1-i,2]<- -log10(qbeta(conf.alpha/2, i, n-i))
    }
    grid.polygon(x=mpts[,1],y=mpts[,2], gp=gpar(fill=conf.col, lty=0), default.units="native")
  }
  
  #reduce number of points to plot
  if (should.thin==T) {
    if (!is.null(grp)) {
      thin <- unique(data.frame(pvalues = round(pvalues, thin.obs.places),
                                exp.x = round(exp.x, thin.exp.places),
                                grp=grp))
      grp = thin$grp
    } else {
      thin <- unique(data.frame(pvalues = round(pvalues, thin.obs.places),
                                exp.x = round(exp.x, thin.exp.places)))
    }
    pvalues <- thin$pvalues
    exp.x <- thin$exp.x
  }
  gc()
  
  prepanel.qqunif= function(x,y,...) {
    A = list()
    A$xlim = range(x, y)*1.02
    A$xlim[1]=0
    A$ylim = A$xlim
    return(A)
  }
  
  # draw the plot
  xyplot(pvalues~exp.x, groups=grp, xlab=xlab, ylab=ylab, aspect=aspect,
         prepanel=prepanel, scales=list(axs="i"), pch=pch, main=main,cex=0.3,
         panel = function(x, y, ...) {
           if (draw.conf) {
             panel.qqconf(n, conf.points=conf.points,
                          conf.col=conf.col, conf.alpha=conf.alpha)
           };
           panel.xyplot(x,y, ...);
           panel.abline(0,1);
         }, par.settings=par.settings, ...
  )
  
  
  # xyplot(pvalues~exp.x, groups=grp, xlab=xlab, ylab=ylab, aspect=aspect,
  #        prepanel=prepanel, scales=list(axs="i"), pch=pch, main=main,col=brewer.pal(11,"Spectral"),
  #        panel = function(x, y, ...) {
  #          if (draw.conf) {
  #            panel.qqconf(n, conf.points=conf.points, 
  #                         conf.col=conf.col, conf.alpha=conf.alpha)
  #          };
  #          panel.xyplot(x,y, ...);
  #          panel.abline(0,1);
  #        },
  #        par.settings = par.settings,
  #        
  #        # ltext(x = x, y = y, labels = groups[subscripts], cex=1,
  #              # fontfamily = "HersheySans")
  #        
  #        # auto.key = list(space = "right")
  #        # par.settings=par.settings, key = simpleKey(levels(grp), space = "right")
  # )
  
}






result_my = readRDS("../../../Application/WMWAresult_185657.rds") #../../../
ind = 1
comre = readRDS("../../../Application/compresult_alv.rds") #../../../

dataset = readRDS('../../../Application/GSE185657.rds') 
data = dataset[["expr"]]
genes = rownames(data)

library(reticulate) 
np<-import("numpy") #datareading 
path = '/Users/guoyin/Desktop/github/'
mat_alv<-np$load(paste0(path, "consexpression-master/results/alv/alv_deg.npy"), allow_pickle=TRUE) 
Res_consexpression = mat_alv[[1]][["Consexp"]][["0"]][["0"]]
Res_NOISeq = mat_alv[[1]][["NOISeq"]][["0"]][["0"]]

mat_BaySeq = read.csv(paste0(path, "consexpression-master/results/alv/alv_bayseq.csv"), sep = '\t')
idx = match(genes, rownames(mat_BaySeq))
mat_BaySeq_new = mat_BaySeq[idx,]
Res_BaySeq = mat_BaySeq_new[, 'FDR.DE']

mat_alv2<-np$load(paste0(path, "DEGnext_code/results/alv/alv_deg.npy"), allow_pickle=TRUE) 
Res_DEGnext = as.data.frame(mat_alv2[[1]][["0"]][["0"]], row.names = genes)
colnames(Res_DEGnext) = 'FDR.DE'




Res_WMWAN = result_my[["Res"]][["WMWAN"]][[ind]]
Res_WMWALN = result_my[["Res"]][["WMWALN"]][[ind]]
Res_WMWANB = result_my[["Res"]][["WMWANB"]][[ind]]
Res_WMWANM = result_my[["Res"]][["WMWANM"]][[ind]]
Res_WMWALNM = result_my[["Res"]][["WMWALNM"]][[ind]]
Res_WMWANBM = result_my[["Res"]][["WMWANBM"]][[ind]]


# Res_WMWAN = p.adjust(result_my[["Res"]][["WMWAN"]][[ind]], method = "fdr")
# Res_WMWALN = p.adjust(result_my[["Res"]][["WMWALN"]][[ind]], method = "fdr")
# Res_WMWANB = p.adjust(result_my[["Res"]][["WMWANB"]][[ind]], method = "fdr")
# Res_WMWANM = p.adjust(result_my[["Res"]][["WMWANM"]][[ind]], method = "fdr")
# Res_WMWALNM = p.adjust(result_my[["Res"]][["WMWALNM"]][[ind]], method = "fdr")
# Res_WMWANBM = p.adjust(result_my[["Res"]][["WMWANBM"]][[ind]], method = "BH")


Res_WMWAN[which(Res_WMWAN==0)]=1e-4
Res_WMWALN[which(Res_WMWALN==0)]=1e-4
Res_WMWANB[which(Res_WMWANB==0)]=1e-4
Res_WMWALNM[which(Res_WMWALNM==0)]=1e-4
Res_WMWANM[which(Res_WMWANM==0)]=1e-4
Res_WMWANBM[which(Res_WMWANBM==0)]=1e-4


# wmwa=readMat("result_scLG1norm_178331_5.mat")
# wmwaG=wmwa[["wmwaG1.1.2"]][1,]
# wmwaG[which(wmwaG==0)]=1e-3


Res_DESeq=comre[["DESeq"]]
Res_edgeR=comre[["edgeR"]]
Res_Limma=comre[["Limma"]]
Res_wilx=comre[["wilx"]]
Res_ttest=comre[["ttest"]]
Res_ttestR=comre[["ttestR"]]
Res_welch=comre[["welch"]]
Res_MAST = comre[["MAST"]]
Res_monocle = comre[["monocle"]]
Res_scde = comre[["scde"]]

# Res_DESeq[which(Res_DESeq==0)]=1e-4
# Res_edgeR[which(Res_edgeR==0)]=1e-4
# Res_Limma[which(Res_Limma==0)]=1e-4
# Res_ttest[which(Res_ttest==0)]=1e-4
Res_ttestR[which(Res_ttestR==0)]=1e-4
# Res_welch[which(Res_welch==0)]=1e-4

Res_monocle[which(Res_monocle<1e-10)]=1e-4

Res_zingeR = comre[["zingeR"]]
Res_Agg = comre[["Agg"]]
Res_DEsingle = comre[["DEsingle"]]
Res_muscat = as.vector(comre[["muscat"]])

Res_zingeR[which(Res_zingeR==0)]=1e-4
Res_Agg[which(Res_Agg==0)]=1e-4
Res_DEsingle[which(Res_DEsingle==0)]=1e-4
Res_muscat[which(Res_muscat==0)]=1e-4

Res_zingeR[which(is.na(Res_zingeR))]=1
Res_Agg[which(is.na(Res_Agg))]=1
Res_DEsingle[which(is.na(Res_DEsingle))]=1
Res_muscat[which(is.na(Res_muscat))]=1



my.pvalue.list<-list("Limma"=Res_Limma,"DESeq"=Res_DESeq, "edgeR"=Res_edgeR, "t-test"=Res_ttest, "t-testR"=Res_ttestR,
                     "Welch"=Res_welch, "WMW"=Res_wilx, 
                     "wmwa-LG"=Res_WMWALN,"wmwa-LGM"=Res_WMWALNM,
                     "wmwa-NB"=Res_WMWANB,"wmwa-NBM"=Res_WMWANBM)

my.pvalue.list1<-list("DESeq"=Res_DESeq, "edgeR"=Res_edgeR,"MAST"=Res_MAST, 
                      "Monocle"=Res_monocle,"SCDE"=Res_scde, 'Bayseq'=Res_BaySeq
)
alvqq1=qqunif.plot(my.pvalue.list1, main = " ", 
                   par.settings = list(superpose.symbol = list(pch=1, col = brewer.pal(6,"Set2"), fontfamily = "serif")),
                   auto.key=list(corner=c(.95,.05)))

alvqq1

my.pvalue.list2<-list("Limma"=Res_Limma, "t-test"=Res_ttest, "t-testR"=Res_ttestR,
                      "Welch"=Res_welch, "WMW"=Res_wilx
)
alvqq2=qqunif.plot(my.pvalue.list2, main = " ", 
                   par.settings = list(superpose.symbol = list(pch=1, col = brewer.pal(5,"Paired"), fontfamily = "serif")),
                   auto.key=list(corner=c(.95,.05)))

alvqq2

my.pvalue.list3<-list("ZINB-WaVE" = Res_zingeR,
                      "aggregateBioVar" = Res_Agg,
                      "DEsingle" = Res_DEsingle,
                      "muscat" = Res_muscat
)
alvqq3=qqunif.plot(my.pvalue.list3, main = " ",
                   par.settings = list(superpose.symbol = list(pch=1, col = brewer.pal(4,"Set1"), fontfamily = "serif")),
                   auto.key=list(corner=c(.95,.05)))

alvqq3


my.pvalue.list4<-list("WMW-A-LG"=Res_WMWALN,"WMW-A-LGM"=Res_WMWALNM,
                      "WMW-A-NB"=Res_WMWANB,"WMW-A-NBM"=Res_WMWANBM)
alvqq4=qqunif.plot(my.pvalue.list4, main = " ", 
                   par.settings = list(superpose.symbol = list(pch=1, col = brewer.pal(4,"Dark2"), fontfamily = "serif")),
                   key = simpleKey(levels(space = "right"),fontsize=10),
                   auto.key=list(fontsize=10,corner=c(.95,.05)))
alvqq4

palvqq1 = ggplotify::as.ggplot(alvqq1)+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
alvqq=ggpubr::ggarrange(alvqq1, alvqq2, alvqq3,alvqq4,nrow=1)
# 
# ggsave("alv_qq_2.png",width = 7,height = 3.5)
# 
# library("ggpubr")
# ggarrange(p1, p2,nrow=1,labels = c("A", "B"))
# 
# 
# ggsave("alv_qq_2.png",width = 10.6,height = 3.8)





#############################################################################################

result_my = readRDS("../../../Application/WMWAresult_185657.rds")
ind = 2
comre = readRDS("../../../Application/compresult_bro.rds")

path = '/Users/guoyin/Desktop/github/'
mat_bro<-np$load(paste0(path, "consexpression-master/results/bro/bro_deg.npy"), allow_pickle=TRUE) 
Res_consexpression = mat_bro[[1]][["Consexp"]][["0"]][["0"]]
Res_NOISeq = mat_bro[[1]][["NOISeq"]][["0"]][["0"]]

mat_BaySeq = read.csv(paste0(path, "consexpression-master/results/bro/bro_bayseq.csv"), sep = '\t')
idx = match(genes, rownames(mat_BaySeq))
mat_BaySeq_new = mat_BaySeq[idx,]
Res_BaySeq = mat_BaySeq_new[, 'FDR.DE']


mat_bro2<-np$load(paste0(path, "DEGnext_code/results/bro/bro_deg.npy"), allow_pickle=TRUE) 
Res_DEGnext = as.data.frame(mat_bro2[[1]][["0"]][["0"]], row.names = genes)
colnames(Res_DEGnext) = 'FDR.DE'

Res_WMWAN = result_my[["Res"]][["WMWAN"]][[ind]]
Res_WMWALN = result_my[["Res"]][["WMWALN"]][[ind]]
Res_WMWANB = result_my[["Res"]][["WMWANB"]][[ind]]
Res_WMWANM = result_my[["Res"]][["WMWANM"]][[ind]]
Res_WMWALNM = result_my[["Res"]][["WMWALNM"]][[ind]]
Res_WMWANBM = result_my[["Res"]][["WMWANBM"]][[ind]]


# Res_WMWAN = p.adjust(result_my[["Res"]][["WMWAN"]][[ind]], method = "fdr")
# Res_WMWALN = p.adjust(result_my[["Res"]][["WMWALN"]][[ind]], method = "fdr")
# Res_WMWANB = p.adjust(result_my[["Res"]][["WMWANB"]][[ind]], method = "fdr")
# Res_WMWANM = p.adjust(result_my[["Res"]][["WMWANM"]][[ind]], method = "fdr")
# Res_WMWALNM = p.adjust(result_my[["Res"]][["WMWALNM"]][[ind]], method = "fdr")
# Res_WMWANBM = p.adjust(result_my[["Res"]][["WMWANBM"]][[ind]], method = "BH")


Res_WMWAN[which(Res_WMWAN==0)]=1e-4
Res_WMWALN[which(Res_WMWALN==0)]=1e-4
Res_WMWANB[which(Res_WMWANB==0)]=1e-4
Res_WMWALNM[which(Res_WMWALNM==0)]=1e-4
Res_WMWANM[which(Res_WMWANM==0)]=1e-4
Res_WMWANBM[which(Res_WMWANBM==0)]=1e-4


# wmwa=readMat("result_scLG1norm_178331_5.mat")
# wmwaG=wmwa[["wmwaG1.1.2"]][1,]
# wmwaG[which(wmwaG==0)]=1e-3


Res_DESeq=comre[["DESeq"]]
Res_edgeR=comre[["edgeR"]]
Res_Limma=comre[["Limma"]]
Res_wilx=comre[["wilx"]]
Res_ttest=comre[["ttest"]]
Res_ttestR=comre[["ttestR"]]
Res_welch=comre[["welch"]]
Res_MAST = comre[["MAST"]]
Res_monocle = comre[["monocle"]]
Res_scde = comre[["scde"]]

# Res_DESeq[which(Res_DESeq==0)]=1e-4
# Res_edgeR[which(Res_edgeR==0)]=1e-4
# Res_Limma[which(Res_Limma==0)]=1e-4
# Res_ttest[which(Res_ttest==0)]=1e-4
Res_ttestR[which(Res_ttestR==0)]=1e-4
# Res_welch[which(Res_welch==0)]=1e-4

Res_monocle[which(Res_monocle<1e-10)]=1e-4

Res_zingeR = comre[["zingeR"]]
Res_Agg = comre[["Agg"]]
Res_DEsingle = comre[["DEsingle"]]
Res_muscat = as.vector(comre[["muscat"]])

Res_zingeR[which(Res_zingeR==0)]=1e-4
Res_Agg[which(Res_Agg==0)]=1e-4
Res_DEsingle[which(Res_DEsingle==0)]=1e-4
Res_muscat[which(Res_muscat==0)]=1e-4

Res_zingeR[which(is.na(Res_zingeR))]=1
Res_Agg[which(is.na(Res_Agg))]=1
Res_DEsingle[which(is.na(Res_DEsingle))]=1
Res_muscat[which(is.na(Res_muscat))]=1



my.pvalue.list<-list("Limma"=Res_Limma,"DESeq"=Res_DESeq, "edgeR"=Res_edgeR, "t-test"=Res_ttest, "t-testR"=Res_ttestR,
                     "Welch"=Res_welch, "WMW"=Res_wilx, 
                     "wmwa-LG"=Res_WMWALN,"wmwa-LGM"=Res_WMWALNM,
                     "wmwa-NB"=Res_WMWANB,"wmwa-NBM"=Res_WMWANBM)

my.pvalue.list1<-list("DESeq"=Res_DESeq, "edgeR"=Res_edgeR,"MAST"=Res_MAST, 
                      "Monocle"=Res_monocle,"SCDE"=Res_scde, 'Bayseq'=Res_BaySeq
)
broqq1=qqunif.plot(my.pvalue.list1, main = " ", 
                   par.settings = list(superpose.symbol = list(pch=1, col = brewer.pal(6,"Set2"), fontfamily = "serif")),
                   auto.key=list(corner=c(.95,.05)))

broqq1

my.pvalue.list2<-list("Limma"=Res_Limma, "t-test"=Res_ttest, "t-testR"=Res_ttestR,
                      "Welch"=Res_welch, "WMW"=Res_wilx
)
broqq2=qqunif.plot(my.pvalue.list2, main = " ", 
                   par.settings = list(superpose.symbol = list(pch=1, col = brewer.pal(5,"Paired"), fontfamily = "serif")),
                   auto.key=list(corner=c(.95,.05)))

broqq2

my.pvalue.list3<-list("ZINB-WaVE" = Res_zingeR,
                      "aggregateBioVar" = Res_Agg,
                      "DEsingle" = Res_DEsingle,
                      "muscat" = Res_muscat
)
broqq3=qqunif.plot(my.pvalue.list3, main = " ",
                   par.settings = list(superpose.symbol = list(pch=1, col = brewer.pal(4,"Set1"), fontfamily = "serif")),
                   auto.key=list(corner=c(.95,.05)))

broqq3


my.pvalue.list4<-list("WMW-A-LG"=Res_WMWALN,"WMW-A-LGM"=Res_WMWALNM,
                      # "WMW-A-N"=Res_WMWALN,"WMW-A-NM"=Res_WMWALNM,
                      "WMW-A-NB"=Res_WMWANB,"WMW-A-NBM"=Res_WMWANBM)
broqq4=qqunif.plot(my.pvalue.list4, main = " ", 
                   par.settings = list(superpose.symbol = list(pch=1, col = brewer.pal(4,"Dark2"), fontfamily = "serif")),
                   key = simpleKey(levels(space = "right"),fontsize=10),
                   auto.key=list(fontsize=10,corner=c(.95,.05)))
broqq4

pbroqq1 = ggplotify::as.ggplot(broqq1)+labs(tag = expression(paste(bold("a"))))+theme(text = element_text(family = "sans"))
broqq=ggpubr::ggarrange(broqq1, broqq2, broqq3, broqq4,nrow=1)

