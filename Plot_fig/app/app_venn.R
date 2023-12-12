Res_WMWANBM = result_my[["Res"]][["WMWANBM"]][[1]]
Res_WMWANBM[which(Res_WMWANBM==0)]=1e-4

dl <- list(gene = id[,2], padj = Res_WMWANBM, logfc = logfc[["logfc1"]])
d = which(dl$padj < 0.01 & abs(dl$logfc)>1)

comre = readRDS("../../../Application/compresult_alv.rds")


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

dl <- list(gene = id[,2], padj = Res_DESeq, logfc = logfc[["logfc1"]])
d1 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_edgeR, logfc = logfc[["logfc1"]])
d2 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_Limma, logfc = logfc[["logfc1"]])
d3 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_wilx, logfc = logfc[["logfc1"]])
d4 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_ttest, logfc = logfc[["logfc1"]])
d5 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_ttestR, logfc = logfc[["logfc1"]])
d6 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_welch, logfc = logfc[["logfc1"]])
d7 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_MAST, logfc = logfc[["logfc1"]])
d8 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_monocle, logfc = logfc[["logfc1"]])
d9 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_scde, logfc = logfc[["logfc1"]])
d10 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_zingeR, logfc = logfc[["logfc1"]])
d11 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_Agg, logfc = logfc[["logfc1"]])
d12 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_DEsingle, logfc = logfc[["logfc1"]])
d13 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_muscat, logfc = logfc[["logfc1"]])
d14 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_BaySeq, logfc = logfc[["logfc1"]])
d15 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_DEGnext, logfc = logfc[["logfc1"]])
d16 = which(dl$padj == 1 & abs(dl$logfc)>1)

set1 = d
set2 = unique(c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16))
library(ggVennDiagram)
library(ggplot2)
mydata<-list("WMW-A test"=set1,"Compared test"=set2)
alvvenn = ggVennDiagram(mydata,
                        # edge_lty = "dashed", 
                        edge_size = 1)+scale_fill_gradient(low = "#EDF8E9", high = "#74C476")
#+scale_fill_gradient(low="white",high = "#b9292b")
# +scale_fill_gradient(low = "#4169E1", high = "#D23D3D")


sid=id[setdiff(set1,set2),2]
sid=as.matrix(sid)

##########################################################################################


Res_WMWANBM = result_my[["Res"]][["WMWANBM"]][[2]]
Res_WMWANBM[which(Res_WMWANBM==0)]=1e-4

dl <- list(gene = id[,2], padj = Res_WMWANBM, logfc = logfc[["logfc2"]])
d = which(dl$padj < 0.01 & abs(dl$logfc)>1)

comre = readRDS("../../../Application/compresult_bro.rds")


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

path = '/Users/guoyin/Desktop/github/'
mat_bro<-np$load(paste0(path, "consexpression-master/results/bro/bro_deg.npy"), allow_pickle=TRUE) 
Res_consexpression = mat_bro[[1]][["Consexp"]][["0"]][["0"]]
Res_NOISeq = mat_bro[[1]][["NOISeq"]][["0"]][["0"]]

mat_BaySeq = read.csv(paste0(path, "consexpression-master/results/bro/bro_bayseq.csv"), sep = '\t')
idx = match(genes, rownames(mat_BaySeq))
mat_BaySeq_new = mat_BaySeq[idx,]
Res_BaySeq = mat_BaySeq_new[, 'FDR.DE']

dl <- list(gene = id[,2], padj = Res_DESeq, logfc = logfc[["logfc2"]])
d1 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_edgeR, logfc = logfc[["logfc2"]])
d2 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_Limma, logfc = logfc[["logfc2"]])
d3 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_wilx, logfc = logfc[["logfc2"]])
d4 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_ttest, logfc = logfc[["logfc2"]])
d5 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_ttestR, logfc = logfc[["logfc2"]])
d6 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_welch, logfc = logfc[["logfc2"]])
d7 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_MAST, logfc = logfc[["logfc2"]])
d8 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_monocle, logfc = logfc[["logfc2"]])
d9 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_scde, logfc = logfc[["logfc2"]])
d10 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_zingeR, logfc = logfc[["logfc2"]])
d11 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_Agg, logfc = logfc[["logfc2"]])
d12 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_DEsingle, logfc = logfc[["logfc2"]])
d13 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_muscat, logfc = logfc[["logfc2"]])
d14 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_BaySeq, logfc = logfc[["logfc1"]])
d15 = which(dl$padj < 0.01 & abs(dl$logfc)>1)
dl <- list(gene = id[,2], padj = Res_DEGnext, logfc = logfc[["logfc1"]])
d16 = which(dl$padj == 1 & abs(dl$logfc)>1)

set1 = d
set2 = unique(c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16))
library(ggVennDiagram)
library(ggplot2)
mydata<-list("WMW-A test"=set1,"Compared test"=set2)
brovenn = ggVennDiagram(mydata)+scale_fill_gradient(low = "#EDF8E9", high = "#74C476")

# brovenn = ggVennDiagram(mydata,
# edge_lty = "dashed",
# edge_size = 1)+scale_fill_gradient(low="white",high = "#b9292b")
