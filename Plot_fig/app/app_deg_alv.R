library (ggplot2)
library (reshape2)#????ת??
require(scales)#????????
library(ggtree)#????
library(aplot)#ƴͼ
library(pheatmap)




logfc = readRDS("../../../Application/logfc.rds")
id = readRDS("../../../Application/genesymbol.rds")
dataset = readRDS('../../../Application/GSE185657.rds') #count data, 1410*60
exp = dataset[["expr"]]
label = dataset$label


result_my = readRDS("../../../Application/WMWAresult_185657.rds")
Res_WMWAN = result_my[["Res"]][["WMWAN"]][[1]]
Res_WMWALN = result_my[["Res"]][["WMWALN"]][[1]]
Res_WMWANB = result_my[["Res"]][["WMWANB"]][[1]]
Res_WMWANM = result_my[["Res"]][["WMWANM"]][[1]]
Res_WMWALNM = result_my[["Res"]][["WMWALNM"]][[1]]
Res_WMWANBM = result_my[["Res"]][["WMWANBM"]][[1]]
Res_WMWAN[which(Res_WMWAN==0)]=1e-4
Res_WMWALN[which(Res_WMWALN==0)]=1e-4
Res_WMWANB[which(Res_WMWANB==0)]=1e-4
Res_WMWALNM[which(Res_WMWALNM==0)]=1e-4
Res_WMWANM[which(Res_WMWANM==0)]=1e-4
Res_WMWANBM[which(Res_WMWANBM==0)]=1e-4



dl <- list(gene = id[,2], padj = Res_WMWANBM, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]
annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

alvdeg = pheatmap(new_test,annotation_col = annotation_col,scale = "row",
                  annotation_legend = FALSE,
                  cluster_rows = F,cluster_cols = F,
                  # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                  # color = colorRampPalette(c("deepskyblue3","white","red"))(100),
                  border_color = "NA",
                  show_rownames = F,
                  show_colnames = F,
                  # clustering_distance_cols = "correlation",
                  # clustering_method = "complete"#mcquitty,complete,
) 



################################################################################################################

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
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_1 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,




dl <- list(gene = id[,2], padj = Res_edgeR, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)

p1_2 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,



dl <- list(gene = id[,2], padj = Res_Limma, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)
p1_3 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          annotation_legend = FALSE,
                          border_color = "NA",
                          cluster_rows = F,cluster_cols = F,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") 


dl <- list(gene = id[,2], padj = Res_wilx, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p1_4 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          # annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_ttest, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p1_5 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_ttestR, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p1_6 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,



dl <- list(gene = id[,2], padj = Res_welch, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p1_7 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,



dl <- list(gene = id[,2], padj = Res_MAST, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_8 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_monocle, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_9 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_scde, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
# rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]
# 
# 
# test=data
# p1=pheatmap(test,scale = "row")
# gn=rownames(test)[p1$tree_row[["order"]]]
# sn=colnames(test)[p1$tree_col[["order"]]]
# new_test=test[gn,sn]
# 
# annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# # annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
# rownames(annotation_col) = colnames(data)
# 
# 
# p1_10 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
#                         cluster_rows = F,cluster_cols = F,
#                         # color = colorRampPalette(c("navy","white","firebrick3"))(100),
#                         border_color = "NA",
#                         annotation_legend = FALSE,
#                         show_rownames = F,show_colnames = F,
#                         clustering_distance_cols = "correlation",
#                         clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_zingeR, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_11 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,

dl <- list(gene = id[,2], padj = Res_Agg, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_12 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_DEsingle, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_13 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_muscat, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_14 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_BaySeq, logfc = logfc[["logfc1"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),1:12]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_15 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,



dl <- list(gene = id[,2], padj = Res_DEGnext, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj ==1 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj == 1 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p1_16 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,



#10
#############################################################################################


library(ggplotify)
palvdeg = ggplotify::as.ggplot(alvdeg)+labs(title = "WMW-A-NBM")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_1 = ggplotify::as.ggplot(p1_1)+labs(title = "DESeq")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_2 = ggplotify::as.ggplot(p1_2)+labs(title = "edgeR")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_3 = ggplotify::as.ggplot(p1_3)+labs(title = "Limma")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_4 = ggplotify::as.ggplot(p1_4)+labs(title = "WMW")+theme(plot.title = element_text(hjust = 0.3))+theme(text = element_text(family = "sans"))
pp1_5 = ggplotify::as.ggplot(p1_5)+labs(title = "ttest")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_6 = ggplotify::as.ggplot(p1_6)+labs(title = "ttestR")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_7 = ggplotify::as.ggplot(p1_7)+labs(title = "Welch")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_8 = ggplotify::as.ggplot(p1_8)+labs(title = "MAST")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_9 = ggplotify::as.ggplot(p1_9)+labs(title = "Monocle")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
# pp1_10 = ggplotify::as.ggplot(p1_10)+labs(title = "scde")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_11 = ggplotify::as.ggplot(p1_11)+labs(title = "ZINB-WaVE")+theme(plot.title = element_text(hjust = 0.3))+theme(text = element_text(family = "sans"))
pp1_12 = ggplotify::as.ggplot(p1_12)+labs(title = "aggregateBioVar")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_13 = ggplotify::as.ggplot(p1_13)+labs(title = "DEsingle")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_14 = ggplotify::as.ggplot(p1_14)+labs(title = "muscat")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_15 = ggplotify::as.ggplot(p1_15)+labs(title = "Bayseq")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp1_16 = ggplotify::as.ggplot(p1_16)+labs(title = "DEGnext")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
