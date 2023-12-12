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
Res_WMWAN = result_my[["Res"]][["WMWAN"]][[2]]
Res_WMWALN = result_my[["Res"]][["WMWALN"]][[2]]
Res_WMWANB = result_my[["Res"]][["WMWANB"]][[2]]
Res_WMWANM = result_my[["Res"]][["WMWANM"]][[2]]
Res_WMWALNM = result_my[["Res"]][["WMWALNM"]][[2]]
Res_WMWANBM = result_my[["Res"]][["WMWANBM"]][[2]]
Res_WMWAN[which(Res_WMWAN==0)]=1e-4
Res_WMWALN[which(Res_WMWALN==0)]=1e-4
Res_WMWANB[which(Res_WMWANB==0)]=1e-4
Res_WMWALNM[which(Res_WMWALNM==0)]=1e-4
Res_WMWANM[which(Res_WMWANM==0)]=1e-4
Res_WMWANBM[which(Res_WMWANBM==0)]=1e-4

dl <- list(gene = id[,2], padj = Res_WMWANBM, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]
# annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

brodeg = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                            cluster_rows = F,cluster_cols = F,
                            # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                            border_color = "NA",
                            annotation_legend = FALSE,
                            show_rownames = F,show_colnames = F,
                            clustering_distance_cols = "correlation",
                            clustering_method = "complete") #mcquitty,complete,




################################################################################################################


#############################################################################################
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

# Res_DESeq[which(Res_DESeq==0)]=1e-4
# Res_edgeR[which(Res_edgeR==0)]=1e-4
# Res_Limma[which(Res_Limma==0)]=1e-4
# Res_ttest[which(Res_ttest==0)]=1e-4
# Res_ttestR[which(Res_ttestR==0)]=1e-4
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

path = '/Users/guoyin/Desktop/github/'
mat_bro<-np$load(paste0(path, "consexpression-master/results/bro/bro_deg.npy"), allow_pickle=TRUE) 
Res_consexpression = mat_bro[[1]][["Consexp"]][["0"]][["0"]]
Res_NOISeq = mat_bro[[1]][["NOISeq"]][["0"]][["0"]]

mat_BaySeq = read.csv(paste0(path, "consexpression-master/results/bro/bro_bayseq.csv"), sep = '\t')
idx = match(genes, rownames(mat_BaySeq))
mat_BaySeq_new = mat_BaySeq[idx,]
Res_BaySeq = mat_BaySeq_new[, 'FDR.DE']


dl <- list(gene = id[,2], padj = Res_DESeq, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)

p2_1 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,



dl <- list(gene = id[,2], padj = Res_edgeR, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p2_2 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_Limma, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p2_3 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_wilx, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p2_4 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          # annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_ttest, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p2_5 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_ttestR, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p2_6 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_welch, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]

test=data
p1=pheatmap(test,scale = "row")
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

p2_7 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,



dl <- list(gene = id[,2], padj = Res_MAST, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test <- data[apply(data, 1, function(x) sd(x)!=0),]
p1=pheatmap(test,scale = "row")#
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p2_8 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_monocle, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test <- data[apply(data, 1, function(x) sd(x)!=0),]
p1=pheatmap(test,scale = "row")#
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p2_9 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                          cluster_rows = F,cluster_cols = F,
                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                          border_color = "NA",
                          annotation_legend = FALSE,
                          show_rownames = F,show_colnames = F,
                          clustering_distance_cols = "correlation",
                          clustering_method = "complete") #mcquitty,complete,

#
dl <- list(gene = id[,2], padj = Res_scde, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
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
# p2_10 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
#                          cluster_rows = F,cluster_cols = F,
#                          # color = colorRampPalette(c("navy","white","firebrick3"))(100),
#                          border_color = "NA",
#                          annotation_legend = FALSE,
#                          show_rownames = F,show_colnames = F,
#                          clustering_distance_cols = "correlation",
#                          clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_zingeR, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
data = apply(data, 2, as.numeric)
rownames(data) = id[which(dl$padj < 0.01 & abs(dl$logfc)>1),1]


test <- data[apply(data, 1, function(x) sd(x)!=0),]
p1=pheatmap(test,scale = "row")#
gn=rownames(test)[p1$tree_row[["order"]]]
sn=colnames(test)[p1$tree_col[["order"]]]
new_test=test[gn,sn]

annotation_col = data.frame(group=c(rep("Alveolar_control",6),rep("Alveolar_spike",6)))
# annotation_col = data.frame(group=c(rep("Bronchial_control",6),rep("Bronchial_spike",6)))
rownames(annotation_col) = colnames(data)


p2_11 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,

dl <- list(gene = id[,2], padj = Res_Agg, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
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


p2_12 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_DEsingle, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
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


p2_13 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,


dl <- list(gene = id[,2], padj = Res_muscat, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
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


p2_14 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,




dl <- list(gene = id[,2], padj = Res_BaySeq, logfc = logfc[["logfc2"]])
data = exp[which(dl$padj < 0.01 & abs(dl$logfc)>1),13:24]
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


p2_15 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
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


p2_16 = pheatmap::pheatmap(new_test,scale = "row", annotation_col = annotation_col,
                           cluster_rows = F,cluster_cols = F,
                           # color = colorRampPalette(c("navy","white","firebrick3"))(100),
                           border_color = "NA",
                           annotation_legend = FALSE,
                           show_rownames = F,show_colnames = F,
                           clustering_distance_cols = "correlation",
                           clustering_method = "complete") #mcquitty,complete,



library(ggplotify)

pbrodeg = ggplotify::as.ggplot(brodeg)+labs(title = "WMW-A-NBM")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_1 = ggplotify::as.ggplot(p2_1)+labs(title = "DESeq")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_2 = ggplotify::as.ggplot(p2_2)+labs(title = "edgeR")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_3 = ggplotify::as.ggplot(p2_3)+labs(title = "Limma")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_4 = ggplotify::as.ggplot(p2_4)+labs(title = "WMW")+theme(plot.title = element_text(hjust = 0.3))+theme(text = element_text(family = "sans"))
pp2_5 = ggplotify::as.ggplot(p2_5)+labs(title = "ttest")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_6 = ggplotify::as.ggplot(p2_6)+labs(title = "ttestR")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_7 = ggplotify::as.ggplot(p2_7)+labs(title = "Welch")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_8 = ggplotify::as.ggplot(p2_8)+labs(title = "MAST")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_9 = ggplotify::as.ggplot(p2_9)+labs(title = "Monocle")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
# pp2_10 = ggplotify::as.ggplot(p2_10)+labs(title = "scde")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_11 = ggplotify::as.ggplot(p2_11)+labs(title = "ZINB-WaVE")+theme(plot.title = element_text(hjust = 0.3))+theme(text = element_text(family = "sans"))
pp2_12 = ggplotify::as.ggplot(p2_12)+labs(title = "aggregateBioVar")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_13 = ggplotify::as.ggplot(p2_13)+labs(title = "DEsingle")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_14 = ggplotify::as.ggplot(p2_14)+labs(title = "muscat")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_15 = ggplotify::as.ggplot(p1_15)+labs(title = "Bayseq")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))
pp2_16 = ggplotify::as.ggplot(p1_16)+labs(title = "DEGnext")+theme(plot.title = element_text(hjust = 0.5))+theme(text = element_text(family = "sans"))


# logfc = readRDS("logfc.rds")
# id = readRDS("genesymbol.rds")
# dataset = readRDS('GSE185657.rds') #count data, 1410*60
# exp = dataset[["expr"]]
# label = dataset$label
# 


