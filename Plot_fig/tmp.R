
Res_monocle = readRDS('Application/compresult_monocle_alv.rds')
Res$monocle = Res_monocle

Res = readRDS("Application/compresult_alv.rds")
comre = readRDS("Application/result_185657_1_unfdr2.rds")
Res[["Limma"]] = comre[["Limma"]]
saveRDS(Res,"Application/compresult_alv.rds")

Res = readRDS("Application/compresult_bro.rds")
comre = readRDS("Application/result_185657_2_unfdr2.rds")
Res[["Limma"]] = comre[["Limma"]]
saveRDS(Res,"Application/compresult_bro.rds")


comre2 = readRDS("Application/compresult_185657_1_unfdr.rds")
Res_zingeR = comre2[["zingeR"]]
Res_Agg = comre2[["Agg"]]
Res_DEsingle = comre2[["DEsingle"]][["pval"]]
Res_muscat = as.vector(comre2[["muscat"]])



Res_monocle = readRDS('Application/compresult_monocle_bro.rds')
Res$monocle = Res_monocle


comre = readRDS("Application/result_185657_1_unfdr2.rds")


comre2 = readRDS("Application/compresult_185657_2_unfdr.rds")
Res_zingeR = comre2[["zingeR"]]
Res_Agg = comre2[["Agg"]]
Res_DEsingle = comre2[["DEsingle"]][["pval"]]
Res_muscat = as.vector(comre2[["muscat"]])





Res = readRDS("compresult_alv.rds")
Res_scde = readRDS("Application/compresult_scde_alv.rds")
Res$scde = Res_scde
saveRDS(Res,"Application/compresult_alv.rds")



a = c(1,2)
b = c(7,8)
p = wilcox.test(a, b)$p.value


a = c(1:4,7)
b = c(6:9,3)
p = wilcox.test(a, b)$p.value


a = c(1:4)
b = c(3:6)
p = wilcox.test(a, b)$p.value



EBOut=EBTest(Data=m, Conditions=as.factor(rep(c(' + grup + '),each=' + str(self._replic) + ')), sizeFactors=Sizes, maxround=5)

function (object, q = 0.95, M = NULL) 
{
  if (class(object) != "Output") 
    stop("You must give the object returned by the noiseq function\n")
  x <- object@results[[1]]
  noiseqbio = "theta" %in% colnames(x)[1:4]
  if (noiseqbio) {
    y <- na.omit(x[c("theta", "prob")])
    colnames(y)[1] = "M"
  }
  else {
    y <- na.omit(x[c("M", "D", "prob")])
  }
  if (is.null(M)) {
    losdeg <- y[y[, "prob"] > q, ]
    print(paste(dim(losdeg)[1], "differentially expressed features"))
  }
  else if (M == "up") {
    estos <- y[y[, "M"] > 0, ]
    losdeg <- estos[estos[, "prob"] > q, ]
    print(paste(dim(losdeg)[1], "differentially expressed features (up in first condition)"))
  }
  else if (M == "down") {
    estos <- y[y[, "M"] < 0, ]
    losdeg <- estos[estos[, "prob"] > q, ]
    print(paste(dim(losdeg)[1], "differentially expressed features (down in first condition)"))
  }
  else {
    stop("ERROR! Value for parameter M is not valid. Please, choose among NULL, 'up' or 'down'")
  }
  losdeg = x[rownames(losdeg), ]
  losdeg[order(losdeg[, "prob"], decreasing = TRUE), ]
}
