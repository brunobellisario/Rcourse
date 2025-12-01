# Loading Required Libraries --------------------------------------------------------------#
lib <- .libPaths()[1]
required.packages <- c("vegan","bipartite","dplyr","PerformanceAnalytics","BAT","stringdist","taxize","FD",
                       "FactoMineR","factoextra","picante","ape","Ternary","ggplot2","ggtern","readxl")
i1 <- !(required.packages %in% row.names(installed.packages()))
if(any(i1)) {
  install.packages(required.packages[i1], dependencies = TRUE, lib = lib) 
}
lapply(required.packages, require, character.only = TRUE)

install.packages("readxl")

# Load data -------------------------------------------------------------------------#
Species_name=as.data.frame(read_excel("Supplementary_Materials_SM1.xlsx", sheet = "taxa_traits"))[,1]
Genus=as.data.frame(read_excel("Supplementary_Materials_SM1.xlsx", sheet = "taxa_traits"))[,2]
Species=as.data.frame(read_excel("Supplementary_Materials_SM1.xlsx", sheet = "taxa_traits"))[,3]
Med <- as.data.frame(read_excel("Supplementary_Materials_SM1.xlsx", sheet = "taxa_traits"))[,4:31]
rownames(Med)=Species_name
traits=as.data.frame(read_excel("Supplementary_Materials_SM1.xlsx", sheet = "taxa_traits"))[,32:33]
rownames(traits)=Species_name
env.par <- as.data.frame(read_excel("Supplementary_Materials_SM1.xlsx", sheet = "envdata"))[,3:7]
rownames(env.par)=colnames(Med)
bioregions <- as.data.frame(read_excel("Supplementary_Materials_SM1.xlsx", sheet = "bioregions"))[,2:29]
rownames(bioregions)=colnames(bioregions)

# Diversity analysis
library(vegan)
Numero_specie_totale=colSums(Med)
Numero_specie_medio=colMeans(Med)
library(bipartite)
Med_plus=as.matrix(Med)*rowSums(as.matrix(Med))/2
Med_plus_simul=round(nullmodel(Med_plus,1,method=4)[[1]])
rownames(Med_plus_simul)=Species_name
colnames(Med_plus_simul)=colnames(Med)

H <- diversity(t(Med_plus_simul))
simp <- diversity(t(Med_plus_simul),"simpson")
invsimp <- diversity(t(Med_plus_simul), "inv")
## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefaction:
unbias.simp <- rarefy(t(Med_plus_simul), 2) - 1
## Fisher's alpha
alpha <- fisher.alpha(t(Med_plus_simul))
## Plot all
pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
## Species richness (S) and Pielou's evenness (J):
S <- specnumber(t(Med_plus_simul)) ## rowSums(x > 0) does the same...
J <- H/log(S)

# We test the hypothesis of a correlation between diversity parameters and environmental parameters
# Univariate analysis
cor.test(S,env.par$`PPOMed - Probability of occurrence of Posidonia oceanica`)
cor.test(S,env.par$`SST - Sea Surface Temperature`)
cor.test(S,env.par$`Psu - Sea Salinity`)
# Single linear regression
modello1 = lm(log(S) ~ env.par$`PPOMed - Probability of occurrence of Posidonia oceanica`, x=TRUE, y=TRUE)
modello1
summary(modello1)
par(pty="s")
plot(log(S)  ~ env.par$`PPOMed - Probability of occurrence of Posidonia oceanica`,pch=19,las=1)
abline(modello1)

modello2 = lm(log(S)  ~ env.par$`Psu - Sea Salinity`, x=TRUE, y=TRUE)
modello2
summary(modello2)
par(pty="s")
plot(log(S)  ~ env.par$`Psu - Sea Salinity`,pch=19,las=1)
abline(modello2)

modello3 = lm(log(S)  ~ env.par$Longitude, x=TRUE, y=TRUE)
modello3
summary(modello3)
par(pty="s")
plot(log(S)  ~ env.par$Longitude,pch=19,las=1)
abline(modello3)

# We test the hypothesis of a correlation between diversity parameters and environmental parameters
# Multiple regression
modello4 = lm(S ~ env.par$`PPOMed - Probability of occurrence of Posidonia oceanica`*env.par$Latitude, x=TRUE, y=TRUE)
modello4
summary(modello4)
par(pty="s")
plot(log(S) ~ env.par$`PPOMed - Probability of occurrence of Posidonia oceanica`*env.par$Latitude,pch=19,las=1)

modello5 = lm(S ~ env.par$`Psu - Sea Salinity`*env.par$Longitude, x=TRUE, y=TRUE)
modello5
summary(modello5)
par(pty="s")
plot(modello5)

# We test the hypothesis of a correlation between diversity parameters and environmental parameters
# Multivariate analysis
PCA2=prcomp(env.par,scale=T,center=TRUE)
summary(PCA2)
plot(PCA2)
biplot(PCA2)
scoresPCA2=scores(PCA2)

DATAMax=data.frame(H,simp,J,S,env.par)
PCA1=prcomp(DATAMax,scale=T,center=T)
plot(PCA1)
biplot(PCA1)

modello6 = lm(log(S) ~ scoresPCA2[,2])
modello6
summary(modello6)
par(pty="s")
plot(log(S) ~ scoresPCA2[,2],pch=19,las=1)
abline(modello6)
text(log(S) ~ scoresPCA2[,2], labels=rownames(scoresPCA2), col="red")

# Distance matrices
Dist1=vegdist(t(Med),"jaccard")
Dist2=vegdist(env.par[,1:3],"euclidean")
Dist3=vegdist(env.par[,4:5],"euclidean")
heatmap(as.matrix(Dist1))
heatmap(as.matrix(Dist2))
heatmap(as.matrix(Dist3))

mantel(Dist1,Dist2)
mantel(Dist1,Dist3)
mantel.partial(Dist1,Dist2,Dist3)
mantel.partial(Dist1,Dist3,Dist2)

Clust1=hclust(Dist1)
Clust2=hclust(Dist2)
Clust3=hclust(Dist3)

plot(Clust1)
plot(Clust2)
plot(Clust3)

library(dendextend)
dend1=as.dendrogram(Clust1)
dend2=as.dendrogram(Clust2)
dend3=as.dendrogram(Clust3)
dl1 <- dendlist(dend1, dend2)
tanglegram(dl1, sort = TRUE, common_subtrees_color_lines = T, highlight_distinct_edges  = F, highlight_branches_lwd = F)
dl2 <- dendlist(dend1, dend3)
tanglegram(dl2, sort = TRUE, common_subtrees_color_lines = T, highlight_distinct_edges  = F, highlight_branches_lwd = F)
dl3 <- dendlist(dend2, dend3)
tanglegram(dl3, sort = TRUE, common_subtrees_color_lines = T, highlight_distinct_edges  = F, highlight_branches_lwd = F)
