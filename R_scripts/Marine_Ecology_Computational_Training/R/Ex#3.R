#Load package utils
library(vegan)
library(bipartite)
library(dplyr)
library(PerformanceAnalytics)
library(BAT)
library(stringdist)
#Load datasets
dat=read.csv("taxa.csv",sep=";",dec=".")
Med=dat[,3:30]#Extract community data [r,c]
env.par=read.csv("envdata.csv",sep=";",dec=",")#Environmental data
traits=dat[,31:32]#Traits data
#Retrive taxonomic information from WORMS----------------------------------------------#
library(taxize)
taxa=paste(dat$Genus,dat$Species)
taxa_class=classification(taxa,db="worms",return_id=F)
taxa_frame=as.data.frame(cbind(taxa_class))
#Subset for tree reconstruction
#subset_taxa_frame=data.frame(taxa_frame$species,taxa_frame$genus,taxa_frame$family,
#taxa_frame$superfamily,taxa_frame$infraorder,taxa_frame$suborder)
subset_taxa_frame=data.frame(taxa_frame[,9:14],species=dat$Species)
rownames(subset_taxa_frame)=rownames(Med)
#Set taxonomic distance matrix as surrogate for phylogeny
tax.dist=taxa2dist(subset_taxa_frame,varstep=T)
#End of phylogenetic distance estimation-----------------------------------------------#
heatmap(as.matrix(tax.dist))
#Functional distance estimation--------------------------------------------------------#
library(FD)
fun.dist=gowdis(traits)
#End of functional distance estimation-------------------------------------------------#
heatmap(as.matrix(fun.dist))

funct.dendrogram=as.phylo(hclust(fun.dist))
phylo.dendrogram=as.phylo(hclust(tax.dist))
#phylosignal(dat$BS,phylo.dendrogram)
#dat2=data.frame(rownames(dat),dat)
#color.plot.phylo(phylo.dendrogram,dat2,"BS","rownames.dat.")
#color.plot.phylo(phylo.dendrogram,dat2,"Living.habit","rownames.dat.")

#Environmental, geographic and biogeographic distance estimation-----------------------#
library(FactoMineR)
library(factoextra)
env.PCA=PCA(data.frame(decostand(env.par[,3:4],"standardize"),env.par$PPOMed), graph=F)
scores.PCA=env.PCA$ind$coord[,1:2]
env.dist=as.matrix(dist(scores.PCA))#Environmental distance (sensu lato)
geo.dist=dist(data.frame(env.par$Long,env.par$Lat))
bioregions=read.csv("bioreg.csv",sep=";")
#End-----------------------------------------------------------------------------------#

#Taxonomic, functional and phylogenetic beta diversity---------------------------------#
library(BAT)
TDb=beta(t(Med),abund=FALSE)
FDb=beta(t(Med),hclust(fun.dist),abund = FALSE)
PDb=beta(t(Med),hclust(tax.dist),abund = FALSE)
TDb.multi=beta.multi(t(Med),abund = FALSE)
FDb.multi=beta.multi(t(Med),hclust(fun.dist),abund = FALSE)
PDb.multi=beta.multi(t(Med),hclust(tax.dist),abund = FALSE)

#Null models--------------------------------------------------------------------------#
library(picante)
library(ape)
null.traits=function(i) {
  null.traits=tipShuffle(as.phylo(hclust(fun.dist)))
  FDb.null.traits=beta.multi(t(Med),null.traits,abund = FALSE)[,1]
}
null.phylo=function(i) {
  null.phylo=tipShuffle(as.phylo(hclust(tax.dist)))
  FDb.null.phylo=beta.multi(t(Med),null.phylo,abund = FALSE)[,1]
}
set.seed(1)#for reproducible example; you may want to comment out this line 
null.FD <- t(sapply(1:1000,null.traits))
null.PD <- t(sapply(1:1000,null.phylo))
z.FD.total=(FDb.multi[1,1]-(null.FD[,1]))/sd(null.FD[,1])
z.FD.repl=(FDb.multi[2,1]-(null.FD[,2]))/sd(null.FD[,2])
z.FD.rich=(FDb.multi[3,1]-(null.FD[,3]))/sd(null.FD[,3])
z.PD.total=(PDb.multi[1,1]-(null.PD[,1]))/sd(null.PD[,1])
z.PD.repl=(PDb.multi[2,1]-(null.PD[,2]))/sd(null.PD[,2])
z.PD.rich=(PDb.multi[3,1]-(null.PD[,3]))/sd(null.PD[,3])
#P-values estimation------------------------------------------------------------------#
praw.fun.tot <- sum(null.FD[,1]>FDb.multi[1,1]) / length(null.FD[,1])
ifelse(praw.fun.tot > 0.5, 1-praw.fun.tot, praw.fun.tot)    # P-value
praw.fun.repl <- sum(null.FD[,2]>FDb.multi[2,1]) / length(null.FD[,2])
ifelse(praw.fun.repl > 0.5, 1-praw.fun.repl, praw.fun.repl)    # P-value
praw.fun.rich <- sum(null.FD[,3]>FDb.multi[3,1]) / length(null.FD[,3])
ifelse(praw.fun.rich > 0.5, 1-praw.fun.rich, praw.fun.rich)    # P-value
praw.phylo.tot <- sum(null.PD[,1]>PDb.multi[1,1]) / length(null.PD[,1])
ifelse(praw.phylo.tot > 0.5, 1-praw.phylo.tot, praw.phylo.tot)    # P-value
praw.phylo.repl <- sum(null.PD[,2]>PDb.multi[2,1]) / length(null.PD[,2])
ifelse(praw.phylo.repl > 0.5, 1-praw.phylo.repl, praw.phylo.repl)    # P-value
praw.phylo.rich <- sum(null.PD[,3]>PDb.multi[3,1]) / length(null.PD[,3])
ifelse(praw.phylo.rich > 0.5, 1-praw.phylo.rich, praw.phylo.rich)    # P-value
#Plot options-------------------------------------------------------------------------#
pdf("pippo.pdf")
par(pty="s",mfrow=c(2,3),cex=1.1)
hist(null.FD[,1],main=NULL,xlab="Total")
abline(v=FDb.multi[1,1],lty=2,lwd=2)
hist(null.FD[,2],main=NULL,xlab="Replacement")
abline(v=FDb.multi[2,1],lty=2,lwd=2)
hist(null.FD[,3],main=NULL,xlab="Richness")
abline(v=FDb.multi[3,1],lty=2,lwd=2)
hist(null.PD[,1],main=NULL,xlab="Total")
abline(v=PDb.multi[1,1],lty=2,lwd=2)
hist(null.PD[,2],main=NULL,xlab="Replacement")
abline(v=PDb.multi[2,1],lty=2,lwd=2)
hist(null.PD[,3],main=NULL,xlab="Richness")
abline(v=PDb.multi[3,1],lty=2,lwd=2)
dev.off()
#Compare TDb PDb and FDb for each component -------------------------------------------#
TDb_FDb_total<-vegan::mantel(TDb$Btotal,FDb$Btotal)
TDb_PDb_total<-vegan::mantel(TDb$Btotal,PDb$Btotal)
TDb_FDb_repl<-vegan::mantel(TDb$Brepl,FDb$Brepl)
TDb_PDb_repl<-vegan::mantel(TDb$Brepl,PDb$Brepl)
TDb_FDb_rich<-vegan::mantel(TDb$Brich,FDb$Brich)
TDb_PDb_rich<-vegan::mantel(TDb$Brich,PDb$Brich)
#Compare FDb and PDb anc control for TDb-----------------------------------------------#
FDb_PDb_total<-vegan::mantel(FDb$Btotal,PDb$Btotal)
FDb_PDb_TDb_total<-vegan::mantel.partial(FDb$Btotal,PDb$Btotal,TDb$Btotal)
FDb_PDb_repl<-vegan::mantel(FDb$Brepl,PDb$Brepl)
FDb_PDb_TDb_repl<-vegan::mantel.partial(FDb$Brepl,PDb$Brepl,TDb$Brepl)
FDb_PDb_rich<-vegan::mantel(FDb$Brich,PDb$Brich)
FDb_PDb_TDb_rich<-vegan::mantel.partial(FDb$Brich,PDb$Brich,TDb$Brich)

#Function to perform the (partial) Mantel test relating the beta diversity components--#
#with the environmental, geographic and biogeographic distance-------------------------#
mantel.function=function(beta,env,geo,bioreg) {
                Beta.total.env=vegan::mantel(beta$Btotal,env)
                Beta.repl.env=vegan::mantel(beta$Brepl,env)
                Beta.rich.env=vegan::mantel(beta$Brich,env)
                Beta.total.geo=vegan::mantel(beta$Btotal,geo)
                Beta.repl.geo=vegan::mantel(beta$Brepl,geo)
                Beta.rich.geo=vegan::mantel(beta$Brich,geo)
                Beta.total.env.geo=vegan::mantel.partial(beta$Btotal,env,geo)
                Beta.repl.env.geo=vegan::mantel.partial(beta$Brepl,env,geo)
                Beta.rich.env.geo=vegan::mantel.partial(beta$Brich,env,geo)
                
                Beta.total.geo.env=vegan::mantel.partial(beta$Btotal,geo,env)
                Beta.repl.geo.env=vegan::mantel.partial(beta$Brepl,geo,env)
                Beta.rich.geo.env=vegan::mantel.partial(beta$Brich,geo,env)
                
                Beta.total.env.bioreg=vegan::mantel.partial(beta$Btotal,env,bioreg)
                Beta.repl.env.bioreg=vegan::mantel.partial(beta$Brepl,env,bioreg)
                Beta.rich.env.bioreg=vegan::mantel.partial(beta$Brich,env,bioreg)
                Beta.total.geo.bioreg=vegan::mantel.partial(beta$Btotal,geo,bioreg)
                Beta.repl.geo.bioreg=vegan::mantel.partial(beta$Brepl,geo,bioreg)
                Beta.rich.geo.bioreg=vegan::mantel.partial(beta$Brich,geo,bioreg)
Mantel.stats=rbind(Beta.total.env$statistic,Beta.repl.env$statistic,
Beta.rich.env$statistic, Beta.total.geo$statistic,Beta.repl.geo$statistic ,Beta.rich.geo$statistic ,Beta.total.env.geo$statistic, Beta.repl.env.geo$statistic,
Beta.rich.env.geo$statistic,Beta.total.env.bioreg$statistic,Beta.repl.env.bioreg$statistic,Beta.rich.env.bioreg$statistic,Beta.total.geo.bioreg$statistic,
Beta.repl.geo.bioreg$statistic,Beta.rich.geo.bioreg$statistic,Beta.total.geo.env$statistic,Beta.repl.geo.env$statistic,Beta.rich.geo.env$statistic)

Mantel.p=rbind(Beta.total.env$signif,Beta.repl.env$signif,
Beta.rich.env$signif, Beta.total.geo$signif,Beta.repl.geo$signif ,Beta.rich.geo$signif ,Beta.total.env.geo$signif, Beta.repl.env.geo$signif,
Beta.rich.env.geo$signif,Beta.total.env.bioreg$signif,Beta.repl.env.bioreg$signif,Beta.rich.env.bioreg$signif,Beta.total.geo.bioreg$signif,
Beta.repl.geo.bioreg$signif,Beta.rich.geo.bioreg$signif,Beta.total.geo.env$signif,Beta.repl.geo.env$signif,Beta.rich.geo.env$signif)
Data=cbind(Mantel.stats,Mantel.p)
colnames(Data)=c("Mantel statistic","p-value")
rownames(Data)=c("Total.ENV","Replacement.ENV","Ricnhess.ENV",
                 "Total.GEO","Replacement.GEO","Ricnhess.GEO",
                 "Total.ENV|GEO","Replacement.ENV|GEO","Ricnhess.ENV|GEO",
                 "Total.ENV|BIO","Replacement.ENV|BIO","Ricnhess.ENV|BIO","Total.GEO|BIO","Replacement.GEO|BIO","Ricnhess.GEO|BIO",
                 "Total.GEO|ENV","Replacement.GEO|ENV","Ricnhess.GEO|ENV")
Data
}
TDb.mantel=mantel.function(TDb,as.dist(env.dist),geo.dist,as.dist(bioregions))
FDb.mantel=mantel.function(FDb,as.dist(env.dist),geo.dist,as.dist(bioregions))
PDb.mantel=mantel.function(PDb,as.dist(env.dist),geo.dist,as.dist(bioregions))

#Ternary plot beta diversity components------------------------------------------------#
#(For a better visualization, the figure has been passed on Inkscape!)
library(Ternary)
library(ggplot2)
library(ggtern)
taxabeta<-data.frame(1-rowMeans(as.matrix(TDb$Btotal)),
                     rowMeans(as.matrix(TDb$Brepl)),rowMeans(as.matrix(TDb$Brich)))
colnames(taxabeta)=c("Total","Replacement", "Richness")
funcbeta<-data.frame(1-rowMeans(as.matrix(FDb$Btotal)),
                     rowMeans(as.matrix(FDb$Brepl)),rowMeans(as.matrix(FDb$Brich)))
colnames(funcbeta)=c("Total","Replacement", "Richness")
phylobeta<-data.frame(1-rowMeans(as.matrix(PDb$Btotal)),
                      rowMeans(as.matrix(PDb$Brepl)),rowMeans(as.matrix(PDb$Brich)))
colnames(phylobeta)=c("Total","Replacement", "Richness")
colorcode=c("blue","blue","blue","blue","blue","blue","blue","blue","blue"
            ,"green","green","green","red","red","red","yellow","yellow","yellow",
            "yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow"
            ,"yellow","yellow")
pdf("betaplot.pdf")
A=ggtern(data=taxabeta,aes(Replacement,Total,Richness)) + 
  geom_point() +
  theme_showarrows() +
  ggtitle(expression("TD"*beta)) +
  xlab("Replacement") + 
  ylab("Similarity") +
  zlab("Richness") +
  guides(color = "none", fill = "none", alpha = "none")+
  geom_point(size=2,color=colorcode,pch=19) +theme_hidegrid()
B=ggtern(data=funcbeta,aes(Replacement,Total,Richness)) + 
 geom_point() +
  theme_showarrows() +
  ggtitle(expression("FD"*beta)) +
  xlab("Replacement") + 
  ylab("Similarity") +
  zlab("Richness") +
  guides(color = "none", fill = "none", alpha = "none")+
  geom_point(size=2,color=colorcode,pch=19) +theme_hidegrid()
C=ggtern(data=phylobeta,aes(Replacement,Total,Richness)) + 
  geom_point() +
  theme_showarrows() +
  ggtitle(expression("PD"*beta)) +
  xlab("Replacement") + 
  ylab("Similarity") +
  zlab("Richness") +
  guides(color = "none", fill = "none", alpha = "none")+
  geom_point(size=2,color=colorcode,pch=19) +theme_hidegrid()
grid.arrange(A,B,C,ncol=3)
dev.off()


