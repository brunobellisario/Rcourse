#Load libraries
library(easypackages)#allows loading multiple packages at once
libraries("readxl","tidyverse","tidyverse","funspace","vegan","ape","FactoMineR","factoextra","writexl","mice","VIM","phylobase")
#Load raw databases_____________________________________________________________
Data=as.data.frame(read_excel("/Users/bruno/Documents/Databases/Fish/Dataset_global/Seagrass_fish_Med.xlsx",sheet=2))#Dataset from Lattanzi et al. (in review)
Data$Genus_Species <- gsub(" ", "_", Data$Genus_Species)
Traits=as.data.frame(read_excel("/Users/bruno/Documents/Databases/Fish/Traits/joinedtraits.xlsx",sheet=1))
Traits$Species <- gsub(" ", "_", Traits$Species)
fish_phylo_actinopterygii=as.phylo(read.tree("/Users/bruno/Documents/Databases/Fish/Phylogeny/chronogram.tre"))
fish_phylo_chondrichthyes=as.phylo(read.tree("/Users/bruno/Documents/Databases/Fish/Phylogeny/Chond.610sp.10Cal.500TreeSet.tre")[[2]])
#_______________________________________________________________________________

#Separate categorical from numeric variables
Traits_cat=Traits[,1:18] %>% mutate(across(where(is.character), as.factor))
Traits_num=Traits[,19:33] %>% mutate(across(where(is.character), as.numeric)) 
rownames(Traits_cat)=Traits$Species
rownames(Traits_num)=Traits$Species
#Exploratory analysis for missing data
aggr_plot_Traits_num <- aggr(Traits_num, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(Traits_num), cex.axis=.55, gap=3, ylab=c("Histogram of missing data","Pattern"))
aggr_plot_Traits_cat <- aggr(Traits_cat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                             labels=names(Traits_cat), cex.axis=.4, gap=3, ylab=c("Histogram of missing data","Pattern"))
#Use phylogenetic information to impute missing quantitative traits
Traits_num_imputed <- impute(traits = Traits_num, phylo = fish_phylo_actinopterygii, addingSpecies = TRUE)$imputed

mixError(ximp=Traits_num_imputed, xmis=Traits_num, Traits_num)


#Use mice to impute missing cat data
init = mice(Traits_cat, maxit=0) 
meth = init$method
predM = init$predictorMatrix
predM[, c("Species","feeding_habit","feeding_type")]=0
meth[c("Species","feeding_habit","feeding_type")]=""
set.seed(103)
imputed = mice(Traits_cat, method=meth, predictorMatrix=predM, m=5)
Traits_cat_imputed <- complete(imputed)
#Accuracy_______________________________________________________________________
densityplot(imputed, ~feeding_behaviour)
#Merge and scale imputed traits
Imputed_Traits=merge(Traits_num_imputed,Traits_cat_imputed,by=0)
Imputed_Traits_scaled=Imputed_Traits[,2:ncol(Imputed_Traits)] %>% mutate(across(where(is.numeric), scale))

#Extract information from Lattanzi et al. about the life stages of fish
Life_stages=data.frame(Species=Data$Genus_Species,Life_stages=Data$Life_Stage)%>% filter(!Life_stages=="UNSPEC")%>% filter(!Life_stages=="Larvae") %>% filter(!Life_stages=="Both")
Life_stages=as.data.frame.matrix((table(Life_stages$Species,Life_stages$Life_stages))>0)*1
#Create categories
Life_stages$lf_cat=ifelse(Life_stages$Adult>Life_stages$Juvenile,"Adult",
                          ifelse(Life_stages$Juvenile>Life_stages$Adult,"Juvenile","Both") 
)
Life_stages=data.frame(Species=rownames(Life_stages),lf_cat=Life_stages$lf_cat)
Life_stages$lf_cat=as.factor(Life_stages$lf_cat)
#_______________________________________________________________________________

#Select species for which we have complete data about the life stage (Adult, exclusive; Juvenile, exclusive; Both)
species_w_lifestage=Life_stages$Species
#Subsetting Imputed_Traits_scaled to match with Life_stages
subset_traits_scaled=Imputed_Traits_scaled[(Imputed_Traits_scaled$Species %in% species_w_lifestage),]
subset_traits_scaled=left_join(subset_traits_scaled,Life_stages)
rownames(subset_traits_scaled)=subset_traits_scaled$Species
df2<-subset_traits_scaled[!(subset_traits_scaled$Species=="Thunnus_thynnus" | subset_traits_scaled$Species=="Clinitrachus_argentatus" | subset_traits_scaled$Species=="Lichia_amia"),]
write_xlsx(df2,"finaldataset.xlsx")
#_______________________________________________________________________________

