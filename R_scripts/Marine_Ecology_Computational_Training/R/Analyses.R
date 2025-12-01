#Load libraries
library(easypackages)#allows loading multiple packages at once
libraries("readxl","tidyverse","tidyverse","funspace","vegan","ape","FactoMineR","factoextra","writexl","mice","VIM","gridExtra")
Traits_data=as.data.frame(read_excel("/Users/bruno/Documents/Unitus/DEB/Esercitazione R/finaldataset.xlsx"))
Data=as.data.frame(read_excel("/Users/bruno/Documents/Databases/Fish/Dataset_global/Seagrass_fish_Med.xlsx",sheet=2))#Dataset from Lattanzi et al. (in review)
#Check for those species recorded only once in only one paper
uncommon=data.frame(frequency=rowSums(table(Data$Genus_Species,Data$Paper_List))) %>%  filter(frequency == 1)
uncommon=rownames(uncommon)
#Traits selection
sel_traits=c("trophic_level","max_length","k","generation_time","feeding_behaviour","lf_cat")
selected_traits=Traits_data[,(colnames(Traits_data) %in% sel_traits)]
rownames(selected_traits)=Traits_data$Species
selected_traits$p_Adult=ifelse(selected_traits$lf_cat == "Adult",1,0)
selected_traits$p_Juvenile=ifelse(selected_traits$lf_cat == "Juvenile" | selected_traits$lf_cat == "Both",1,0)
selected_traits <- selected_traits[!(row.names(selected_traits) %in% uncommon),]
famd_traits=selected_traits[,!names(selected_traits) %in% 
                  c("lf_cat", "p_Adult","p_Juvenile")]

#Functional space analysis
famd=FAMD(famd_traits,graph = F)
fviz_famd_var(famd,"quanti.var")
fviz_famd_ind(famd)
famd_coordinates=famd$ind$coord[,1:2]
P_oceanica_fspace=funspace(x = famd_coordinates, threshold = 0.99)
summary(P_oceanica_fspace)
P_oceanica_fspace_null=funspaceNull(P_oceanica_fspace,alter="two-sided",null.distribution = "uniform",n=1)
summary(P_oceanica_fspace_null)

#Plot functional space
par(pty="s")
plot(P_oceanica_fspace,quant.plot=TRUE,pnt=T,
     pnt.cex = .75,quant=c(0.50,0.99),axis.title.x = "",axis.title.y = "",
     globalContour.lwd=.5,arrows=TRUE,quant.lwd = 1.5,pnt.pch =c(15,17,19)[as.numeric(as.factor(famd_traits$feeding_behaviour))],
    pnt.col="black"
)

FunGAM_Adults=funspaceGAM(selected_traits$p_Adult,P_oceanica_fspace,family="binomial")
FunGAM_Juvenile=funspaceGAM(selected_traits$p_Juvenile,P_oceanica_fspace,family="quasibinomial")

summary(FunGAM_Adults)
summary(FunGAM_Juvenile)


par(pty="s")
plot(FunGAM_Adults,quant.plot=TRUE,quant=c(0.50,0.75,0.99),pnt=T,pnt.col="black",colors=c("red","green"),pnt.cex = .75,
     pnt.pch =c(15,17,19)[as.numeric(as.factor(famd_traits$feeding_behaviour))],
     quant.lwd=1,quant.col="black",quant.lty=2)
plot(FunGAM_Juvenile,quant.plot=TRUE,quant=c(0.50,0.75,0.99),pnt=T,pnt.col="black",colors=c("red","green"),pnt.cex = .75,
     pnt.pch =c(15,17,19)[as.numeric(as.factor(famd_traits$feeding_behaviour))],
     quant.lwd=1,quant.col="black",quant.lty=2)

#Prediction surfaces
Adults_predicted=FunGAM_Adults$global$images$predicted
Juveniles_predicted=FunGAM_Juvenile$global$images$predicted
Diff=(Adults_predicted-Juveniles_predicted)
points_coord=as.matrix(P_oceanica_fspace$parameters$pcs)

par(pty="s")
image(Diff)
contour(Diff,add=T)
points(points_coord)



