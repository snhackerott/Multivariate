#### Script to Visualize Multivariate Data with PCA ####
## Author: Serena Hackerott, November 2024

## DataFrame should include multivariate response variables and meta data experimental design variables
## Set rownames of the DataFrame to be unique and meaningful sample IDs


#### Load Required Packages ####
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("corrplot")) install.packages("corrplot")
if (!require("vegan")) install.packages("vegan")
if (!require("FactoMineR")) install.packages("FactoMineR")
if (!require("factoextra")) install.packages("factoextra")
if (!require("ggpubr")) install.packages("ggpubr")

library(ggplot2) #Required for ggplots
library(dplyr) #Required for data organization
library(Hmisc) #Required for correlation matrix rcorr function
library(corrplot) #Required for correlation plot
library(vegan) #Required for PERMANOVA
library(FactoMineR) #Required for custom PCA
library(factoextra) #Required for custom PCA
library(ggpubr) #Required for stat_conf_ellipse


#### Prepare Input Data ####

##Check Correlation between Variables 
#Reduce dataset to variables of interest and limit pairs with strong correlations

##Spearman rank Correlations
#Replace "Var1:Var5" with range of response variables
Data.cor<-rcorr(as.matrix(DataFrame[,c(Var1:Var5)]), type="spearman")
Data.cor

##Set Diagonal to Zero
diag(Data.cor$P)<-0

##Plot Correlations
corrplot(Data.cor$r, type="upper", order="hclust", p.mat = Data.cor$P, sig.level = 0.01, insig = "blank")


##Standardize Response Metrics
#Accounts for response variables at different scales
#Replace "Var1" - "Var5" with names of response variables (can be more or less than 5)
DataFrame.st <- DataFrame %>% mutate_at(c("Var1", "Var2", "Var3", "Var4", "Var5"), ~(scale(.) %>% as.vector))


#### Run PCA ####
#Run Principal Components Analysis
#Replace "MetaVar1:MetaVar4" with range of meta data variables to be ignored
PCA <- prcomp(DataFrame.st[,-c(MetaVar1:MetaVar4)]) 

#Initial plot
fviz_pca_ind(PCA)

#Check Variance Explained by Components
summary(PCA)

##Extract % Variance PC 1
PC1<-sprintf("%1.2f",get_eigenvalue(PCA)$variance.percent[1])
PC1

##Extract % Variance PC 2
PC2_Th_HA<-sprintf("%1.2f",get_eigenvalue(PCA)$variance.percent[2])
PC2_Th_HA

##Extract Individual Sample Scores
PCA.scores <- as.data.frame(PCA$x[,c(1:2)])
PCA.scores$ID<-rownames(PCA.scores)

##Merge with Metadata
#Merges by ID column to add meta data variables to PCA scores
PCA.scores<-merge(PCA.scores, DataFrame.st)


#### Custom PCA Plot ####
fviz_pca_biplot(PCA_Th_HA, repel=TRUE, geom = "none", col.var="black", labelsize=sig.sz)+
  geom_point(aes(colour = PCA.scores$Group, shape=PCA.scores$Group), 
             size =3, alpha = 0.7)+
  theme_classic()+
  labs(x=paste0('PC 1 (',PC1_Th_HA,"%)"), y=paste0('PC 2 (',PC2_Th_HA,"%)"), title=NULL)

##Option to add confidence interval ellipses
#Add to ggplot object
#Customize Groups for colour, fill, and linetype as needed
#+stat_conf_ellipse(geom = "polygon", aes(colour=PCA.scores$Group, fill=PCA.scores$Group,linetype=PCA.scores$Group), alpha=0.6, linewidth=1)

##Options to customize legend items
#Within labs(), add Group Names for shape, colour, fill, and linetype as needed
#shape="Group", fill="Group", colour="Group", linetype="Group"

#To remove legend items
#Add to ggplot object
#Set legend title to none for items as needed
#+guides(linetype/colour/fill/shape="none")

##Option to customize colors
#Add to ggplot object
#Replace "ReplaceWithListofColorsinOrder" with desired colors
#+scale_fill_manual(values =c(ReplaceWithListofColorsinOrder))
#+scale_colour_manual(values =c(ReplaceWithListofColorsinOrder))

##Option for standard theme
#Add to ggplot object
#+theme(axis.title.x = element_text(size = 18), 
#      axis.title.y = element_text(size = 18), 
#      axis.text.x=element_text(size=14, colour="black"),
#      axis.text.y=element_text(size=14, colour="black"),
#      legend.text=element_text(size=12), 
#      legend.title=element_text(size=15),
#      legend.box.background = element_rect(color = "black"))