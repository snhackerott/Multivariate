#### Script to Evaluate Multivariate Data with PERMANOVA ####
## Author: Serena Hackerott, November 2024

## DataFrame should include multivariate response variables and meta data experimental design variables

#### Load Required Packages ####
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("corrplot")) install.packages("corrplot")
if (!require("vegan")) install.packages("vegan")
if (!require("dplyr")) install.packages("dplyr")
if (!require("devtools")) install.packages("devtools")
library(devtools) #Required to download 
if (!require("pairwiseAdonis")) devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
if (!require("MicEco")) devtools::install_github("Russel88/MicEco")

library(Hmisc) #Required for correlation matrix rcorr function
library(corrplot) #Required for correlation plot
library(vegan) #Required for PERMANOVA
library(dplyr) #Required for data organization
library(pairwiseAdonis) #Required for pairwise PERMANOVA
library(MicEco) #Required for Effect size of PERMANOVA


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


#### PERMANOVA Model ####

##Run PERMANOVA
#Replace "Var1:Var5" with range of response variables
#Replace "MetaVar1:MetaVar4" with meta data predictor variables
#Note: Use alternative distance methods when appropriate 
#(Examples: community composition data = "bray", categorical data = "gower")
PERM.mod<-adonis2(DataFrame.st[,c(Var1:Var5)]~MetaVar1 + MetaVar2 + MetaVar3 + MetaVar4, 
                  data=DataFrame.st, method="euclidean")
PERM.mod

##Option to include interactions when appropriate
#Add within model within adonis2() call
#+MetaVar1:MetaVar2

##Option to include strata when appropriate 
#Add within adonis2() call
#,strata=DataFrame.st$MetaVar4


#### Pairwise PERMANOVA ####

##Pairwise Comparison
#When a significant predictor has more than 2 levels
#Replace "Var1:Var5" with range of response variables
#Replace "MetaVar1" with significant predictor variable
#Note: Use alternative distance methods when appropriate 
PERM.mod.pair<-data.frame(pairwise.adonis(DataFrame.st[,c(Var1:Var5)], DataFrame.st$MetaVar1, sim.method = "euclidean"))
PERM.mod.pair


#### Check Dispersion ####

##Run PERMDISP
#For each significant predictor within PERM.mod, you must check if the 
#effect could also be due to differences in dispersion (i.e., variance) 
#rather than centroid location (i.e., mean differences)
#Replace "Var1:Var5" with range of response variables
#Replace "MetaVar1" with significant predictor variable
#Note: Use alternative distance methods when appropriate 
PERM.disp<-anova(betadisper(vegdist(DataFrame.st[,c(Var1:Var5)], "euclidean"), DataFrame.st$MetaVar1))
PERM.disp

#### Pairwise Dispersion #### 

##Pairwise Comparison
#When a significant predictor has more than 2 levels
PERM.disp.pair<-data.frame(TukeyHSD(PERM.disp)$group)
PERM.disp.pair


#### Effect Size ####
#Omega Squared can be calculated as a measure of effect size for 
#significant predictors of the multivariate response
adonis_OmegaSq(PERM.mod)


#### Save Results ####

##PERMANOVA and PERMDISP
#Remove last row of model results (Residuals)
PERM.res<-data.frame(adonis_OmegaSq(PERM.mod))

#Option to add PERMDISP for relevant variables
#Adjust the number of NA's if multiple PERMDISP p values are added
PERM.res$p_DISP<-c(PERM.disp$`Pr(>F)`[1], rep(NA, (length(PERM.res)-1)))

#Option to add Response and Predictor columns
#Replace "Multivariate Response" with more meaningful description of response data
#Replace each "MetaVar" with names of predictors in order of the model formula
PERM.res$Response<-"Multivariate Response"
PERM.res$Predictor<-c("MetaVar1", "MetaVar2", "MetaVar3", "MetaVar4")

##Pairwise
PERM.pair.res<-cbind(PERM.mod.pair, PERM.disp.pair)

#Clean column names
PERM.pair.res<-PERM.pair.res %>% dplyr::rename(Pair = pairs, 
              PERMANOVA = p.adjusted, TukeyHSD = "PERM.disp.pair$p.adj")

#Option to add Response and Predictor columns
#Replace "Multivariate Response" with more meaningful description of response data
PERM.pair.res$Response<-"Multivariate Response"


#### Suggested Figures ####
#Visualize multivariate responses with PCA, NMDS, DAPC, etc as most appropriate for the data