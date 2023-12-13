###GLMM Vertebrate invasions in Mountains of the World
PKGs <- c("raster","rlang", "rgbif", "dplyr","scrubr", "rgdal","maps", "maptools", "exactextractr","sp","spocc","plyr", "tidyverse", "pbapply", "sf", "ggplot2", "rgeos", "R.utils","data.table", "here", "rgdal",  "RColorBrewer",  "letsR", "rasterVis", "beepr", "circlize", "stringr", "moments", "diptest", "corrplot", "AER", "sjPlot", "sjmisc", "sjlabelled", "ggpubr", "blmeco","partR2", "magrittr", "glmm.hp", "GLMMadaptive", "gridExtra", "lme4", "furrr")
sapply(PKGs, require, character.only = TRUE)


setwd("C:/Users/Lenovo/Dropbox/PROJECTS_VIENNA/MOUNTAIN INVASIONS")

##Load data (Mountain level info, Alien Richness as response variable + predictors tested)
data<-read.csv("VADENUEVO/FOR SUBMISSION/NEEsubmission/Codes/DataPredictors.csv", sep=";")

##Test for multicolinearity among predictors
preds<-data[, c(15:24)] ##Choose numeric predictors
df_new <- preds[, order(colnames(preds))]
df_new<-round(df_new,3)
df_new<-na.omit(df_new)

##Pairwise correlations
p.mat <- cor(df_new)

##Plot
corrplot(p.mat,title = "Correlations among Predictors", method ="number", type = "lower", #
  tl.col = "black",  order = "hclust",hclust.method = "ward.D", number.cex = 1.4,tl.cex = 1.1, cl.cex=1.2, outline = T, mar=c(0,0,0,8), sig.level = 0.05)


###GLMMs
#1. Scale data
dat.scaled<-data[,-c(1:14, 25)] %>%
  mutate_if(is.numeric, scale) ###SCALE ALL DATA
dat.scaled %>%
  summarise_if(is.numeric, ~ near(mean(.x, na.rm=T), y = 0)) ###CHECK ALL MEANS=0
dat.scaled  %>%
  summarise_if(is.numeric, ~ near(sd(.x, na.rm=T), y = 1)) ######CHECK ALL SD=1

dat.scaled2<-data.frame(data[,c(1:14, 25)],dat.scaled)

##Convert to factors all non numeric preds
library(lmerTest)
dat.scaled2$Level_01<-as.factor(dat.scaled2$Level_01)
dat.scaled2$Level_02<-as.factor(dat.scaled2$Level_02)
dat.scaled2$Level_03<-as.factor(dat.scaled2$Level_03)
dat.scaled2$geometry<-as.factor(dat.scaled2$Geometry)

##Cross-taxonomic model
mod_neg.bi<-glmer.nb(AlienRich ~ offset(log(Area)) + BII  +  HumanPopDens + RoadDens + TimeCities + TimePorts  + Samp.completeness +  VccMg_tmn + VccMg_tmx  +  ElevRange + Rough + Geometry +  (1|Level_01/Level_02/Level_03) , data= dat.scaled2, control = glmerControl(optimizer = "bobyqa"))
summary(mod_neg.bi)

###Fishes
mod_neg.bi.fish<-glmer.nb(AlienFish ~ offset(log(Area)) + BII  +  HumanPopDens + RoadDens + TimeCities + TimePorts  + Samp.completeness +  VccMg_tmn + VccMg_tmx  +  ElevRange + Rough + Geometry +  (1|Level_01/Level_02/Level_03) , data= dat.scaled2[dat.scaled2$AlienFish>0,], control = glmerControl(optimizer = "bobyqa"))
summary(mod_neg.bi.fish)

###Amphibians
mod_neg.bi.amphs<-glmer.nb(AlienAmphibians ~ offset(log(Area)) + BII  +  HumanPopDens + RoadDens + TimeCities + TimePorts  + Samp.completeness +  VccMg_tmn + VccMg_tmx  +  ElevRange + Rough + Geometry +  (1|Level_01/Level_02/Level_03) , data= dat.scaled2[dat.scaled2$AlienAmphibians>0,], control = glmerControl(optimizer = "bobyqa"))
summary(mod_neg.bi.amphs)

###Reptiles
mod_neg.bi.repts<-glmer.nb(AlienReptiles ~ offset(log(Area)) + BII  +  HumanPopDens + RoadDens + TimeCities + TimePorts  + Samp.completeness +  VccMg_tmn + VccMg_tmx  +  ElevRange + Rough + Geometry +  (1|Level_01/Level_02/Level_03) , data= dat.scaled2[dat.scaled2$AlienReptiles>0,], control = glmerControl(optimizer = "bobyqa"))
summary(mod_neg.bi.repts)

###Birds
mod_neg.bi.birds<-glmer.nb(AlienBirds ~ offset(log(Area)) + BII  +  HumanPopDens + RoadDens + TimeCities + TimePorts  + Samp.completeness +  VccMg_tmn + VccMg_tmx  +  ElevRange + Rough + Geometry +  (1|Level_01/Level_02/Level_03) , data= dat.scaled2[dat.scaled2$AlienBirds>0,], control = glmerControl(optimizer = "bobyqa"))
summary(mod_neg.bi.birds)

###Mammals
mod_neg.bi.mamms<-glmer.nb(AlienMammals ~ offset(log(Area)) + BII  +  HumanPopDens + RoadDens + TimeCities + TimePorts  + Samp.completeness +  VccMg_tmn + VccMg_tmx  +  ElevRange + Rough + Geometry +  (1|Level_01/Level_02/Level_03) , data= dat.scaled2[dat.scaled2$AlienMammals>0,], control = glmerControl(optimizer = "bobyqa"))
summary(mod_neg.bi.mamms)

###Plot effects
models.nb<- list(mod_neg.bi,mod_neg.bi.fish,mod_neg.bi.amphs,mod_neg.bi.repts,mod_neg.bi.birds, mod_neg.bi.mamms)
effsize_neg.bi.MIX<-plot_models(models.nb,grid=FALSE, dot.size = 2.2, spacing = 0.7, p.shape = TRUE, line.size = .3, transform=NULL, std.est = "std2", title = "All models with NB", axis.title = "Standardized coefficients",colors = c("red3", "goldenrod2","darkorchid2", "green3", "dodgerblue3", "black"), p.threshold= 0.05)

effsize_neg.bi.MIX<-effsize_neg.bi.MIX + theme_classic() + geom_hline(yintercept = 0, color="grey20", linetype=5, linewidth=.5, alpha=0.75)
