PKGs <- c("viridis","raster","ecospat","rlang", "rgbif", "dplyr","scrubr", "rgdal","maps", "maptools", "exactextractr","sp","spocc","plyr", "tidyverse", "pbapply", "usdm", "geosphere", "pbapply", "sf", "ggplot2", "rgeos", "R.utils","data.table", "here", "rgdal", "ggmap", "RColorBrewer", "ggpubr", "biogeo", "letsR", "rasterVis", "waRhol", "spThin", "beepr", "gmbaR", "ggridges", "mgvc")

sapply(PKGs, require, character.only = TRUE)

###NULL MODELS TO TEST ALIEN VERTEBRATE FLOWS AMONG REALMS
setwd("your_path")

All.verts.nat.realms<-read.csv("~/Spp.nat.realms.csv", sep=";")

#Define list of realms
realms<-unique(All.verts.nat.realms$WWF_REALM2)
realms<-sort(realms[1:8])

full.pool<-All.verts.nat.realms[All.verts.nat.realms$WWF_REALM2%in%realms,]
realms.by.spp<-count(full.pool[,2])

spp.mult.realms<-realms.by.spp[realms.by.spp$freq>1, ]### species occurring in more than one realm
n.spp<-nrow(full.pool)-sum(spp.mult.realms$freq)+ nrow(spp.mult.realms)### Total pool 37973 spp

##LOAD TABLE WITH OBSERVED TOTAL FLOWS
flows<-read.csv("~/Table_Flows.csv")
flows.all<-data.frame(janitor::row_to_names(flows[,1:3], row_number = 1))

##CALCULATE EXPECTED FLOWS
exp.flows.f<-sapply(1:999, function(y) ###replicate the draws 1000 times
{
  draws<- lapply(1:8, function(x){  #do it for each recipient realm
    samp.size<-as.numeric(flows.all[flows.all==realms[x],3]) #define the observed number of species introduced to an specific realm
    resamp<-sample(full.pool$WWF_REALM2, samp.size, replace = TRUE) #resample the same number from the overall species pool
    from<-count(resamp) #count the number of spp originating on each realm
    from$to<-realms[x] #
    new.from.to.mat<-from[,c(1,3,2)]
    names(new.from.to.mat)<-c("From", "To", "N.Spp")
    return(new.from.to.mat)
  })
})

##Organize results expected flows
exp.flows<-do.call(rbind,exp.flows.f) ### create a table with all the expected flows from the draws
exp.flows$flow<-paste(substr(exp.flows[,1],1,3), "-",substr(exp.flows[,2],1,3)) ##add a column with specific flow
flow.id<-sort(unique(exp.flows$flow)) ##retrieve the names of the flows (total 64 combinations)

###REFERENCE TO COMPARE THE RESULTS
obs.flows.all<-read.csv("~/AllFromTo.csv")
obs.flows.all$flow<-paste(obs.flows.all[,2],"-",obs.flows.all[,3])
names(obs.flows.all)
###ADD 0's to non detected flows.
NO.flow<-data.frame(setdiff(flow.id,obs.flows.all$flow))
NO.flow$matrixAll.freq<-0
names(NO.flow)<-c("flow", "matrixAll.freq")

obs.flows.all2<-rbind(obs.flows.all[ ,c(5,4)],NO.flow)

##Significance
signif<-lapply(1:length(flow.id),function(x){
  exp.vals<-exp.flows[exp.flows$flow==flow.id[x],3] ##get all 999 values for each flow
  p<-length(exp.vals[exp.vals>obs.flows.all2[obs.flows.all2$flow==flow.id[x],2]])/999 ###
  p2<-if (p > 0.5) {round(1-p,4)} else {round(p,4)}
  col<-if (p2< 0.05) {"brown1"} else {"gray25"}
  p.tab<-data.frame(cbind(flow.id[x]), p2, col)
  names(p.tab)<- c("Flow", "P-val", "color")
  return(p.tab)
})

p.table<-do.call(rbind,signif)

plots<-pblapply(1:length(flow.id), function(x)
{
  N.species<-exp.flows[exp.flows$flow==flow.id[x], 3] #get the 1000 value obtained for each specific flow
  obs<-obs.flows.all2[obs.flows.all2$flow==flow.id[x],2]
  hist_obs<-ggplot() +
    geom_histogram(aes(N.species), fill="blue",bins = 8, alpha=0.35) +
    geom_vline(aes(xintercept = obs), color = p.table[p.table$Flow==flow.id[x],3], linewidth = 1, linetype = "solid") + 
    theme_test() + ggtitle(paste(flow.id[x]))
})

ggpubr::ggarrange(plotlist = plots, ncol = 8,nrow = 8)

