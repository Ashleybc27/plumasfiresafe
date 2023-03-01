#setwd("~/Dropbox/Plumas_GGRF_FireSafe_Working/RCarbonCalcs")

#treedata1 <- read.csv("all_trees.csv")
#treedata1

abovegroundbiomass <- function(treedata="all_trees.csv"){  
  treetable <- read.csv(treedata)
  outtable <- c() 
  for (i in seq(treetable$Species)) {
    if (treetable$Species[[i]] == "SP") {
      biomass <-  exp(-3.984 + 2.667*(log(treetable$DBH_cm[[i]])))
        + exp(-5.295 + 2.619*(log(treetable$DBH_cm[[i]])))
        + exp(-7.637 + 3.365*(log(treetable$DBH_cm[[i]])))
        + exp(-5.413 + 2.127*(log(treetable$DBH_cm[[i]])))
        + exp(-4.023 + 2.032*(log(treetable$DBH_cm[[i]])))
      outtable <- c(outtable, biomass)
    }
    if (treetable$Species[[i]] == "JP") {
      biomass <- (1.0469 * exp(-4.1279 + log(treetable$DBH_cm[[i]]) * 2.7039))
        + (1.0304 * exp(-4.2291 + log(treetable$DBH_cm[[i]]) * 2.2691))
        + (1.0425 * exp(-6.0278 + log(treetable$DBH_cm[[i]]) * 2.8655))
        + (1.1322 * exp(-5.3589 + log(treetable$DBH_cm[[i]]) * 2.2500))
        + (1.0672 * exp(-4.1317 + log(treetable$DBH_cm[[i]]) * 2.0159))
      outtable <- c(outtable,biomass)
    }
    if (treetable$Species[[i]] == "IC") {
      biomass <- exp(-2.656 + 2.53*(log(treetable$DBH_cm[[i]])))
        + exp(-4.108 + 2.39*(log(treetable$DBH_cm[[i]])))
        + exp(-4.786 + 2.389*(log(treetable$DBH_cm[[i]])))
        + exp(-2.455 + 1.4*(log(treetable$DBH_cm[[i]])))
        + exp(-4.151 + 1.982*(log(treetable$DBH_cm[[i]])))
      outtable <- c(outtable,biomass) #just used DF calcs for now...
    }
    if (treetable$Species[[i]] == "WF") {
      biomass <- exp(4.36982 + 2.5043 *(log(treetable$DBH_cm[[i]])))/1000
      outtable <- c(outtable,biomass)
    }
    if (treetable$Species[[i]] == "PP") {
      biomass <- (1.0469 * exp(-4.1279 + log(treetable$DBH_cm[[i]]) * 2.7039))
        + (1.0304 * exp(-4.2291 + log(treetable$DBH_cm[[i]]) * 2.2691))
        + (1.0425 * exp(-6.0278 + log(treetable$DBH_cm[[i]]) * 2.8655))
        + (1.1322 * exp(-5.3589 + log(treetable$DBH_cm[[i]]) * 2.2500))
        + (1.0672 * exp(-4.1317 + log(treetable$DBH_cm[[i]]) * 2.0159))
      outtable <- c(outtable,biomass)
    }
    if (treetable$Species[[i]] =="DF") {
      biomass <- exp(-2.656 + 2.53*(log(treetable$DBH_cm[[i]])))
        + exp(-4.108 + 2.39*(log(treetable$DBH_cm[[i]])))
        + exp(-4.786 + 2.389*(log(treetable$DBH_cm[[i]])))
        + exp(-2.455 + 1.4*(log(treetable$DBH_cm[[i]])))
        + exp(-4.151 + 1.982*(log(treetable$DBH_cm[[i]])))
      outtable <- c(outtable,biomass)
    }
    if (treetable$Species[[i]] == "BO" & treetable$DBH_cm[[i]] >= "11") {
      biomass <- (0.0629*((treetable$DBH_cm[[i]]^2)^1.19522)*treetable$Height_m[[i]]^1.00554)/2.205
      outtable <- c(outtable,biomass)
    }
    if (treetable$Species[[i]] == "BO" & treetable$DBH_cm[[i]] < "11") {
      biomass <- (0.15621*((treetable$DBH_cm[[i]]^2)*treetable$Height_m[[i]])*1.00554)/2.205 
      outtable <- c(outtable,biomass)
    }

  }
  treetable$Biomass_kg=outtable
  treetable <<- treetable
  return(treetable)
}

#abovegroundbiomass("all_trees.csv")

#export csv with biomass column
#write.csv(treetable, "~/Dropbox/Plumas_GGRF_FireSafe_Working/RCarbonCalcs/all_trees_biomass.csv", row.names = FALSE)

##Group first by Plot then summarize (sum) biomass
#Use ?group_by for answers 

#library(dplyr)
#by_plot <- treetable %>% group_by(PlotNum)
#plotbiomass <- by_plot %>% summarise(Biomass_kg=sum(Biomass_kg))

#plotbiomass #final table of just plots and total biomass


#export plot biomass table to csv in dropbox folder
#write.csv(plotbiomass,"~/Dropbox/Plumas_GGRF_FireSafe_Working/RCarbonCalcs/PlotBiomass.csv", row.names = FALSE)



#### the end ####


##Extra Ashley code that might be helpful later
#group by plot and add up biomass 

#Plots <- as.list(treetable$PlotNum)
#plotnodups <- as.list(paste(unique(Plots)))
#groups <- c(plotnodups)
#aggregate(treetable$Biomass, Plots)

#treetabletest <- treetable
#outtable <- c(1,2,3,4,5,6,7,8)

#treetabletest$biomass_kg=outtable
