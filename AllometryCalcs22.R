setwd("~/Desktop")

treedata <- read.csv("Plumas_TreeData_Masterspreadsheet_2023.csv")
treedata

treedata$Species13 <- treedata$X2013.FIA.Species.Code
treedata$Species13[treedata$Species13=="117"] <- "SP"
treedata$Species13[treedata$Species13=="81"] <- "IC"
treedata$Species13[treedata$Species13=="081"] <- "IC"
treedata$Species13[treedata$Species13=="202"] <- "DF"
treedata$Species13[treedata$Species13=="15"] <- "WF"
treedata$Species13[treedata$Species13=="015"] <- "WF"
treedata$Species13[treedata$Species13=="818"] <- "BO"
treedata$Species13[treedata$Species13=="122"] <- "PP"
treedata$Species13[treedata$Species13=="116"] <- "JP"

treedata$Species19 <- treedata$X2019.FIA.Species.Code
treedata$Species19[treedata$Species19=="117"] <- "SP"
treedata$Species19[treedata$Species19=="81"] <- "IC"
treedata$Species19[treedata$Species19=="081"] <- "IC"
treedata$Species19[treedata$Species19=="202"] <- "DF"
treedata$Species19[treedata$Species19=="15"] <- "WF"
treedata$Species19[treedata$Species19=="015"] <- "WF"
treedata$Species19[treedata$Species19=="818"] <- "BO"
treedata$Species19[treedata$Species19=="122"] <- "PP"
treedata$Species19[treedata$Species19=="116"] <- "JP"

treedata$Species21 <- treedata$X2021.FIA.Species.Code
treedata$Species21[treedata$Species21=="117"] <- "SP"
treedata$Species21[treedata$Species21=="81"] <- "IC"
treedata$Species21[treedata$Species21=="081"] <- "IC"
treedata$Species21[treedata$Species21=="202"] <- "DF"
treedata$Species21[treedata$Species21=="15"] <- "WF"
treedata$Species21[treedata$Species21=="015"] <- "WF"
treedata$Species21[treedata$Species21=="818"] <- "BO"
treedata$Species21[treedata$Species21=="122"] <- "PP"
treedata$Species21[treedata$Species21=="116"] <- "JP"

treedata$Species22 <- treedata$X2022.FIA.Species.Code
treedata$Species22[treedata$Species22=="117"] <- "SP"
treedata$Species22[treedata$Species22=="81"] <- "IC"
treedata$Species22[treedata$Species22=="081"] <- "IC"
treedata$Species22[treedata$Species22=="202"] <- "DF"
treedata$Species22[treedata$Species22=="15"] <- "WF"
treedata$Species22[treedata$Species22=="015"] <- "WF"
treedata$Species22[treedata$Species22=="818"] <- "BO"
treedata$Species22[treedata$Species22=="122"] <- "PP"
treedata$Species22[treedata$Species22=="116"] <- "JP"

write.csv(treedata,"~/Desktop/AllTrees.csv")
treedata <- read.csv("AllTrees.csv")

abovegroundbiomass <- function(treedata="AllTrees.csv"){  
  treetable <- read.csv(treedata)
  outtable <- c() 
  for (i in seq(treetable$Species13)) {
    if (treetable$Species13[[i]] == "SP") {
      biomass13 <-  exp(-3.984 + 2.667*(log(treetable$X2013.DBH[[i]])))
      + exp(-5.295 + 2.619*(log(treetable$X2013.DBH[[i]])))
      + exp(-7.637 + 3.365*(log(treetable$X2013.DBH[[i]])))
      + exp(-5.413 + 2.127*(log(treetable$X2013.DBH[[i]])))
      + exp(-4.023 + 2.032*(log(treetable$X2013.DBH[[i]])))
      outtable <- c(outtable,biomass13)
    }
    if (treetable$Species13[[i]] == "JP") {
      biomass13 <- (1.0469 * exp(-4.1279 + log(treetable$X2013.DBH[[i]]) * 2.7039))
      + (1.0304 * exp(-4.2291 + log(treetable$X2013.DBH[[i]]) * 2.2691))
      + (1.0425 * exp(-6.0278 + log(treetable$X2013.DBH[[i]]) * 2.8655))
      + (1.1322 * exp(-5.3589 + log(treetable$X2013.DBH[[i]]) * 2.2500))
      + (1.0672 * exp(-4.1317 + log(treetable$X2013.DBH[[i]]) * 2.0159))
      outtable <- c(outtable,biomass13)
    }
    if (treetable$Species13[[i]] == "IC") {
      biomass13 <- exp(-2.656 + 2.53*(log(treetable$X2013.DBH[[i]])))
      + exp(-4.108 + 2.39*(log(treetable$X2013.DBH[[i]])))
      + exp(-4.786 + 2.389*(log(treetable$X2013.DBH[[i]])))
      + exp(-2.455 + 1.4*(log(treetable$X2013.DBH[[i]])))
      + exp(-4.151 + 1.982*(log(treetable$X2013.DBH[[i]])))
      outtable <- c(outtable,biomass13) #just used DF calcs for now...
    }
    if (treetable$Species13[[i]] == "WF") {
      biomass13 <- exp(4.36982 + 2.5043 *(log(treetable$X2013.DBH[[i]])))/1000
      outtable <- c(outtable,biomass13)
    }
    if (treetable$Species13[[i]] == "PP") {
      biomass13 <- (1.0469 * exp(-4.1279 + log(treetable$X2013.DBH[[i]]) * 2.7039))
      + (1.0304 * exp(-4.2291 + log(treetable$X2013.DBH[[i]]) * 2.2691))
      + (1.0425 * exp(-6.0278 + log(treetable$X2013.DBH[[i]]) * 2.8655))
      + (1.1322 * exp(-5.3589 + log(treetable$X2013.DBH[[i]]) * 2.2500))
      + (1.0672 * exp(-4.1317 + log(treetable$X2013.DBH[[i]]) * 2.0159))
      outtable <- c(outtable,biomass13)
    }
    if (treetable$Species13[[i]] =="DF") {
      biomass13 <- exp(-2.656 + 2.53*(log(treetable$X2013.DBH[[i]])))
      + exp(-4.108 + 2.39*(log(treetable$X2013.DBH[[i]])))
      + exp(-4.786 + 2.389*(log(treetable$X2013.DBH[[i]])))
      + exp(-2.455 + 1.4*(log(treetable$X2013.DBH[[i]])))
      + exp(-4.151 + 1.982*(log(treetable$X2013.DBH[[i]])))
      outtable <- c(outtable,biomass13)
    }
    if (treetable$Species13[[i]] == "BO" & treetable$X2013.DBH[[i]] >= 11) {
      biomass13 <- (0.0629*((treetable$X2013.DBH[[i]]^2)^1.19522)*treetable$X2013.Height[[i]]^1.00554)/2.205
      outtable <- c(outtable,biomass13)
    }
    if (treetable$Species13[[i]] == "BO" & treetable$X2013.DBH[[i]] < 11) {
      biomass13 <- (0.15621*((treetable$X2013.DBH[[i]]^2)*treetable$X2013.Height[[i]])*1.00554)/2.205 
      outtable <- c(outtable,biomass13)
    }
    
  }
  treetable$Biomass_kg13=outtable
  treetable <<- treetable
  return(treetable)
}

abovegroundbiomass("AllTrees.csv")

#export csv with biomass column
write.csv(treetable, "~/Desktop/AllTrees.csv", row.names = FALSE)

##Group first by Plot then summarize (sum) biomass
#Use ?group_by for answers 

library(dplyr)
by_plot <- treetable %>% group_by(PlotNum)
plotbiomass <- by_plot %>% summarise(Biomass_kg=sum(Biomass_kg))

plotbiomass #final table of just plots and total biomass


#export plot biomass table to csv in dropbox folder
write.csv(plotbiomass,"~/Dropbox/Plumas_GGRF_FireSafe_Working/RCarbonCalcs/2022/PlotBiomass.csv", row.names = FALSE)



#### the end ####


##Extra Ashley code that might be helpful later
#group by plot and add up biomass 

Plots <- as.list(treetable$PlotNum)
plotnodups <- as.list(paste(unique(Plots)))
groups <- c(plotnodups)
aggregate(treetable$Biomass, Plots)

treetabletest <- treetable
outtable <- c(1,2,3,4,5,6,7,8)

treetabletest$biomass_kg=outtable
