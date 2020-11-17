#load in all params files


allparams.surfonly <- readRDS("~/EMS_v2/surfonly/allparams.surfonly.1000.000000.0.001000.plot.frame.rds")
allparams.airborneonly <- readRDS("~/EMS_v2/airborneonly/allparams.airborneonly.1000.000000.0.001000.plot.frame.rds")


allparams.surfonly$infect<-infect.surf
allparams.airborneonly$infect<-infect.airborne

airborne.cor<-signif(cor(allparams.airborneonly[3000:4000,], method = c("spearman")),2)
View(airborne.cor[,18])

surf.cor<-signif(cor(allparams.surfonly[3000:4000,], method = c("spearman")),2)
View(surf.cor[,18])