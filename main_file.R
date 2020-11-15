
rm(list = ls())


timestep=0.001 #0.001
iter=1000

airborneval<-c(TRUE,FALSE)
SIM <- c("airborneonly","surfonly")   

NUM.SIM <- length(SIM)     # Count the number of iterations for the automated simulations


this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)    

betapoisson<-read.csv('Exact_BetaPoisson_Bootstrap.csv')

require(gsl)


for(a in 1:NUM.SIM){
  
  sim.name <- SIM[a]
  airborne<-airborneval[a]
  
  source('scenarios.R')
  
  if(sim.name=="airborneonly"){if(dir.exists("airborneonly")==FALSE){dir.create("airborneonly"); setwd("airborneonly")}else{setwd("airborneonly")}}
  if(sim.name=="surfonly"){if(dir.exists("surfonly")==FALSE){dir.create("surfonly"); setwd("surfonly")}else{setwd("surfonly")}}
  
  write.csv(plot.frame,file=sprintf('%s.%f.%f.plot.frame.csv',sim.name,iter,timestep))
  saveRDS(all.scenario,file=sprintf('allscenario.%s.%f.%f.plot.frame.rds',sim.name,iter,timestep))
  saveRDS(all.params,file=sprintf('allparams.%s.%f.%f.plot.frame.rds',sim.name,iter,timestep))
  
  #reset directory to parent folder so we can go to correct subfolder within parent folder for next sim run
  setwd(this.dir)
 
   
}