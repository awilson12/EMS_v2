
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

#-----------------------------------------------------------------------------------------------

#Sensitivity Analysis Model

this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)   

patientmask=FALSE
paramask=FALSE
airborne=FALSE

betapoisson<-read.csv('Exact_BetaPoisson_Bootstrap.csv')

require(gsl)

source('exposure_model_sensitivity.R')

if(dir.exists("sensitivity_scenario")==FALSE){dir.create("sensitivity_scenario"); setwd("sensitivity_scenario")}else{setwd("sensitivity_scenario")}

timestep=0.001 #0.001
iter=1000
params.sensitivity.output<-params

require(ggplot2)
require(ggpubr)

saveRDS(params.sensitivity.output,'params.sensitivity.output.rds')

summary(params.sensitivity.output$val.contam[params.sensitivity.output$infect.track>=5.7*10^-2])
max.point<-max(params.sensitivity.output$val.contam[params.sensitivity.output$infect.track<=5.7E-2])
min.point<-min(params.sensitivity.output$val.contam[params.sensitivity.output$infect.track>=5.7E-2])

windows()
ggplot(data=params.sensitivity.output)+
  geom_rect(aes(xmin=min.point,xmax=max.point,ymin=0,ymax=1),fill="lightblue",alpha=0.03)+
  geom_point(aes(x=val.contam,y=infect.track),size=2)+
  scale_x_continuous(name="Average Number of Viruses per Surface",trans="log10")+
  scale_y_continuous(name="Infection Risk",trans="log10")+
  geom_hline(yintercept=8.3*10^-2,linetype="dashed",color="red",size=2)+
  geom_text(label="Scenario 1A Mean Infection Risk",aes(x=0.3e-02,y=2e-01),size=6)+
  theme_pubr()+
  theme(axis.text = element_text(size=20),axis.title=element_text(size=20))

source('droplet_fraction_estimate.R')
min.frac
max.frac

#-------------------------------------------------------------------------------------------------

#Mask efficacy relationship with risk for scenario 1C
require(ggpmisc)

patientmask=FALSE
paramask=FALSE
airborne=TRUE

timestep=0.001 #0.001
iter=1000

this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)   

source('exposure_model_sensitivity_2.R')

params.sensitivity.output.2.baseline<-params

if(dir.exists("sensitivity_scenario2")==FALSE){dir.create("sensitivity_scenario2"); setwd("sensitivity_scenario2")}else{setwd("sensitivity_scenario2")}

saveRDS(params.sensitivity.output.2.baseline,file="param.sensitivity.2.baseline.rds")

patientmask=TRUE
paramask=FALSE
setwd(this.dir)
source('exposure_model_sensitivity_2.R')

if(dir.exists("sensitivity_scenario2")==FALSE){dir.create("sensitivity_scenario2"); setwd("sensitivity_scenario2")}else{setwd("sensitivity_scenario2")}

params.sensitivity.output.2.intervention<-params
saveRDS(params.sensitivity.output.2.intervention,file="param.sensitivity.2.intervention.rds")


params.sensitivity.output.2.intervention$riskreduce<-(params.sensitivity.output.2.baseline$infect.track-params.sensitivity.output.2.intervention$infect.track)/(params.sensitivity.output.2.baseline$infect.track)*100

windows()
ggplot(data=params.sensitivity.output.2.intervention)+
  geom_point(aes(x=M*100,y=riskreduce),size=2)+
  geom_smooth(method='lm',aes(x=M*100,y=riskreduce))+
  stat_poly_eq(aes(x=M*100,y=riskreduce,label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE,formula=y~x,size=5)+ 
  scale_x_continuous(name="Mask Efficacy (%)")+
  scale_y_continuous(name="Risk Reduction (%)")+
  theme_pubr()+
  theme(axis.text = element_text(size=20),axis.title=element_text(size=20))




