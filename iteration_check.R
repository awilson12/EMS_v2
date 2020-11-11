
#--------SCENARIO 1

require(gsl)

timestep=0.001 #0.001
iter=10000

airborne<-TRUE

patientmask=TRUE
paramask=FALSE

this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)    

betapoisson<-read.csv('Exact_BetaPoisson_Bootstrap.csv')

#run exposure model function (definition of rates and probabilities are nested within the function do to varying based on patientmask and paramask arguments)
source('exposure_model.R')

scenario1.output<-save.total
params1.output<-params

infect<-rep(NA,iter)
for (i in 1:iter){
  tempframe<-save.total[[i]]
  infect[i]<-tempframe$infect1[1]
}

summary(infect)

#---------SCENARIO 2

patientmask=FALSE
paramask=FALSE

#run exposure model function (definition of rates and probabilities are nested within the function do to varying based on patientmask and paramask arguments)
source('exposure_model.R')

scenario2.output<-save.total
params2.output<-params

#-------SCENARIO 3

patientmask=FALSE
paramask=TRUE

#run exposure model function (definition of rates and probabilities are nested within the function do to varying based on patientmask and paramask arguments)
source('exposure_model.R')

scenario3.output<-save.total
params3.output<-params

#-------SCENARIO 4

patientmask=TRUE
paramask=TRUE

#run exposure model function (definition of rates and probabilities are nested within the function do to varying based on patientmask and paramask arguments)
source('exposure_model.R')

scenario4.output<-save.total
params4.output<-params

#------ALL

all.scenario<-c(scenario1.output,scenario2.output,scenario3.output,scenario4.output)
all.params<-rbind(params1.output,params2.output,params3.output,params4.output)


require(ggplot2)

matrix.means<-matrix(ncol=16,nrow=call*(1/timestep))
matrix.sds<-matrix(ncol=16,nrow=call*(1/timestep))

states<-c("room air","exhaust",
          "glucometer","headphones","jumpbag","keyboard","touchscreen","radio",
          "resp para 1","resp para 2","patient inhalation","para hands 1","para hands 2",
          "para mucous 1","para mucous 2","loss of viability")

for (j in 1:(call[l]*(1/timestep))){
  
  #gathering all the concentrations for time j for all iterations into virus, a data.frame
  for (i in 1:iter){
    virustemp<-scenario1.output[[i]][scenario1.output[[i]]$time==j,]
    if (i==1){
      virus<-virustemp
    }else{
      virus<-rbind(virus,virustemp)
    }
  }
  
 #taking a mean of all the concentrations per state for time j
  for(k in 1:16){
    if(k==1 & j==1){
      means.1<-mean(virus[,k][virus$time==j])
      sd.1<-sd(virus[,k][virus$time==j])
      state.1<-states[k]
      time.1<-j
    }else{
      means.1<-c(means.1,mean(virus[,k][virus$time==j]))
      sd.1<-c(sd.1,sd(virus[,k][virus$time==j]))
      state.1<-c(state.1,states[k])
      time.1<-c(time.1,j)
    }
  }
}


for (j in 1:(call[l]*(1/timestep))){
  
  #gathering all the concentrations for time j for all iterations into virus, a data.frame
  for (i in 1:iter){
    virustemp<-scenario2.output[[i]][scenario2.output[[i]]$time==j,]
    if (i==1){
      virus<-virustemp
    }else{
      virus<-rbind(virus,virustemp)
    }
  }
  
  #taking a mean of all the concentrations per state for time j
  for(k in 1:16){
    if(k==1 & j==1){
      means.2<-mean(virus[,k][virus$time==j])
      sd.2<-sd(virus[,k][virus$time==j])
      state.2<-states[k]
      time.2<-j
    }else{
      means.2<-c(means.2,mean(virus[,k][virus$time==j]))
      sd.2<-c(sd.2,sd(virus[,k][virus$time==j]))
      state.2<-c(state.2,states[k])
      time.2<-c(time.2,j)
    }
  }
}

for (j in 1:(call[l]*(1/timestep))){
  
  #gathering all the concentrations for time j for all iterations into virus, a data.frame
  for (i in 1:iter){
    virustemp<-scenario3.output[[i]][scenario3.output[[i]]$time==j,]
    if (i==1){
      virus<-virustemp
    }else{
      virus<-rbind(virus,virustemp)
    }
  }
  
  #taking a mean of all the concentrations per state for time j
  for(k in 1:16){
    if(k==1 & j==1){
      means.3<-mean(virus[,k][virus$time==j])
      sd.3<-sd(virus[,k][virus$time==j])
      state.3<-states[k]
      time.3<-j
    }else{
      means.3<-c(means.3,mean(virus[,k][virus$time==j]))
      sd.3<-c(sd.3,sd(virus[,k][virus$time==j]))
      state.3<-c(state.3,states[k])
      time.3<-c(time.3,j)
    }
  }
}

for (j in 1:(call[l]*(1/timestep))){
  
  #gathering all the concentrations for time j for all iterations into virus, a data.frame
  for (i in 1:iter){
    virustemp<-scenario4.output[[i]][scenario4.output[[i]]$time==j,]
    if (i==1){
      virus<-virustemp
    }else{
      virus<-rbind(virus,virustemp)
    }
  }
  
  #taking a mean of all the concentrations per state for time j
  for(k in 1:16){
    if(k==1 & j==1){
      means.4<-mean(virus[,k][virus$time==j])
      sd.4<-sd(virus[,k][virus$time==j])
      state.4<-states[k]
      time.4<-j
    }else{
      means.4<-c(means.4,mean(virus[,k][virus$time==j]))
      sd.4<-c(sd.4,sd(virus[,k][virus$time==j]))
      state.4<-c(state.4,states[k])
      time.4<-c(time.4,j)
    }
  }
}

plot.frame<-data.frame(means=c(means.1,means.2,means.3,means.4),
                       sd=c(sd.1,sd.2,sd.3,sd.4),
                       state=c(state.1,state.2,state.3,state.4),
                       time=c(time.1,time.2,time.3,time.4),
                       model=c(rep("Patient mask Only",length(means.1)),
                               rep("No one Masked",length(means.2)),
                               rep("Paramedics Masked Only",length(means.3)),
                               rep("Patient and Paramedics Masked",length(means.4))))