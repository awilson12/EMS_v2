
#--------SCENARIO 1

require(gsl)

timestep=0.001 #0.001
iter=10000

airborne<-TRUE

patientmask=TRUE
paramask=TRUE

this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)    

betapoisson<-read.csv('Exact_BetaPoisson_Bootstrap.csv')

#run exposure model function (definition of rates and probabilities are nested within the function do to varying based on patientmask and paramask arguments)
source('exposure_model_iteration_check.R')

summary(infect1)

airborne<-FALSE

source('exposure_model_iteration_check.R')

summary(infect1)





