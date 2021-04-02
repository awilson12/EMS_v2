#comparing two different coding ways...



patientmask=TRUE
paramask=FALSE

#run exposure model function (definition of rates and probabilities are nested within the function do to varying based on patientmask and paramask arguments)
source('exposure_model.R')


v1.output<-save.total
v1.params<-params



#run exposure model function (definition of rates and probabilities are nested within the function do to varying based on patientmask and paramask arguments)
source('exposure_model_v2.R')

v2.output<-save.total
v2.params<-params

#----compare

head(v1.output[[1]])
head(v2.output[[1]])
