require(truncdist)
require(triangle)

#TE hand->surface
TE.HS<-runif(iter,0.0061,0.248)
  
#TE surface->hand
TE.SH<-runif(iter,0.0061,0.248)
  
#TE hand->face
TE.HF<-rtrunc(iter,"norm",mean=0.3390,sd=0.1318,a=0,b=1)
  
#hand-to-nonporous surface contact frequency
#H.surf<-rlnorm(iter,meanlog=log(4.1),sdlog=log(1.6)) 
H.surf<-rtrunc(iter,"norm",mean=10.3,sd=3.4,a=5,b=16) #change during revision stage
  
#fomite-specific contact frequencies
glucometer<-H.surf*0.03
headphones<-H.surf*0.44
jumpbag<-H.surf*0.14
keyboard<-H.surf*0.32
touchscreen<-H.surf*0.05
radio<-H.surf*0.04

#surface areas of fomites
glucometer.SA<-7.8
headphones.SA<-148.44
jumpbag.SA<-141.95
keyboard.SA<-305.64
touchscreen.SA<-65.52
radio.SA<-50.84

#hand-to-face contact frequency (in contacts/hr x 1 hr/60 min to conver to per min)

#with respirator
H.mask<-rtriangle(iter,2.3,17.8,5.4)*(1/60)

#without respirator
H.face<-rtriangle(iter,12.8,22.9,20)*(1/60)

#fraction of hand surface area for hand-to-surf contact
S.H<-runif(iter,0.006,0.24)

#fraction of hand surface area for hand-to-face contact
S.F<-runif(iter,0.006,0.012)

#total hand surface area
A.hand<-runif(iter,445,535)

#inactivation on fomites (in per hr so x 1/60 to conver to per min)
inactiv.fome<-runif(iter,0.085,0.151)*(1/60)

#inactivation in air (in per hr so x 1/60 to conver to per min)
inactiv.air<-rtriangle(iter,a=0.096,b=0.420,c=0.253)*(1/60)

#inactivation on hands (gloved) (in per hr so x 1/60 to conver to per min)
inactiv.hands<-runif(iter,0.77,4.61)*(1/60)

#volume of ambulance
V.room<-9.9 #m^3

#inhalation rate
I<-rtrunc(iter,"norm",mean=2.6E-2,sd=6.0E-3,a=1.4E-2) #m^3/min

#respiratory efficacy
n.param<-10
k.param<-3
M.EMS<-1-rbeta(iter,shape1=k.param,shape2=n.param-k.param+1) #changed from original model based on reviewer feedback
mean.ems<-k.param/(n.param-k.param+1+k.param)
mean.ems
1-mean.ems
#filteration efficacy for patient

#method of moments to fit beta distribution based on mean and sd reported by Lindsley
x.filter<-0.509
sd.filter<-0.077
betadist.alpha<-x.filter*(x.filter*((1-x.filter)/(sd.filter^2))-1)
betadist.beta<-betadist.alpha*(1-x.filter)/x.filter

M.patient.filter<-rbeta(iter,betadist.alpha,betadist.beta)

#source control efficacy for patient
M.patient.sourcecontrol<-rbeta(iter,betadist.alpha,betadist.beta) #changed from original model based on reviewer feedback

#air exchange rate (in units of per hour so multiply by 1/60 to conver to per min)
AER<-runif(iter,12,32)*(1/60)

#gravitational settling, d1 aerosols (per day, so multiply by (1/24)*(1/60)) to conver to per min
d1.settle<-rtriangle(iter,a=21.60,b=36,c=28.80)*(1/24)*(1/60)

RNAinfective<-runif(iter,0.001,0.01) 

if (airborne==TRUE){
  if (patientmask==TRUE){
    #values from Leung et al (2020)
    emissions<-(rtriangle(iter,a=10^0,b=10^5,c=10^0)/30)*(1-M.patient.sourcecontrol)*RNAinfective*(timestep)
    
  }else{
    emissions<-(rtriangle(iter,a=10^0,b=10^5,c=10^0)/30)*RNAinfective*(timestep)
  }
}else{
  emissions<-rep(0,iter)
}

#dose-response parameters
alphabetapairs<-sample(1:length(betapoisson$alpha),iter,replace=TRUE)

#call duration
call<-round(rtriangle(iter,a=5,b=20,c=12.5),1)

