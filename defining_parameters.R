require(truncdist)
require(triangle)

#TE hand->surface
TE.HS<-runif(iter,0.0061,0.248)
  
#TE surface->hand
TE.SH<-runif(iter,0.0061,0.248)
  
#TE hand->face
TE.HF<-rtrunc(iter,"norm",mean=0.3390,sd=0.1318,a=0,b=1)
  
#hand-to-nonporous surface contact frequency
H.surf<-rlnorm(iter,meanlog=log(4.1),sdlog=log(1.6))/2 #contacts/min, divide by 2 to get contact rate per hand (assuming 1 hand per
#contact and that right vs. left have equal chance of being used)
  
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

#if not using mask
H.mouth<-rtrunc(iter,"norm",mean=2.9,sd=2.5,a=0,b=10)*(1/60)*(1/2) #account for only one hand being used
H.nose<-rtrunc(iter,"norm",mean=2.5,sd=2.2,a=0,b=14)*(1/60)*(1/2)

H.face<-H.mouth+H.nose
#if using mask
H.eyes<-rtrunc(iter,"norm",mean=2.4,sd=1.9,a=0,b=8)*(1/60)*(1/2) #account for only one hand being used

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
I<-rtrunc(iter,"norm",mean=2.6E-2,sd=5.5E-3,a=1.5E-2) #m^3/min

#mask efficacy
M<-rtrunc(iter,"norm",mean=0.95,sd=0.00275,a=0,b=1)

#air exchange rate (in units of per hour so multiply by 1/60 to conver to per min)
AER<-runif(iter,36,138)*(1/60)

#gravitational settling, d1 aerosols (per day, so multiply by (1/24)*(1/60)) to conver to per min
d1.settle<-rtriangle(iter,a=21.60,b=36,c=28.80)*(1/24)*(1/60)

RNAinfective<-runif(iter,0.001,0.01) #Range from Jones (2020)

if (airborne==TRUE){
  if (patientmask==TRUE){
    #values from Leung et al (2020)
    emissions<-(rtriangle(iter,a=10^0,b=10^5,c=10^0)/30)*(1-M)*RNAinfective
    
  }else{
    emissions<-(rtriangle(iter,a=10^0,b=10^5,c=10^0)/30)*RNAinfective
  }
}else{
  emissions<-rep(0,iter)
}

#dose-response parameters
alphabetapairs<-sample(1:length(betapoisson$alpha),iter,replace=TRUE)

#call duration
call<-round(rtriangle(iter,a=5,b=20,c=12.5),1)

