states<-c("room air","exhaust",
          "glucometer","headphones","jumpbag","keyboard","touchscreen","radio",
          "resp para 1","resp para 2","patient","para hands 1","para hands 2",
          "para mucous 1","para mucous 2","loss of viability")


#ROW 1: rates for transitions from room air----------------------------------------------

#1->2 room air to exhaust

lambda.1.2<-AER*(1/V.room)

#1->3:8 room air to surfaces

#if (size=="d1"){
  lambda.1.3<-rep(d1.settle,iter)
  lambda.1.4<-rep(d1.settle,iter)
  lambda.1.5<-rep(d1.settle,iter)
  lambda.1.6<-rep(d1.settle,iter)
  lambda.1.7<-rep(d1.settle,iter)
  lambda.1.8<-rep(d1.settle,iter)
#}else{
#  lambda.1.3<-rep(d2.settle,iter)
#  lambda.1.4<-rep(d2.settle,iter)
#  lambda.1.5<-rep(d2.settle,iter)
#  lambda.1.6<-rep(d2.settle,iter)
#  lambda.1.7<-rep(d2.settle,iter)
#  lambda.1.8<-rep(d2.settle,iter)
#}

#1->9:11 room air to respiratory tracts (with mask vs. no mask designations)
#this also affects contact rates with mucosal membranes, so we handle
#that here as well

if(paramask==TRUE){
  lambda.1.9<-(1/V.room)*I*(1-M)
  lambda.1.10<-(1/V.room)*I*(1-M)
  
  lambda.12.14<-0.5*S.F*TE.HF*H.eyes #accounting for only one hand being used
  lambda.13.15<-0.5*S.F*TE.HF*H.eyes #accounting for only one hand being used
  
}else{
  lambda.1.9<-(1/V.room)*I
  lambda.1.10<-(1/V.room)*I
  
  lambda.12.14<-0.5*S.F*TE.HF*H.face #accounting for only one hand being used
  lambda.13.15<-0.5*S.F*TE.HF*H.face #accounting for only one hand being used
}

if(patientmask==TRUE){
  lambda.1.11<-(1/V.room)*I*(1-M)
}else{
  lambda.1.11<-(1/V.room)*I
}

#1->16 room air to inviability
lambda.1.16<-inactiv.air

#ROW 2 (absorbing state, so no rates to other states)

#ROWS 3-8---------------------------------------------------------

#transition of fomites to hands
non.fome.specific<-S.H*TE.SH*A.hand*H.surf

lambda.3.12<-glucometer*glucometer.SA*non.fome.specific
lambda.4.12<-headphones*headphones.SA*non.fome.specific
lambda.5.12<-jumpbag*jumpbag.SA*non.fome.specific
lambda.6.12<-keyboard*keyboard.SA*non.fome.specific
lambda.7.12<-touchscreen*touchscreen.SA*non.fome.specific
lambda.8.12<-radio*radio.SA*non.fome.specific

lambda.3.13<-glucometer*glucometer.SA*non.fome.specific
lambda.4.13<-headphones*headphones.SA*non.fome.specific
lambda.5.13<-jumpbag*jumpbag.SA*non.fome.specific
lambda.6.13<-keyboard*keyboard.SA*non.fome.specific
lambda.7.13<-touchscreen*touchscreen.SA*non.fome.specific
lambda.8.13<-radio*radio.SA*non.fome.specific

#transiton of fomites to inviability
lambda.3.16<-inactiv.fome
lambda.4.16<-inactiv.fome
lambda.5.16<-inactiv.fome
lambda.6.16<-inactiv.fome
lambda.7.16<-inactiv.fome
lambda.8.16<-inactiv.fome

#ROWS 9-11 no transitions since these are absorbing states

#ROWS 12-13---------HANDS--------------------------------------

#transition to fomites to hands
non.fome.specific.2<-0.5*S.H*TE.HS

lambda.12.3<-glucometer*non.fome.specific.2
lambda.12.4<-headphones*non.fome.specific.2
lambda.12.5<-jumpbag*non.fome.specific.2
lambda.12.6<-keyboard*non.fome.specific.2
lambda.12.7<-touchscreen*non.fome.specific.2
lambda.12.8<-radio*non.fome.specific.2

lambda.13.3<-glucometer*non.fome.specific.2
lambda.13.4<-headphones*non.fome.specific.2
lambda.13.5<-jumpbag*non.fome.specific.2
lambda.13.6<-keyboard*non.fome.specific.2
lambda.13.7<-touchscreen*non.fome.specific.2
lambda.13.8<-radio*non.fome.specific.2

#transition to inviability
lambda.12.16<-inactiv.hands
lambda.13.16<-inactiv.hands

#transition to mucosal membrane (taken care of above)

#ROWS 14-16 - absorbing states, so no transitions away