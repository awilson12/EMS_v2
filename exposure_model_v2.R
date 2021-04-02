
set.seed(34)

#diameters<-c("d1","d2")
  
save.total<-list()

source('defining_parameters.R')
  
#for (j in 1:length(diameters)){
    
  #size<-diameters[j]
    
  source('defining_rates.R')

  alphaparam<-rep(NA,iter)
  betaparam<-rep(NA,iter)
    
    for (l in 1:iter){
      
      source('defining_probabilities.R')
      
      #creating a matrix where each column is time point and each row is a state
      columns=length(1:(call[l]*(1/timestep)))
      sim.mat<-matrix(nrow=16,ncol=columns) #convert min to one-thousandth of a minute
      
      if(airborne==TRUE){
        sim.mat[,1]<-rep(0,16)
      }else{
        #give non-zero starting conditions to surfaces for this scenario
        
        #parameters from PPE paper with Marco (triangular informed by Guo et al. 2020,
        #which are genome copies/swabbed area) / assumptions about area swabbed x assumed
        #fraction to be infectious
        #chance.contam<-runif(6,0,1)
        #fraction.contam<-runif(1,0.4,0.75) #between 40 and 75% positivity rate
        #SA<-c(glucometer.SA,headphones.SA,jumpbag.SA,keyboard.SA,touchscreen.SA,radio.SA)
        
        #we pull concentrations from scenario 1 with the same mask scenario and iteration #
        
        if (patientmask==TRUE & paramask==FALSE){
          tempframe<-scenario1.output[[l]]
        }else if (patientmask==FALSE & paramask==FALSE){
          tempframe<-scenario2.output[[l]]
        }else if (patientmask==FALSE & paramask==TRUE){
          tempframe<-scenario3.output[[l]]
        }else if (patientmask==TRUE & paramask==TRUE){
          tempframe<-scenario4.output[[l]]
        }
        
        final.time<-length(tempframe$state1)
        
        sim.mat[1,1]<-tempframe$state1[final.time] #added this change during revision stage
        sim.mat[3,1]<-tempframe$state3[final.time]
        sim.mat[4,1]<-tempframe$state4[final.time]
        sim.mat[5,1]<-tempframe$state5[final.time]
        sim.mat[6,1]<-tempframe$state6[final.time]
        sim.mat[7,1]<-tempframe$state7[final.time]
        sim.mat[8,1]<-tempframe$state8[final.time]
        
        #for (z in 1:6){
          #if (chance.contam[z]<=fraction.contam){
            #conc<-(rtriangle(1,a=3.3E3, c=2.8E4, b=6.6E4)/rtriangle(1,a=5,b=195,c=100))*runif(1,0.001,0.1)
          #}else{
          #  conc<-0
          #}
          #sim.mat[2+z,1]<-conc*SA[z]
        #}
        
        #set other states starting at zero
        sim.mat[2,1]<-0 #added a change during revision stage
        sim.mat[9:16,1]<-0
      }
      
      
      alphaparam[l]<-betapoisson$alpha[alphabetapairs[l]]
      betaparam[l]<-betapoisson$Beta[alphabetapairs[l]]
      
      Ptemp<-P
      
      for (i in 2:(call[l]*(1/timestep))){
      
      #Ptemp<-Ptemp%*%P
        
      sim.mat[,i]<-sim.mat[,i-1]%*%Ptemp
      sim.mat[1,i]<-sim.mat[1,i]+emissions[l]
        
      }#end of time loop (i)
      
      dose1<-sim.mat[9,i]+sim.mat[14,i]
      dose2<-sim.mat[10,i]+sim.mat[15,i]
      
      infect1<-1-hyperg_1F1(alphaparam[l], alphaparam[l]+betaparam[l], -dose1, give=FALSE, strict=TRUE)
      infect2<-1-hyperg_1F1(alphaparam[l], alphaparam[l]+betaparam[l], -dose2, give=FALSE, strict=TRUE)
      
      frame<-data.frame(state1=sim.mat[1,],state2=sim.mat[2,],state3=sim.mat[3,],state4=sim.mat[4,],
                        state5=sim.mat[5,],state6=sim.mat[6,],state7=sim.mat[7,],state8=sim.mat[8,],
                        state9=sim.mat[9,],state10=sim.mat[10,],state11=sim.mat[11,],state12=sim.mat[12,],
                        state13=sim.mat[13,],state14=sim.mat[14,],state15=sim.mat[15,],state16=sim.mat[16,],time=1:(call[l]*(1/timestep)),
                        infect1=infect1,infect2=infect2)
      
      
      #if (j==1){
        save.total[[l]]<-frame
        
      #}else{
      #  save.list[[l+iter]]<-frame
        #params.list[[l+iter]]<-params
      #}
      
      
    }#end of iteration loop (l)
  
    
 # }#end of diameter loop (j)
  
  params<-data.frame(TE.HS=TE.HS,TE.SH=TE.SH,TE.HF=TE.HF,H.surf=H.surf,H.face=H.face,H.mask=H.mask,
                     S.H=S.H,S.F=S.F,A.hand=A.hand,inactiv.fome=inactiv.fome,inactiv.air=inactiv.air,RNAinfective=RNAinfective,
                     inactiv.hands=inactiv.hands,I=I,M.patient.filter=M.patient.filter,M.patient.sourcecontrol=M.patient.sourcecontrol,
                     M.EMS=M.EMS,AER=AER,alpha=alphaparam,beta=betaparam,emissions=emissions)
  
  #save.all<<-save.list
  #params.all<<-params.list
  
#}#end of function
#save.total<-list()

#for (i in 1:iter){
#  save.total[[i]]<-save.list[[i]]+save.list[[i+iter]]
#  save.total[[i]]$time<-save.total[[i]]$time/2
#}

    

