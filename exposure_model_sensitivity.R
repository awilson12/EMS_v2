
set.seed(34)

#diameters<-c("d1","d2")
  
save.total<-list()
infect.track<-rep(NA,iter)

source('defining_parameters.R')
  
#for (j in 1:length(diameters)){
    
  #size<-diameters[j]

  val.contam<-10^runif(iter,-5,5)

  source('defining_rates.R')

  alphaparam<-rep(NA,iter)
  betaparam<-rep(NA,iter)
    
    for (l in 1:iter){
      
      source('defining_probabilities.R')
      
      #creating a matrix where each column is time point and each row is a state
      columns=length(1:(call[l]*(1/timestep)))
      sim.mat<-matrix(nrow=16,ncol=columns) #convert min to one-thousandth of a minute
      sim.mat[3:8,1]<-rep(val.contam[l],6)
      sim.mat[1:2,1]<-0
      sim.mat[9:16,1]<-0
      
      
      
      alphaparam[l]<-betapoisson$alpha[alphabetapairs[l]]
      betaparam[l]<-betapoisson$Beta[alphabetapairs[l]]
      
      for (i in 2:(call[l]*(1/timestep))){
        
        sim.mat[1,i]<-sim.mat[1,i-1]-(sim.mat[1,i-1]*sum(P.1.total[2:16]))+emissions[l]
        
        
        sim.mat[2,i]<-sim.mat[2,i-1]+sim.mat[1,i-1]*P.1.total[2]
        
        
        sim.mat[3,i]<-sim.mat[3,i-1]+(sim.mat[12,i-1]*P.12.total[3])+(sim.mat[13,i-1]*P.13.total[3])-
          (sim.mat[3,i-1]*sum(P.3.total[12:13],P.3.total[16]))+(sim.mat[1,i-1]*P.1.total[3])
        
        
        sim.mat[4,i]<-sim.mat[4,i-1]+(sim.mat[12,i-1]*P.12.total[4])+(sim.mat[13,i-1]*P.13.total[4])-
          (sim.mat[4,i-1]*sum(P.4.total[12:13],P.4.total[16]))+(sim.mat[1,i-1]*P.1.total[4])
        
        
        sim.mat[5,i]<-sim.mat[5,i-1]+(sim.mat[12,i-1]*P.12.total[5])+(sim.mat[13,i-1]*P.13.total[5])-
          (sim.mat[5,i-1]*sum(P.5.total[12:13],P.5.total[16]))+(sim.mat[1,i-1]*P.1.total[5])
        
        
        sim.mat[6,i]<-sim.mat[6,i-1]+(sim.mat[12,i-1]*P.12.total[6])+(sim.mat[13,i-1]*P.13.total[6])-
          (sim.mat[6,i-1]*sum(P.6.total[12:13],P.6.total[16]))+(sim.mat[1,i-1]*P.1.total[6])
        
        
        sim.mat[7,i]<-sim.mat[7,i-1]+(sim.mat[12,i-1]*P.12.total[7])+(sim.mat[13,i-1]*P.13.total[7])-
          (sim.mat[7,i-1]*sum(P.7.total[12:13],P.7.total[16]))+(sim.mat[1,i-1]*P.1.total[7])
        
        
        sim.mat[8,i]<-sim.mat[8,i-1]+(sim.mat[12,i-1]*P.12.total[8])+(sim.mat[13,i-1]*P.13.total[8])-
          (sim.mat[8,i-1]*sum(P.8.total[12:13],P.8.total[16]))+(sim.mat[1,i-1]*P.1.total[8])
        
        
        sim.mat[9,i]<-sim.mat[9,i-1]+(sim.mat[1,i-1]*P.1.total[9])
        
        
        sim.mat[10,i]<-sim.mat[10,i-1]+(sim.mat[1,i-1]*P.1.total[10])
        
        
        sim.mat[11,i]<-sim.mat[11,i-1]+(sim.mat[1,i-1]*P.1.total[11])
        
        
        sim.mat[12,i]<-sim.mat[12,i-1]-sim.mat[12,i-1]*sum(P.12.total[3:8],P.12.total[14:16])+
          (sim.mat[3,i-1]*P.3.total[12])+(sim.mat[4,i-1]*P.4.total[12])+(sim.mat[5,i-1]*P.5.total[12])+
          (sim.mat[6,i-1]*P.6.total[12])+(sim.mat[7,i-1]*P.7.total[12])+(sim.mat[8,i-1]*P.8.total[12])
        
        
        sim.mat[13,i]<-sim.mat[13,i-1]-sim.mat[13,i-1]*sum(P.13.total[3:8],P.13.total[14:16])+
          (sim.mat[3,i-1]*P.3.total[13])+(sim.mat[4,i-1]*P.4.total[13])+(sim.mat[5,i-1]*P.5.total[13])+
          (sim.mat[6,i-1]*P.6.total[13])+(sim.mat[7,i-1]*P.7.total[13])+(sim.mat[8,i-1]*P.8.total[13])
        
        sim.mat[14,i]<-sim.mat[14,i-1]+(sim.mat[12,i-1]*P.12.total[14])
        
        sim.mat[15,i]<-sim.mat[15,i-1]+(sim.mat[13,i-1]*P.13.total[15])
        
        sim.mat[16,i]<-sim.mat[16,i-1]+(sim.mat[1,i-1]*P.1.total[16])+(sim.mat[3,i-1]*P.3.total[16])+(sim.mat[4,i-1]*P.4.total[16])+
          (sim.mat[5,i-1]*P.5.total[16])+(sim.mat[6,i-1]*P.6.total[16])+(sim.mat[7,i-1]*P.7.total[16])+
          (sim.mat[8,i-1]*P.8.total[16])+(sim.mat[12,i-1]*P.12.total[16])+(sim.mat[13,i-1]*P.13.total[16])
        
        
      }#end of time loop (i)
      
      dose1<-sim.mat[9,i]+sim.mat[14,i]
      dose2<-sim.mat[10,i]+sim.mat[15,i]
      
      infect1<-1-hyperg_1F1(alphaparam[l], alphaparam[l]+betaparam[l], -dose1, give=FALSE, strict=TRUE)
      infect2<-1-hyperg_1F1(alphaparam[l], alphaparam[l]+betaparam[l], -dose2, give=FALSE, strict=TRUE)
      infect.track[l]<-infect1
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
      
      
    }      #end of iteration loop (l)
  
    
 # }#end of diameter loop (j)
  
  params<-data.frame(TE.HS=TE.HS,TE.SH=TE.SH,TE.HF=TE.HF,H.surf=H.surf,H.face=H.face,H.eyes=H.eyes,
                     S.H=S.H,S.F=S.F,A.hand=A.hand,inactiv.fome=inactiv.fome,inactiv.air=inactiv.air,
                     inactiv.hands=inactiv.hands,I=I,M=M,AER=AER,alpha=alphaparam,beta=betaparam,
                     emissions=emissions,val.contam=val.contam,infect.track=infect.track)
  
  #save.all<<-save.list
  #params.all<<-params.list
  
#}#end of function
#save.total<-list()

#for (i in 1:iter){
#  save.total[[i]]<-save.list[[i]]+save.list[[i+iter]]
#  save.total[[i]]$time<-save.total[[i]]$time/2
#}

    

