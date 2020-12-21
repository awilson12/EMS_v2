
require(ggplot2)
require(ggpubr)

plot.frame$model[plot.frame$model=="No one Masked"]<-"A. No One with Respirators"
plot.frame$model[plot.frame$model=="Patient mask Only"]<-"C. Patient Respirator Only"
plot.frame$model[plot.frame$model=="Patient and Paramedics Masked"]<-"D. Patient and First Responder Respirators"
plot.frame$model[plot.frame$model=="Paramedics Masked Only"]<-"B. First Responder Respirators Only"

plot.frame.temp<-plot.frame[!is.na(plot.frame$sd) & !is.na(plot.frame$means),]

plot.frame.temp<-plot.frame.temp[plot.frame.temp$state=="resp para 1" | plot.frame.temp$state=="para mucous 1",]

plot.frame.temp$state[plot.frame.temp$state=="resp para 1"]<-"Respiratory Tract"
plot.frame.temp$state[plot.frame.temp$state=="para mucous 1"]<-"Facial Mucosal Membrane"

windows()
A<-ggplot(plot.frame.temp[plot.frame.temp$time<=5000,])+geom_line(aes(x=time*0.001,y=means,group=interaction(state,model),color=state,linetype=state),size=1.1)+
  geom_ribbon(aes(x=time*0.001,ymax=means+(sd*1.96/sqrt(1000)),ymin=means-(sd*1.96/sqrt(1000)),group=interaction(state,model),fill=state),alpha=0.3)+
  #scale_y_continuous(trans="log10")+
  facet_wrap(~model,scales="free")+
  scale_x_continuous(name="Time (Minutes)")+theme_pubr()+
  scale_y_continuous(name="Average Number of Viral Particles")+
  scale_fill_manual(name="",values=c("#999999", "#E69F00"))+
  scale_color_manual(name="",values=c("#999999", "#E69F00"))+
  scale_linetype_discrete(name="")+
  theme(axis.text = element_text(size=18),axis.title=element_text(size=18),
        legend.title=element_text(size=20),legend.text=element_text(size=20),
        strip.text=element_text(size=20))
A

plot.frame.temp<-plot.frame[plot.frame$state=="para hands 1" | plot.frame$state=="radio",]
plot.frame.temp$state[plot.frame.temp$state=="para hands 1"]<-"First Responder's Hands"
plot.frame.temp$state[plot.frame.temp$state=="radio"]<-"Radio"
windows()
A<-ggplot(plot.frame.temp[plot.frame.temp$time<=1600 & plot.frame.temp$model=="No one Masked",])+geom_line(aes(x=time*0.01,y=means,group=state,color=state),size=1.1)+
  geom_ribbon(aes(x=time*0.01,ymax=means+(sd*1.96/sqrt(1000)),ymin=means-(sd*1.96/sqrt(1000)),group=state,fill=state),alpha=0.3)+
  scale_x_continuous(name="Time (Minutes)",trans="log10")+theme_pubr()+
  scale_y_continuous(name="Viral Particles",trans="log10")+
  scale_fill_manual(name="",values=c("#999999", "#E69F00"))+
  scale_color_manual(name="",values=c("#999999", "#E69F00"))+
  scale_linetype_discrete(name="")+
  theme(axis.text = element_text(size=18),axis.title=element_text(size=18),
        legend.title=element_text(size=20),legend.text=element_text(size=20))
A

plot.frame.temp<-plot.frame[plot.frame$state=="para mucous 1" | plot.frame$state=="para mucous 2" |
                              plot.frame$state=="resp para 1" | plot.frame$state=="resp para 2" |
                              plot.frame$state=="room air" | plot.frame$state=="loss of viability",]

windows()
A<-ggplot(plot.frame.temp)+geom_line(aes(x=time,y=means,group=interaction(state,model),linetype=model))+
  geom_ribbon(aes(x=time,ymax=means+(sd*1.96/sqrt(1000)),ymin=means-(sd*1.96/sqrt(1000)),group=interaction(state,model),fill=model),alpha=0.3)+
  #scale_y_continuous(trans="log10")+
  facet_wrap(~state,scales="free")+
  scale_x_continuous(trans="log10")+theme_pubr()+
  theme(axis.text = element_text(size=18),axis.title=element_text(size=18),
        legend.title=element_text(size=20),legend.text=element_text(size=20),
        strip.text=element_text(size=20))
A

plot.frame<-airborneonly_1000_000000_0_001000_plot_frame
plot.frame.temp<-plot.frame[!is.na(plot.frame$sd) & !is.na(plot.frame$means),]
timestep<-0.001


#para 1 doses, #para 2 doses and percentages attributable to aerosols vs fomites
fracmucous1.model1<-plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Patient mask Only"]/(plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Patient mask Only"]+plot.frame.temp$means[plot.frame.temp$state=="resp para 1"& plot.frame.temp$model=="Patient mask Only"])
fracresp1.model1<-plot.frame.temp$means[plot.frame.temp$state=="resp para 1" & plot.frame.temp$model=="Patient mask Only"]/(plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Patient mask Only"]+plot.frame.temp$means[plot.frame.temp$state=="resp para 1" & plot.frame.temp$model=="Patient mask Only"])

fracmucous1.model2<-plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="No one Masked"]/(plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="No one Masked"]+plot.frame.temp$means[plot.frame.temp$state=="resp para 1"& plot.frame.temp$model=="No one Masked"])
fracresp1.model2<-plot.frame.temp$means[plot.frame.temp$state=="resp para 1" & plot.frame.temp$model=="No one Masked"]/(plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="No one Masked"]+plot.frame.temp$means[plot.frame.temp$state=="resp para 1" & plot.frame.temp$model=="No one Masked"])

fracmucous1.model3<-plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Paramedics Masked Only"]/(plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Paramedics Masked Only"]+plot.frame.temp$means[plot.frame.temp$state=="resp para 1"& plot.frame.temp$model=="Paramedics Masked Only"])
fracresp1.model3<-plot.frame.temp$means[plot.frame.temp$state=="resp para 1" & plot.frame.temp$model=="Paramedics Masked Only"]/(plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Paramedics Masked Only"]+plot.frame.temp$means[plot.frame.temp$state=="resp para 1" & plot.frame.temp$model=="Paramedics Masked Only"])

fracmucous1.model4<-plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Patient and Paramedics Masked"]/(plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Patient and Paramedics Masked"]+plot.frame.temp$means[plot.frame.temp$state=="resp para 1"& plot.frame.temp$model=="Patient and Paramedics Masked"])
fracresp1.model4<-plot.frame.temp$means[plot.frame.temp$state=="resp para 1" & plot.frame.temp$model=="Patient and Paramedics Masked"]/(plot.frame.temp$means[plot.frame.temp$state=="para mucous 1" & plot.frame.temp$model=="Patient and Paramedics Masked"]+plot.frame.temp$means[plot.frame.temp$state=="resp para 1" & plot.frame.temp$model=="Patient and Paramedics Masked"])


frame.temp.2<-data.frame(fraction=c(fracmucous1.model1,fracmucous1.model2,fracmucous1.model3,fracmucous1.model4,
                                    fracresp1.model1,fracresp1.model2,fracresp1.model3,fracresp1.model4),
                         type=c(rep("Mucous Membrane",length(fracmucous1.model1)*4),
                                rep("Respiratory Tract",length(fracresp1.model1)*4)),
                         time=rep(1:length(fracmucous1.model1),8),
                         model=rep(c(rep("Patient Respirator Only",length(fracmucous1.model1)),
                                     rep("No One with Respirators",length(fracmucous1.model2)),
                                     rep("First Responder Respirators Only",length(fracmucous1.model3)),
                                     rep("Patient and First Responder Respirator",length(fracmucous1.model4))),2))


windows()
ggplot(frame.temp.2[frame.temp.2$time>2 & frame.temp.2$time<=1600,],aes(x=time*timestep,y=fraction,fill=type))+geom_bar(stat="identity",alpha=0.5)+facet_wrap(~model)+
  scale_x_continuous(name="Time (Minutes)")+
  scale_y_continuous(name="Fraction of Dose")+
  scale_fill_manual(name="Type of Dose",values=c("#999999", "#56B4E9"))+
  theme_pubr()+
  theme(axis.text = element_text(size=18),axis.title=element_text(size=18),
        legend.title=element_text(size=20),legend.text=element_text(size=20),
        strip.text=element_text(size=20))

frame.temp.2$fraction[frame.temp.2$time==16000 & frame.temp.2$type=="Respiratory Tract" & frame.temp.2$model=="No One with Respirators"]
frame.temp.2$fraction[frame.temp.2$time==16000 & frame.temp.2$type=="Mucous Membrane" & frame.temp.2$model=="No One with Respirators"]

frame.temp.2$fraction[frame.temp.2$time==16000 & frame.temp.2$type=="Respiratory Tract" & frame.temp.2$model=="Patient Respirator Only"]
frame.temp.2$fraction[frame.temp.2$time==16000 & frame.temp.2$type=="Mucous Membrane" & frame.temp.2$model=="Patient Respirator Only"]

frame.temp.2$fraction[frame.temp.2$time==16000 & frame.temp.2$type=="Respiratory Tract" & frame.temp.2$model=="First Responder Respirators Only"]
frame.temp.2$fraction[frame.temp.2$time==16000 & frame.temp.2$type=="Mucous Membrane" & frame.temp.2$model=="First Responder Respirators Only"]

frame.temp.2$fraction[frame.temp.2$time==16000 & frame.temp.2$type=="Respiratory Tract" & frame.temp.2$model=="Patient and First Responder Respirator"]
frame.temp.2$fraction[frame.temp.2$time==16000 & frame.temp.2$type=="Mucous Membrane" & frame.temp.2$model=="Patient and First Responder Respirator"]


#-------------- looking at infection risks ----------------------------------------

#first import rds for allsenario files

allscenario.airborneonly <- readRDS("~/EMS_v2/airborneonly/allscenario.airborneonly.1000.000000.0.001000.plot.frame.rds")
allscenario.surfonly <- readRDS("~/EMS_v2/surfonly/allscenario.surfonly.1000.000000.0.001000.plot.frame.rds")

scenario.all<-rep(NA,4000)
infect.surf<-rep(NA,4000)
infect.airborne<-rep(NA,4000)

surf.conc.sum<-rep(NA,4000)
starting.conc.sum<-rep(NA,4000)

for (i in 1:4000){
  if(i<=1000){
    scenario<-"C. Patient Respirator Only"
  }else if (i>1000 & i<=2000){
    scenario<-"A. No One with Respirators"
  }else if (i>2000 & i<=3000){
    scenario<-"B. First Responder Respirators Only"
  }else{
    scenario<-"D. Patient and First Responder Respirator"
  }
  
  tempframe.surf<-allscenario.surfonly[[i]]
  tempframe.airborne<-allscenario.airborneonly[[i]]
  
  scenario.all[i]<-scenario
  infect.surf[i]<-tempframe.surf$infect1[1]
  infect.airborne[i]<-tempframe.airborne$infect1[1]
  finaltime<-length(tempframe.airborne$state1)
  surf.conc.sum[i]<-tempframe.airborne$state3[finaltime]+tempframe.airborne$state4[finaltime]+tempframe.airborne$state5[finaltime]+
    tempframe.airborne$state6[finaltime]+tempframe.airborne$state7[finaltime]+tempframe.airborne$state8[finaltime]
  starting.conc.sum[i]<-tempframe.surf$state3[1]+tempframe.surf$state4[1]+tempframe.surf$state5[1]+
    tempframe.surf$state6[1]+tempframe.surf$state7[1]+tempframe.surf$state8[1]
}

deposition.check<-data.frame(scenario=scenario.all,infect=infect.surf,surfdepo=surf.conc.sum,starting.conc.sum=starting.conc.sum)

windows()
ggplot(deposition.check)+
  geom_violin(aes(x=scenario,y=starting.conc.sum,group=scenario,fill=scenario),draw_quantiles = c(0.25,0.5,0.75),alpha=0.5)+
  scale_y_continuous(trans="log10",name="Average Total Number of Viral Particles on Surfaces")+
  scale_x_discrete(name="",labels=c("","","",""))+
  scale_fill_manual(name="",values=c("#999999", "#E69F00", "#56B4E9","#CC6666"))+
  theme_pubr()+
  theme(axis.title = element_text(size=20), axis.text=element_text(size=20),
        legend.text= element_text(size=20),strip.text = element_text(size=20),legend.position = "right",
        axis.ticks.x=element_blank())
  

ggplot(deposition.check)+geom_violin(aes(x=scenario,y=infect.surf,group=scenario,fill=scenario))+
  scale_y_continuous(trans="log10")

infect.frame<-data.frame(scenario=rep(scenario.all,2),infect=c(infect.airborne,infect.surf),
                         model=c(rep("Scenario 1",4000),rep("Scenario 2",4000)))
infect.frame$scenario<-factor(infect.frame$scenario,levels=c("A. No One with Respirators", "B. First Responder Respirators Only","C. Patient Respirator Only",
                                "D. Patient and First Responder Respirator"))
windows()
ggplot(infect.frame)+geom_violin(aes(x=scenario,y=infect,group=scenario,fill=scenario),draw_quantiles = c(0.25,0.5,0.75),alpha=0.5)+
  scale_y_continuous(trans="log10",name="Infection Risk")+facet_wrap(~model)+
  scale_x_discrete(name="",labels=c("","","",""))+
  scale_fill_manual(name="",values=c("#999999", "#E69F00", "#56B4E9","#CC6666"))+
  theme_pubr()+
  theme(axis.title = element_text(size=20), axis.text=element_text(size=20),
        legend.text= element_text(size=20),strip.text = element_text(size=20),legend.position = "right",
        axis.ticks.x=element_blank())

#scenario 1-------------------------------------------------------------------------------------------------------------------

#% changes in infection risk 
baseline<-mean(infect.frame$infect[infect.frame$model=="Scenario 1" & infect.frame$scenario=="A. No One with Respirators" ])

summary(infect.frame$infect[infect.frame$model=="Scenario 1" & infect.frame$scenario=="A. No One with Respirators" ])
sd(infect.frame$infect[infect.frame$model=="Scenario 1" & infect.frame$scenario=="A. No One with Respirators" ])

#First responder w/ respirator
int.1<-mean(infect.frame$infect[infect.frame$model=="Scenario 1" & infect.frame$scenario=="B. First Responder Respirators Only" ])

(baseline-int.1)/baseline*100

#Patient w/ respirator
int.2<-mean(infect.frame$infect[infect.frame$model=="Scenario 1" & infect.frame$scenario=="C. Patient Respirator Only" ])

(baseline-int.2)/baseline*100

#Both w/ respirator
int.3<-mean(infect.frame$infect[infect.frame$model=="Scenario 1" & infect.frame$scenario=="D. Patient and First Responder Respirator" ])

summary(infect.frame$infect[infect.frame$model=="Scenario 1" & infect.frame$scenario=="D. Patient and First Responder Respirator" ])
sd(infect.frame$infect[infect.frame$model=="Scenario 1" & infect.frame$scenario=="D. Patient and First Responder Respirator" ])

(baseline-int.3)/baseline*100

#scenario 2--------------------------------------------------------------------------------------------------------------------

#% changes in infection risk 
baseline<-mean(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="A. No One with Respirators" ])

summary(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="A. No One with Respirators" ])
sd(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="A. No One with Respirators" ])

summary(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="No One with Respirators" ])
sd(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="No One with Respirators" ])

#First responder w/ respirator
int.1<-mean(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="B. First Responder Respirators Only" ])

(baseline-int.1)/baseline*100

#Patient w/ respirator
int.2<-mean(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="C. Patient Respirator Only" ])

(baseline-int.2)/baseline*100

#Both w/ respirator
int.3<-mean(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="D. Patient and First Responder Respirator" ])

(baseline-int.3)/baseline*100


summary(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="D. Patient and First Responder Respirator" ])
sd(infect.frame$infect[infect.frame$model=="Scenario 2" & infect.frame$scenario=="D. Patient and First Responder Respirator" ])




