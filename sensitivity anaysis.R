#run figures.R file first
#load in all params files


allparams.surfonly <- readRDS("~/EMS_v2/surfonly/allparams.surfonly.1000.000000.0.001000.plot.frame.rds")
allparams.airborneonly <- readRDS("~/EMS_v2/airborneonly/allparams.airborneonly.1000.000000.0.001000.plot.frame.rds")

#(must run figures.R file first as this sets up infect.surf and infect.airborne)
allparams.surfonly$loginfect<-log10(infect.surf)
allparams.airborneonly$loginfect<-log10(infect.airborne)

#------------airborneonly

airborne.cor<-signif(cor(allparams.airborneonly[3001:4000,], method = c("spearman")),2)
View(airborne.cor[,22])

#patients wear respirators
airborne.cor<-signif(cor(allparams.airborneonly[0:1000,], method = c("spearman")),2)
View(airborne.cor[,22])

#------------surfonly

#no one wears respirators
surf.cor<-signif(cor(allparams.surfonly[1001:2000,], method = c("spearman")),2)
View(surf.cor[,22])

#paramedics wear respirators
surf.cor<-signif(cor(allparams.surfonly[2001:3000,], method = c("spearman")),2)
View(surf.cor[,22])

surf.cor<-signif(cor(allparams.surfonly[3001:4000,], method = c("spearman")),2)
View(surf.cor[,22])

allparams.surfonly<-allparams.surfonly[3001:4000,]
allparams.airborneonly<-allparams.airborneonly[3001:4000,]
allparams.surfonly$type<-"surfonly"
allparams.airborneonly$type<-"airborneonly"
all<-rbind(allparams.surfonly,allparams.airborneonly)

A<-ggplot(all)+geom_point(aes(x=TE.HS,y=loginfect,color=type))
B<-ggplot(all)+geom_point(aes(x=TE.SH,y=loginfect,color=type))
C<-ggplot(all)+geom_point(aes(x=TE.HF,y=loginfect,color=type))
D<-ggplot(all)+geom_point(aes(x=H.surf,y=loginfect,color=type))
E<-ggplot(all)+geom_point(aes(x=H.mask,y=loginfect,color=type))
G<-ggplot(all)+geom_point(aes(x=S.H,y=loginfect,color=type))
H<-ggplot(all)+geom_point(aes(x=S.F,y=loginfect,color=type))
I<-ggplot(all)+geom_point(aes(x=A.hand,y=loginfect,color=type))
J<-ggplot(all)+geom_point(aes(x=inactiv.fome,y=loginfect,color=type))
K<-ggplot(all)+geom_point(aes(x=inactiv.air,y=loginfect,color=type))
L<-ggplot(all)+geom_point(aes(x=inactiv.hands,y=loginfect,color=type))
M<-ggplot(all)+geom_point(aes(x=I,y=loginfect,color=type))
N<-ggplot(all)+geom_point(aes(x=M.patient.sourcecontrol,y=loginfect,color=type))
O<-ggplot(all)+geom_point(aes(x=M.EMS,y=loginfect,color=type))
P<-ggplot(all)+geom_point(aes(x=AER,y=loginfect,color=type))
Q<-ggplot(all)+geom_point(aes(x=alpha,y=loginfect,color=type))
R<-ggplot(all)+geom_point(aes(x=beta,y=loginfect,color=type))

windows()
ggarrange(A,B,C,D,
          E,G,H,I,nrow=2,ncol=4,common.legend=TRUE)
          
windows()
ggarrange(J,K,L,M,
          N,O,P,Q,R,nrow=3,ncol=4,common.legend = TRUE)
