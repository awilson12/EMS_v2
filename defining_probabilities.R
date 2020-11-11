


#here since all transitions are in units of mi^-1, delta T = 1 min

# ROW 1------------------------------------------------------------------------------------

#All transitions away
lambdas.1<-c(lambda.1.2[l],lambda.1.3[l],lambda.1.4[l],lambda.1.5[l],lambda.1.6[l],lambda.1.7[l],
             lambda.1.8[l],lambda.1.9[l],lambda.1.10[l],lambda.1.11[l],0,0,0,0,lambda.1.16[l])

lambda.1.T<-sum(lambdas.1)

P.1.1<-exp(-lambda.1.T)

P.1<-(1-P.1.1)*(lambdas.1/lambda.1.T)

P.1.total<-c(P.1.1,P.1)


#ROW 2----------------------------------------------------------------------------------------

P.2.total<-c(0,1,rep(0,14))

#ROW 3----------------------------------------------------------------------------------------

lambdas.3<-c(rep(0,11),lambda.3.12[l],lambda.3.13[l],rep(0,2),lambda.3.16[l])
             
lambda.3.T<-sum(lambdas.3)

P.3.3<-exp(-lambda.3.T)

P.3<-(1-P.3.3)*(lambdas.3/lambda.3.T)

P.3.total<-c(P.3[1:2],P.3.3,P.3[4:16])

#ROW 4----------------------------------------------------------------------------------------

lambdas.4<-c(rep(0,11),lambda.4.12[l],lambda.4.13[l],rep(0,2),lambda.4.16[l])

lambda.4.T<-sum(lambdas.4)

P.4.4<-exp(-lambda.4.T)

P.4<-(1-P.4.4)*(lambdas.4/lambda.4.T)

P.4.total<-c(P.4[1:3],P.4.4,P.4[5:16])

#ROW 5------------------------------------------------------------------------------------

lambdas.5<-c(rep(0,11),lambda.5.12[l],lambda.5.13[l],rep(0,2),lambda.5.16[l])

lambda.5.T<-sum(lambdas.5)

P.5.5<-exp(-lambda.5.T)

P.5<-(1-P.5.5)*(lambdas.5/lambda.5.T)

P.5.total<-c(P.5[1:4],P.5.5,P.5[6:16])

#ROW 6------------------------------------------------------------------------------------

lambdas.6<-c(rep(0,11),lambda.6.12[l],lambda.6.13[l],rep(0,2),lambda.6.16[l])

lambda.6.T<-sum(lambdas.6)

P.6.6<-exp(-lambda.6.T)

P.6<-(1-P.6.6)*(lambdas.6/lambda.6.T)

P.6.total<-c(P.6[1:5],P.6.6,P.6[7:16])

#ROW 7------------------------------------------------------------------------------------

lambdas.7<-c(rep(0,11),lambda.7.12[l],lambda.7.13[l],rep(0,2),lambda.7.16[l])

lambda.7.T<-sum(lambdas.7)

P.7.7<-exp(-lambda.7.T)

P.7<-(1-P.7.7)*(lambdas.7/lambda.7.T)

P.7.total<-c(P.7[1:6],P.7.7,P.7[8:16])

#ROW 8------------------------------------------------------------------------------------

lambdas.8<-c(rep(0,11),lambda.8.12[l],lambda.8.13[l],rep(0,2),lambda.8.16[l])

lambda.8.T<-sum(lambdas.8)

P.8.8<-exp(-lambda.8.T)

P.8<-(1-P.8.8)*(lambdas.8/lambda.8.T)

P.8.total<-c(P.8[1:7],P.8.8,P.8[9:16])

#ROWS 9 THROUGH 11 ARE ABSORBING----------------------------------------------

P.9.total<-c(rep(0,8),1,rep(0,7))
P.10.total<-c(rep(0,9),1,rep(0,6))
P.11.total<-c(rep(0,10),1,rep(0,5))

#ROW 12----------------------------------------------------------------------

lambdas.12<-c(rep(0,2),lambda.12.3[l],lambda.12.4[l],lambda.12.5[l],lambda.12.6[l],
              lambda.12.7[l],lambda.12.8[l],rep(0,5),lambda.12.14[l],0,lambda.12.16[l])

lambda.12.T<-sum(lambdas.12)

P.12.12<-exp(-lambda.12.T)

P.12<-(1-P.12.12)*(lambdas.12/lambda.12.T)

P.12.total<-c(P.12[1:11],P.12.12,P.12[13:16])

#ROW 13------------------------------------------------------------------------

lambdas.13<-c(rep(0,2),lambda.13.3[l],lambda.13.4[l],lambda.13.5[l],lambda.13.6[l],
              lambda.13.7[l],lambda.13.8[l],rep(0,6),lambda.13.15[l],lambda.13.16[l])

lambda.13.T<-sum(lambdas.13)

P.13.13<-exp(-lambda.13.T)

P.13<-(1-P.13.13)*(lambdas.13/lambda.13.T)

P.13.total<-c(P.13[1:12],P.13.13,P.13[14:16])

#ROWS 14-16 absorbing so no transitions away------------------------------------

P.14.total<-c(rep(0,13),1,rep(0,2))
P.15.total<-c(rep(0,14),1,0)
P.16.total<-c(rep(0,15),1)


#probs<-matrix(nrow=16,ncol=16)
#probs[1,]<-P.1.total
#probs[2,]<-P.2.total
#probs[3,]<-P.3.total
#probs[4,]<-P.4.total
#probs[5,]<-P.5.total
#probs[6,]<-P.6.total
#probs[7,]<-P.7.total
#probs[8,]<-P.8.total
#probs[9,]<-P.9.total
#probs[10,]<-P.10.total
#probs[11,]<-P.11.total
#probs[12,]<-P.12.total
#probs[13,]<-P.13.total
#probs[14,]<-P.14.total
#probs[15,]<-P.15.total
#probs[16,]<-P.16.total

#states<-c("room air","exhaust",
#          "glucometer","headphones","jumpbag","keyboard","touchscreen","radio",
#          "resp para 1","resp para 2","patient","para hands 1","para hands 2",
#          "para mucous 1","para mucous 2","loss of viability")
#colnames(probs)<-states
#rownames(probs)<-states

#require(reshape2)
#require(tidyverse)
#require(plotly)

#probs<-melt(probs)


#have to rerun melt before new plot
#a<-plot_ly(probs, y = ~Var1, x = ~Var2, z = ~value, colors= "Blues", colorbar=list(
#  title='Probability'
#)) %>%
#  add_heatmap()%>%
#  colorbar(limits = c(0, 1))%>%
#  layout(xaxis = list(title="To"), yaxis = list(title="From"))

#a




