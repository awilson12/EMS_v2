min.point<-13
max.point<-232


#relating min and max sars-cov-2 particles/cm^2 on surfaces needed to result in risks in scenario 2 on par with risks in scenario 1

#droplets per size range per cough...
#divide max diameter by 2 and conver to cm, so we get volume
#in cm^3, or mL to relate to viral particles/mL

#100 to 125 um 
bin1<-27
m1<-(4/3)*pi*((125*10^-4)/2)^3

#125 to 150 um
bin2<-32
m2<-(4/3)*pi*((150*10^-4)/2)^3

#150 to 200 um
bin3<-30
m3<-(4/3)*pi*((200*10^-4)/2)^3

#200 to 250 um
bin4<-83
m4<-(4/3)*pi*((250*10^-4)/2)^3

#250 to 500 um
bin5<-47
m5<-(4/3)*pi*((500*10^-4)/2)^3

#500 to 1000 um
bin6<-40
m6<-(4/3)*pi*((1000*10^-4)/2)^3

#1000 to 2000 um
bin7<-27
m7<-(4/3)*pi*((2000*10^-4)/2)^3

#mL per cough
total.vol<-(bin1*m1)+(bin2*m2)+(bin3*m3)+(bin4*m4)+(bin5*m5)+(bin6*m6)+(bin7*m7)

#viral particles per cough to land on surfaces
virus.ml<-10000 #already adjusted for % of genome copies we assume relates to infective virus

viral.surf<-total.vol*virus.ml

numcough<-4 #assumes 20 min ride and 12 coughs per hour

viral.surf.ride<-viral.surf*numcough

numsurf<-6 #6 surfaces in this model

#viral partices on surface for total # of coughs (%)
min.frac<-(min.point*numsurf)/viral.surf.ride*100
max.frac<-(max.point*numsurf)/viral.surf.ride*100