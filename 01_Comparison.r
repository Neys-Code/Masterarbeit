############################################
#
#Comparison
#
###############################################
#read list
#my libary #set right path!
source("E:\\Dropbox\\Dropbox\\Masterarbeit\\02_R-Code\\abgabe\\lib.r")

# load library
library(readr)
library(caret)
library(e1071)

pfad<-("C:/Users/jana/Dropbox/Masterarbeit/Daten_Kal")#select specific path
#pfad<-("E:/Dropbox/Dropbox/Masterarbeit/Daten_Kal")

setwd(pfad)# set working directory
# list files
f<-list.files(path = pfad, pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# create all matrices and variables to be filled within loop 
n=c()
i=1
liste=c()
normal<-mittel<-median<-min<-max<-sd<-var<-rse<-matrix(rep(0,times=length(f)),ncol=4,nrow =length(f) )
colnames(normal)<-colnames(max)<-colnames(mittel)<-colnames(median)<-colnames(min)<-colnames(max)<-colnames(sd)<-colnames(var)<-colnames(rse)<-c("PM10","PM5","Temp","Hum")
rownames(normal)<-rownames(max)<-rownames(mittel)<-rownames(median)<-rownames(min)<-rownames(max)<-rownames(sd)<-rownames(var)<-rownames(rse)<-f

cubic90<-linear90<-linear<-pearson<-spearman<-cubic<-cubic90 <-matrix(rep(0,times=length(f)),ncol=6,nrow =20)
colnames(linear90)<-colnames(cubic90)<-colnames(cubic)<-colnames(linear)<-rownames(pearson)<-rownames(spearman)<-c("Hum_PM10","Hum_PM5","PM5_PM10","Temp_PM10","Temp_PM5","Temp_Hum")
rownames(linear90)<-rownames(cubic90)<-rownames(cubic)<-rownames(linear)<-colnames(pearson)<-colnames(spearman)<-f

#loop to read all data and perform several statistical operations 
for (i in 1:20 )
{
  y=j=k=1 # reset counter Variables 
  Date=Time=Hum=PM5=PM10=Temp=c() # reset help Variables
  
  # e = list to be iterated
  e<-list.files(path = paste0(pfad,"/",f[i]), pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  for (k in 1:length(e))
  { y=1
  n<-e[k]
  # list to be iterated
  l<-list.files(path = paste0(pfad,"/",f[i],"/",e[k]), pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #reset "inner" help variables
  Temp2<-c()
  Date2=Time4=Hum2=PM52=PM102=c()
  
  #inner Loop
  for (y in 1:length(l))
  {j=1
  x<-read_delim(paste0(pfad,"/",f[i],"/",e[k],"/",l[y]),";", escape_double = FALSE, trim_ws = TRUE, na = c("", "NA"),quoted_na = TRUE)
  
  if (is.na(x$BMP_temperature[1])==FALSE)
  {x$Temp<-x$BMP_temperature
  x$Humidity<-x$BMP_pressure}
  Time2=mapply(motj,c(x$Time),  USE.NAMES = FALSE)#minuites as decimal
  x=cbind(x,Time2)
  rm(Time2)
  
  Temp2<-c(Temp2,x$Temp)
  PM102=c(PM102,x$SDS_P1)
  PM52=c(PM52,x$SDS_P2)
  Hum2=c(Hum2,x$Humidity)
  Time4=c(Time4,x$Time2)
  Date2<-c(Date2,x$Time)
  }
  
  Temp<-c(Temp,Temp2)
  PM10=c(PM10,PM102)
  PM5=c(PM5,PM52)
  Hum=c(Hum,Hum2)
  Time=c(Time,Time4)
  Date=c(Date,Date2)
  }
  
  PM10<-round(PM10,digits=2)
  PM5<-round(PM5,digits=2)
  Temp<-round(Temp,digits=2)
  Hum<-round(Hum,digits=2)
  
  #putting all variables together
  Sensor<-data.frame(Time,PM10,PM5,Temp,Hum,Date)
  
  #calculate different Statistics and put them together in a matrix
  mittel[i,1]<-round(mean(Sensor$PM10,na.rm = TRUE),digits = 2)
  mittel[i,2]<-round(mean(Sensor$PM5,na.rm = TRUE),digits = 2)
  mittel[i,3]<-round(mean(Sensor$Temp,na.rm = TRUE),digits = 2)
  mittel[i,4]<-round(mean(Sensor$Hum,na.rm = TRUE),digits = 2)
  
  median[i,1]<-round(median(Sensor$PM10,na.rm = TRUE),digits = 2)
  median[i,2]<-round(median(Sensor$PM5,na.rm = TRUE),digits = 2)
  median[i,3]<-round(median(Sensor$Temp,na.rm = TRUE),digits = 2)
  median[i,4]<-round(median(Sensor$Hum,na.rm = TRUE),digits = 2)
  
  min[i,1]<-round(min(Sensor$PM10,na.rm = TRUE),digits = 2)
  min[i,2]<-round(min(Sensor$PM5,na.rm = TRUE),digits = 2)
  min[i,3]<-round(min(Sensor$Temp,na.rm = TRUE),digits = 2)
  min[i,4]<-round(min(Sensor$Hum,na.rm = TRUE),digits = 2)
  
  max[i,1]<-round(max(Sensor$PM10,na.rm = TRUE),digits = 2)
  max[i,2]<-round(max(Sensor$PM5,na.rm = TRUE),digits = 2)
  max[i,3]<-round(max(Sensor$Temp,na.rm = TRUE),digits = 2)
  max[i,4]<-round(max(Sensor$Hum,na.rm = TRUE),digits = 2)  
  
  sd[i,1]<-round(sd(Sensor$PM10,na.rm = TRUE),digits = 2)
  sd[i,2]<-round(sd(Sensor$PM5,na.rm = TRUE),digits = 2)
  sd[i,3]<-round(sd(Sensor$Temp,na.rm = TRUE),digits = 2)
  sd[i,4]<-round(sd(Sensor$Hum,na.rm = TRUE),digits = 2)  
  
  var[i,1]<-round(var(Sensor$PM10,na.rm = TRUE),digits = 2)
  var[i,2]<-round(var(Sensor$PM5,na.rm = TRUE),digits = 2)
  var[i,3]<-round(var(Sensor$Temp,na.rm = TRUE),digits = 2)
  var[i,4]<-round(var(Sensor$Hum,na.rm = TRUE),digits = 2)  
  
  #relative standart error
  rse[i,1]<-round((sd(Sensor$PM10,na.rm = TRUE)/sqrt(length(c(sd(Sensor$PM10,na.rm=TRUE)))))/mean(Sensor$PM10,na.rm = TRUE),digits = 2)
  rse[i,2]<-round((sd(Sensor$PM5,na.rm = TRUE)/sqrt(length(c(sd(Sensor$PM5,na.rm=TRUE)))))/mean(Sensor$PM5,na.rm = TRUE),digits = 2)
  rse[i,3]<-round((sd(Sensor$Temp,na.rm = TRUE)/sqrt(length(c(sd(Sensor$Temp,na.rm=TRUE)))))/mean(Sensor$Temp,na.rm = TRUE),digits = 2)
  rse[i,4]<-round((sd(Sensor$Hum,na.rm = TRUE) /sqrt(length(c(sd(Sensor$Hum,na.rm=TRUE)))))/mean(Sensor$Hum,na.rm = TRUE),digits = 2)
  
  
  # Spearmans Rho
  spearman[i,1]<-cor.test(Sensor$Hum,Sensor$PM10,method="spearman")[["estimate"]]
  spearman[i,2]<-cor.test(Sensor$Hum,Sensor$PM5,method="spearman")[["estimate"]]
  spearman[i,3]<-cor.test(Sensor$PM5,Sensor$PM10,method="spearman")[["estimate"]]
  spearman[i,4]<-cor.test(Sensor$Temp,Sensor$PM10,method="spearman")[["estimate"]]
  spearman[i,5]<-cor.test(Sensor$Temp,Sensor$PM5,method="spearman")[["estimate"]]
  spearman[i,6]<-cor.test(Sensor$Hum,Sensor$Temp,method="spearman")[["estimate"]]
  
  # pearsons Rho
  pearson[i,1]<-cor.test(Sensor$Hum,Sensor$PM10,method="pearson")[["estimate"]]
  pearson[i,2]<-cor.test(Sensor$Hum,Sensor$PM5,method="pearson")[["estimate"]]
  pearson[i,3]<-cor.test(Sensor$PM5,Sensor$PM10,method="pearson")[["estimate"]]
  pearson[i,4]<-cor.test(Sensor$Temp,Sensor$PM10,method="pearson")[["estimate"]]
  pearson[i,5]<-cor.test(Sensor$Temp,Sensor$PM5,method="pearson")[["estimate"]]
  pearson[i,6]<-cor.test(Sensor$Hum,Sensor$Temp,method="pearson")[["estimate"]]
  
  # adjusted r squared 
  cubic[i,1]<-summary(lm(Sensor$Hum ~ poly(Sensor$PM10, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,2]<-summary(lm(Sensor$Hum ~ poly(Sensor$PM5, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,3]<-summary(lm(Sensor$PM5 ~ poly(Sensor$PM10, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,4]<-summary(lm(Sensor$Temp ~ poly(Sensor$PM10, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,5]<-summary(lm(Sensor$Temp ~ poly(Sensor$PM5, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,6]<-summary(lm(Sensor$Hum ~ poly(Sensor$Temp, 3, raw=TRUE)))[["adj.r.squared"]]
  
  linear[i,1]<-summary(lm(Sensor$Hum ~ Sensor$PM10))[["adj.r.squared"]]
  linear[i,2]<-summary(lm(Sensor$Hum ~ Sensor$PM5))[["adj.r.squared"]]
  linear[i,3]<-summary(lm(Sensor$PM5 ~ Sensor$PM10))[["adj.r.squared"]]
  linear[i,4]<-summary(lm(Sensor$Temp ~ Sensor$PM10))[["adj.r.squared"]]
  linear[i,5]<-summary(lm(Sensor$Temp~Sensor$PM5))[["adj.r.squared"]]
  linear[i,6]<-summary(lm(Sensor$Hum ~ Sensor$Temp))[["adj.r.squared"]]
  
  
  #write Variable with Sensor ID and number
  assign(paste0("Sensor",substr (f[i],1,10)),Sensor)
  liste<-c(liste,(paste0("Sensor",substr (f[i],1,10))))
}

cubic<-round(cubic,digits=2)
linear<-round(linear,digits=2)
pearson<-round(pearson,digits=2)
spearman<-round(spearman,digits=2)
##################################################
#
#datamerging
#
#####################################################
########## Stundenwerte
liste2<-c()
#reclassify as hourly values with mean
for (i in 1:length(liste))
{ 
  s<-get(liste[i])
  result<-Stundenwerte(s)
  assign(paste0("Stunde",liste[i]), result)
  liste2<-rbind(liste2,paste0("Stunde",liste[i]))
} 

#create dummy for all variables
dummy<-data.frame(`StundeSensor6-7134028`$Date,`StundeSensor6-7134028`$PM10)
names(dummy)<-c("Date","dummy")
PM10<-PM5<-Hum<-Temp<-data.frame(dummy$Date,dummy$dummy)
names(PM10)<-c("Date","dummy")
names(PM5)<-c("Date","dummy")
names(Hum)<-c("Date","dummy")
names(Temp)<-c("Date","dummy")

#merging  Data
liste2<-na.omit(liste2)
liste<-na.omit(liste)
for (i in 1:length(liste2))
{ 
  s<-get(liste2[i])
  PM10<-merge(x=PM10 , y= s[1:2], by= 'Date', all.x= T)
  PM5<-merge(x=PM5 , y=s[,c(1,3)] , by= 'Date', all.x= T)
  Temp<-merge(x= Temp , y=s[,c(1,4)], by= 'Date', all.x= T)
  Hum<-merge(x=Hum , y=s[,c(1,5)] , by= 'Date', all.x= T)
  names(PM10)<-c("Date","dummy",liste[1:i])
  names(PM5)<-c("Date","dummy",liste[1:i])
  names(Temp)<-c("Date","dummy",liste[1:i])
  names(Hum)<-c("Date","dummy",liste[1:i])
} 

PM10<-PM10[-c(2)]
PM5<-PM5[-c(2)]
Temp<-Temp[-c(2)]
Hum<-Hum[-c(2)]


##########################################
# overall mean 
##########################################

#overall mean of every hour over all Sensors 
RMPM10<-rowMeans(PM10[-c(1)], na.rm=TRUE)
RMPM5<-rowMeans(PM5[-c(1)], na.rm=TRUE)
RMHum<-rowMeans(Hum[-c(1)], na.rm=TRUE)
RMTemp<-rowMeans(Temp[-c(1)], na.rm=TRUE)

#overall mean for every sensor

CMPM10<-colMeans(PM10[-c(1)], na.rm=TRUE)
CMPM5<-colMeans(PM5[-c(1)], na.rm=TRUE)
CMHum<-colMeans(Hum[-c(1)], na.rm=TRUE)
CMTemp<-colMeans(Temp[-c(1)], na.rm=TRUE)

windows()
plot(RMPM10~RMPM5,xlab="PM 2.5 µg/m³", ylab="PM 10 µg/m³", lwd = 1.5, main="Beziehung zwischen gemittelten PM 10 und PM 2,5 Werten",cex.axis=1.5,cex.lab=1.5)
plot(RMPM10~RMHum,xlab="Luftfeuchtigkeit %", ylab="PM 10 µg/m³", lwd = 1.5, main="Beziehung zwischen den gemittelten PM 10 Werten und der Luftfeuchtigkeit",cex.axis=1.5,cex.lab=1.5)
plot(RMPM10~RMTemp,xlab="Temperatur °C", ylab="PM 10 µg/m³", lwd = 1.5, main="Beziehung zwischen den gemittelten PM 10 Werten und Temperatur in °C",cex.axis=1.5,cex.lab=1.5)
plot(RMPM5~RMTemp,xlab="Temperatur °C", ylab="PM 2,5 µg/m³", lwd = 1.5, main="Beziehung zwischen den gemittelten PM 2,5 Werten und Temperatur in °C",cex.axis=1.5,cex.lab=1.5)
plot(RMPM5~RMHum,xlab="Luftfeuchtigkeit %", ylab="PM 2,5 µg/m³", lwd = 1.5, main="Beziehung zwischen den gemittelten PM 2,5 Werten und der Luftfeuchtigkeit",cex.axis=1.5,cex.lab=1.5)
plot(RMTemp~RMHum,xlab="Luftfeuchtigkeit %", ylab="Temperatur °C", lwd = 1.5, main="Beziehung zwischen der Temperatur und der Luftfeuchtigkeit",cex.axis=1.5,cex.lab=1.5)


#########################################################
#Dataclound for base statistics

w.Hum <-c(Hum[,2],Hum[,3],Hum[,4],Hum[,5],Hum[,6],Hum[,7],Hum[,8],Hum[,9],Hum[,10],Hum[,11],Hum[,12],Hum[,13],Hum[,14],Hum[,15],Hum[,16],Hum[,17],Hum[,18],Hum[,19],Hum[,20])
w.PM10 <-c(PM10[,2],PM10[,3],PM10[,4],PM10[,5],PM10[,6],PM10[,7],PM10[,8],PM10[,9],PM10[,10],PM10[,11],PM10[,12],PM10[,13],PM10[,14],PM10[,15],PM10[,16],PM10[,17],PM10[,18],PM10[,19],PM10[,20])
w.PM5 <-c(PM5[,2],PM5[,3],PM5[,4],PM5[,5],PM5[,6],PM5[,7],PM5[,8],PM5[,9],PM5[,10],PM5[,11],PM5[,12],PM5[,13],PM5[,14],PM5[,15],PM5[,16],PM5[,17],PM5[,18],PM5[,19],PM5[,20])
w.Temp<-c(Temp[,2],Temp[,3],Temp[,4],Temp[,5],Temp[,6],Temp[,7],Temp[,8],Temp[,9],Temp[,10],Temp[,11],Temp[,12],Temp[,13],Temp[,14],Temp[,15],Temp[,16],Temp[,17],Temp[,18],Temp[,19],Temp[,20])

#base statistics
Statistik(w.PM10)
Statistik(w.PM5)
Statistik(w.Hum)
Statistik(w.Temp)

#without Sensor 3 -[15]
w.Hum_S3 <-c(Hum[,2],Hum[,3],Hum[,4],Hum[,5],Hum[,6],Hum[,7],Hum[,8],Hum[,9],Hum[,10],Hum[,11],Hum[,12],Hum[,13],Hum[,14],Hum[,16],Hum[,17],Hum[,18],Hum[,19],Hum[,20])
Statistik(w.Hum_S3)

###################
# Check if  Measured data lies within within the given error intervall
###################
#dht 
#+-0.5°C
#+-2-5%
#sds011
#± 10µg/m³
# differnence of every sensor to the mean

#max difference between sinle Value and overall mean
mitt<-diff<-matrix(rep(0,times=length(f)),ncol=6,nrow =length(f) )
colnames(mitt)<-colnames(diff)<-c("PM10","PM5","Temp","Hum","Temperatur-Werte innerhalb der Genauigkeit","Feuchtigkeits-Werte innerhalb der Genauigkeit")
rownames(mitt)<-rownames(diff)<-f

for (i in 1:length(liste))
{ 
  s<-get(liste2[i])
  p1<-round(s$PM10-RMPM10, digits=2)
  p2<-round(s$PM5-RMPM5, digits=2)
  p3<-round(s$Temp-RMTemp, digits=2)
  p4<-round(s$Hum-RMHum, digits=2)
  
  #wich percentage of values is out of the tolerance
  count.Temp<-length(p3[abs(p3)>=0.5])
  cTemp<-(100/length(p3))*(length(p3)-count.Temp)
  count.Hum<-length(p4[abs(p4)>=2])
  CHum<-(100/length(p4))*(length(p4)-count.Hum)
  
  mitt[i,1]<-mean(p1)#mean difference sensor and mean
  diff[i,1]<-max(p1)#max difference Sensor and mean
  mitt[i,2]<-mean(p2)#mean difference sensor and mean
  diff[i,2]<-max(p2)#max difference Sensor and mean
  mitt[i,3]<-mean(p3)#mean difference sensor and mean
  diff[i,3]<-max(p3)#max difference Sensor and mean
  mitt[i,4]<-mean(p4)#mean difference sensor and mean
  diff[i,4]<-max(p4)#max difference Sensor and mean
  diff[i,5]<-cTemp
  diff[i,6]<-CHum
  # assign(paste0("Stunde",liste[i]), result)
} 
diff<-round(diff,digits=2)
######
#enerel difference from sensor mean and overall mean
round(CMPM10-mean(CMPM10),digits=2)
round(CMPM5-mean(CMPM5),digits=2)
round(CMTemp-mean(CMTemp),digits=2)
round(CMHum-mean(CMHum),digits=2)

# 20 repetations of a value measured a a specific time
PM102<-PM10[-c(1)]
PM52<-PM5[-c(1)]
Temp2<-Temp[-c(1)]
Hum2<-Hum[-c(1)]

sd.PM10<-c()
sd.PM5<-c()
sd.Temp<-c()
sd.Hum<-c()

for (i in 1:97)
{
  sd.PM10[i]<-sd(PM102[i,],na.rm=TRUE)
  sd.PM5[i]<-sd(PM52[i,],na.rm=TRUE)
  sd.Temp[i]<-sd(Temp2[i,],na.rm=TRUE)
  sd.Hum[i]<-sd(Hum2[i,],na.rm=TRUE)
}
max(sd.PM10,na.rm=TRUE)
max(sd.PM5,na.rm=TRUE)
max(sd.Temp,na.rm=TRUE)
max(sd.Hum,na.rm=TRUE)


#plot helper function
#creates hourly values for motj function
motj.plot<-function(x)
{
  #27.09.2018 doy 270
  #starting point 00:00 
  m<-motj(x)
  b<-270*24*60
  c<-(m-b)/60
  return(c)
}

######################################
#
#Plots 
#
###########################
#Plotloop temperature
windows()
plot(motj.plot(`StundeSensor1-7132425`$Date),`StundeSensor1-7132425`$Temp,cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="l" ,xlim=c(14,110),ylim=c(18,25), col = "magenta", xlab="Zeit in Stunden", ylab="Temperatur °C", lwd = 1.5, main="Veränderung der Temperatur über den Testzeitraum ")
par(new=T)
for (i in 1:20)
{ 
  s<-liste[i]
  s<-paste0("Stunde",s)
  m<-get(s)
  plot(motj.plot(m$Date),m$Temp,xlim=c(14,110),ylim=c(18,25), xlab="",ylab="",type="l",col = "magenta", axes=FALSE)
  par(new=T)
}
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),RMTemp,xlim=c(14,110),ylim=c(18,25), xlab="",ylab="",type="l",col = "black", axes=FALSE,lwd=3)
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),(RMTemp-0.5),xlim=c(14,110),ylim=c(18,25), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),(RMTemp+0.5),xlim=c(14,110),ylim=c(18,25), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
legend(7,25, legend=c("Zeitreihe einzelner Sensoren","Genauigkeit(+-0.5°C)", "gemittelte Zeitreihe"),col=c("magenta", "darkgrey","black"), lty=1:2,lwd=3, cex=1.5,text.font=4 ,bty="n")


#Plotloop humidity
windows()
plot(motj.plot(`StundeSensor1-7132425`$Date),`StundeSensor1-7132425`$Hum,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, type="l" ,xlim=c(14,110),ylim=c(20,60), col = "blue", xlab="Zeit in Stunden", ylab="relative Luftfeuchtigkeit %", lwd = 1.5, main="Veränderung der relativen Luftfeuchtigkeit über den Testzeitraum ")
par(new=T)
for (i in 1:20)
{ 
  s<-liste[i]
  s<-paste0("Stunde",s)
  m<-get(s)
  plot(motj.plot(m$Date),m$Hum,xlim=c(14,110),ylim=c(20,60), xlab="",ylab="",type="l",col = "blue", axes=FALSE)
  par(new=T)
}
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),RMHum,xlim=c(14,110),ylim=c(20,60), xlab="",ylab="",type="l",col = "black", axes=FALSE,lwd=3)
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),(RMHum-5),xlim=c(14,110),ylim=c(20,60), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),(RMHum+5),xlim=c(14,110),ylim=c(20,60), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
legend(7,60, legend=c("Zeitreihe einzelner Sensoren","Genauigkeit(+-5%)", "gemittelte Zeitreihe"),col=c("blue","darkgrey", "black"), lty=1:2,lwd=3, cex=1.5,text.font=4 ,bty="n")

#relative error
#Plotloop PM10
windows()
plot(motj.plot(`StundeSensor1-7132425`$Date),`StundeSensor1-7132425`$PM10,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, type="l" ,xlim=c(14,110),ylim=c(0,20), col = "darkgreen", xlab="Zeit in Stunden", ylab="PM 10 µg/m³", lwd = 1.5, main="Veränderung von PM 10 über den Testzeitraum ")
par(new=T)
for (i in 1:20)
{ 
  s<-liste[i]
  s<-paste0("Stunde",s)
  m<-get(s)
  plot(motj.plot(m$Date),m$PM10,xlim=c(14,110),ylim=c(0,17), xlab="",ylab="",type="l",col = "darkgreen", axes=FALSE)
  par(new=T)
}
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),RMPM10,xlim=c(14,110),ylim=c(0,17), xlab="",ylab="",type="l",col = "black", axes=FALSE,lwd=3)
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),(RMPM10+(RMPM10*0.15)),xlim=c(14,110),ylim=c(0,17), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),(RMPM10-(RMPM10*0.15)),xlim=c(14,110),ylim=c(0,17), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
#par(new=T)
#plot(motj.plot(`StundeSensor1-7132425`$Date),(RMPM10+10),xlim=c(14,110),ylim=c(0,17), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
#par(new=T)
#plot(motj.plot(`StundeSensor1-7132425`$Date),(RMPM10-10),xlim=c(14,110),ylim=c(0,17), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)

legend(7,17, legend=c("Zeitreihe einzelner Sensoren","relativer Fehler(15%)", "gemittelte Zeitreihe"),col=c("darkgreen","grey" ,"black"), lty=1:2,lwd=3, cex=1.5,text.font=4 ,bty="n")


#Plotloop PM 2,5
windows()
plot(motj.plot(`StundeSensor1-7132425`$Date),`StundeSensor1-7132425`$PM5,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, type="l" ,xlim=c(14,110),ylim=c(0,5), col = "darkorange", xlab="Zeit in Stunden", ylab="PM 2,5 µg/m³", lwd = 1.5, main="Veränderung von PM 2,5 über den Testzeitraum ")
par(new=T)
for (i in 1:20)
{ 
  s<-liste[i]
  s<-paste0("Stunde",s)
  m<-get(s)
  plot(motj.plot(m$Date),m$PM5,xlim=c(14,110),ylim=c(0,5), xlab="",ylab="",type="l",col = "darkorange", axes=FALSE)
  par(new=T)
}
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),RMPM5,xlim=c(14,110),ylim=c(0,5), xlab="",ylab="",type="l",col = "black", axes=FALSE,lwd=3)
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),(RMPM5+(RMPM5*0.15)),xlim=c(14,110),ylim=c(0,5), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),(RMPM5-(RMPM5*0.15)),xlim=c(14,110),ylim=c(0,5), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
#error +-10
#par(new=T)
#plot(motj.plot(`StundeSensor1-7132425`$Date),(RMPM5+10),xlim=c(14,110),ylim=c(0,17), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
#par(new=T)
#plot(motj.plot(`StundeSensor1-7132425`$Date),(RMPM5-10),xlim=c(14,110),ylim=c(0,17), xlab="",ylab="",type="l",col = "darkgrey", axes=FALSE,lwd=3,lty=2)
legend(7,5, legend=c("Zeitreihe einzelner Sensoren","relativer Fehler(15%)", "gemittelte Zeitreihe"),col=c("darkorange","grey", "black"), lty=1:2,lwd=3, cex=1.5,text.font=4 ,bty="n")


#without error
#Plotloop PM10
windows()
plot(motj.plot(`StundeSensor1-7132425`$Date),`StundeSensor1-7132425`$PM10,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, type="l" ,xlim=c(14,110),ylim=c(0,14), col = "darkgreen", xlab="Zeit in Stunden", ylab="PM 10 µg/m³", lwd = 1.5, main="Veränderung von PM 10 über den Testzeitraum ")
par(new=T)
for (i in 1:20)
{ 
  s<-liste[i]
  s<-paste0("Stunde",s)
  m<-get(s)
  plot(motj.plot(m$Date),m$PM10,xlim=c(14,110),ylim=c(0,14), xlab="",ylab="",type="l",col = "darkgreen", axes=FALSE)
  par(new=T)
}
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),RMPM10,xlim=c(14,110),ylim=c(0,14), xlab="",ylab="",type="l",col = "black", axes=FALSE,lwd=3)
legend(7,14, legend=c("Zeitreihe einzelner Sensoren", "gemittelte Zeitreihe"),col=c("darkgreen", "black"), lty=1:1,lwd=3, cex=1.5,text.font=4 ,bty="n")


#Plotloop PM 2,5
windows()
plot(motj.plot(`StundeSensor1-7132425`$Date),`StundeSensor1-7132425`$PM5,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, type="l" ,xlim=c(14,110),ylim=c(0,5), col = "darkorange", xlab="Zeit in Stunden", ylab="PM 2,5 µg/m³", lwd = 1.5, main="Veränderung von PM 2,5 über den Testzeitraum ")
par(new=T)
for (i in 1:20)
{ 
  s<-liste[i]
  s<-paste0("Stunde",s)
  m<-get(s)
  plot(motj.plot(m$Date),m$PM5,xlim=c(14,110),ylim=c(0,5), xlab="",ylab="",type="l",col = "darkorange", axes=FALSE)
  par(new=T)
}
par(new=T)
plot(motj.plot(`StundeSensor1-7132425`$Date),RMPM5,xlim=c(14,110),ylim=c(0,5), xlab="",ylab="",type="l",col = "black", axes=FALSE,lwd=3)
legend(17,5, legend=c("Zeitreihe einzelner Sensoren", "gemittelte Zeitreihe"),col=c("darkorange", "black"), lty=1:1,lwd=3, cex=0.8,text.font=4)


