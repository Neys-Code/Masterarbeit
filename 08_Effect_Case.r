###################
#
# Effect of the case
#
#############
#Sensor 14 unchanged with case
#Sensor 20 without case
# load library
library(readr)
library(caret)
library(e1071)

#libary
#my libary #set right path!
source("E:\\Dropbox\\Dropbox\\Masterarbeit\\02_R-Code\\abgabe\\lib.r")
#set path to data
pfad<-("C:/Users/jana/Dropbox/Masterarbeit/Daten_vgl")#select specific Data
#pfad<-("E:/Dropbox/Dropbox/Masterarbeit/Daten_vgl")#select specific Data

setwd(pfad)# set woring directory
# list files
f<-list.files(path = pfad, pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# create all matrices and Variables to be filled within loop 
n=c()
i=1
liste=c()
normal<-mittel<-median<-min<-max<-sd<-var<-rse<-matrix(rep(0,times=length(f)),ncol=4,nrow =length(f) )
colnames(normal)<-colnames(max)<-colnames(mittel)<-colnames(median)<-colnames(min)<-colnames(max)<-colnames(sd)<-colnames(var)<-colnames(rse)<-c("PM10","PM5","Temp","Hum")
rownames(normal)<-rownames(max)<-rownames(mittel)<-rownames(median)<-rownames(min)<-rownames(max)<-rownames(sd)<-rownames(var)<-rownames(rse)<-f
#read 
#for loop to read all data and perform several statistical operations 
for (i in 1:20 )
{
  y=j=k=1 # reset counter Variables 
  Date=Time=Hum=PM5=PM10=Temp=c() # reset help Variables
  
  # e list to be iterated
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

  #write Variable with Sensor ID and number
  assign(paste0("Sensor",substr (f[i],1,10)),Sensor)
  liste<-c(liste,(paste0("Sensor",substr (f[i],1,10))))
}

########## Hourly mean values
liste2<-c()
#reclassify as hourly values with mean
for (i in 1:length(liste))
{ 
  s<-get(liste[i])
  assign(paste0("Tag",liste[i]), Tageswerte(s))
  assign(paste0("Stunde",liste[i]), Stundenwerte(s))
  liste2<-rbind(liste2,paste0("Stunde",liste[i]))
} 

#plot helper function
#creates hourly values for motj function
motj.plot<-function(x)
{
  #27.09.2018 doy 270
  #starting point 00:00 
  m<-motj(x)
  b<-132*24*60
  c<-(m-b)/60
  return(c)
}

#############
# PM10
#####
windows()
#unaltered # +17 because day 1 beginns at 17:00
plot(motj.plot(`StundeSensor14-7133108`$Date) ,`StundeSensor14-7133108`$PM10, type="l",col = "darkred",xlim=c(0,750),ylim=c(0,30),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="PM 10 µg/m³", lwd = 1.5, main="Verleich von Stunden- und Tagesmittel der PM10 Konzentration, mit und ohne Gehäuse")
par(new=T)
plot((motj.plot(`TagSensor14-7133108`$Date))+17 ,`TagSensor14-7133108`$PM10, type="l",col = "red",xlim=c(0,750),ylim=c(0,30),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="PM 10 µg/m³", lwd = 1.5)
par(new=T)
#no case
plot(motj.plot(`StundeSensor20-7133655`$Date) ,`StundeSensor20-7133655`$PM10, type="l",col = "darkgreen",xlim=c(0,750),ylim=c(0,30),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="PM 10 µg/m³", lwd = 1.5 )
par(new=T)
plot(motj.plot(`TagSensor20-7133655`$Date)+17 ,`TagSensor20-7133655`$PM10, type="l",col = "green",xlim=c(0,750),ylim=c(0,30),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="PM 10 µg/m³", lwd = 1.5)
par(new=T)
legend(-5,30, legend=c("Messgerät Nr.14 mit Gehäuse", "Messgerät Nr.20 ohne Gehäuse"),col=c("darkred", "darkgreen"), lty=1:1,lwd=3, cex=1.5,text.font=4 ,bty="n")


#############
# PM25
#####
windows()
#unaltered
plot(motj.plot(`StundeSensor14-7133108`$Date) ,`StundeSensor14-7133108`$PM5, type="l",col = "darkred",xlim=c(0,750),ylim=c(0,30),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="PM 2,5 µg/m³", lwd = 1.5,  main="Verleich von Stunden- und Tagesmittel der PM 2,5 Konzentration, mit und ohne Gehäuse")
par(new=T)
plot(motj.plot(`TagSensor14-7133108`$Date)+17 ,`TagSensor14-7133108`$PM5, type="l",col = "red",xlim=c(0,750),ylim=c(0,30),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5)
par(new=T)
#no case
plot(motj.plot(`StundeSensor20-7133655`$Date) ,`StundeSensor20-7133655`$PM5, type="l",col = "darkgreen",xlim=c(0,750),ylim=c(0,30),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5 )
par(new=T)
plot(motj.plot(`TagSensor20-7133655`$Date)+17 ,`TagSensor20-7133655`$PM5, type="l",col = "green",xlim=c(0,750),ylim=c(0,30),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5)
par(new=T)
legend(-5,30, legend=c("Messgerät Nr.14 mit Gehäuse", "Messgerät Nr.20 ohne Gehäuse"),col=c("darkred", "darkgreen"), lty=1:1,lwd=3, cex=1.5,text.font=4 ,bty="n")


#############
# Temp
#####
windows()
#unaltered
plot(motj.plot(`StundeSensor14-7133108`$Date) ,`StundeSensor14-7133108`$Temp, type="l",col = "darkred",xlim=c(0,750),ylim=c(0,40),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="Temperatur °C", lwd = 1.5, main="Verleich von Stunden- und Tagesmittel der Temperatur, mit und ohne Gehäuse")
par(new=T)
plot(motj.plot(`TagSensor14-7133108`$Date)+17 ,`TagSensor14-7133108`$Temp, type="l",col = "red",xlim=c(0,750),ylim=c(0,40),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5)
par(new=T)
#no case
plot(motj.plot(`StundeSensor20-7133655`$Date) ,`StundeSensor20-7133655`$Temp, type="l",col = "blue",xlim=c(0,750),ylim=c(0,40),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5 )
par(new=T)
plot(motj.plot(`TagSensor20-7133655`$Date)+17 ,`TagSensor20-7133655`$Temp, type="l",col = "darkblue",xlim=c(0,750),ylim=c(0,40),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5)
par(new=T)
legend(-5,40, legend=c("Messgerät Nr.14 mit Gehäuse", "Messgerät Nr.20 ohne Gehäuse"),col=c("darkred", "darkblue"), lty=1:1,lwd=3, cex=1.5,text.font=4 ,bty="n")

#############
# Hum
#####
windows()
#unaltered
plot(motj.plot(`StundeSensor14-7133108`$Date) ,`StundeSensor14-7133108`$Hum, type="l",col = "green",xlim=c(0,750),ylim=c(20,100),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="rel. Luftfeuchtigkeit %", lwd = 1.5, main="Verleich von Stunden- und Tagesmittel der rel. Luftfeuchtikeit, mit und ohne Gehäuse")
par(new=T)
plot(motj.plot(`TagSensor14-7133108`$Date)+17 ,`TagSensor14-7133108`$Hum, type="l",col = "darkgreen",xlim=c(0,750),ylim=c(20,100),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5)
par(new=T)
#no case
plot(motj.plot(`StundeSensor20-7133655`$Date) ,`StundeSensor20-7133655`$Hum, type="l",col = "blue",xlim=c(0,750),ylim=c(20,100),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5 )
par(new=T)
plot(motj.plot(`TagSensor20-7133655`$Date)+17 ,`TagSensor20-7133655`$Hum, type="l",col = "darkblue",xlim=c(0,750),ylim=c(20,100),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, xlab="Zeit in Stunden", ylab="", lwd = 1.5 )
par(new=T)
legend(-5,100, legend=c("Messgerät Nr.14 mit Gehäuse", "Messgerät Nr.20 ohne Gehäuse"),col=c("green", "blue"), lty=1:1,lwd=3, cex=1.5,text.font=4 ,bty="n")

#######stats
Statistik(`StundeSensor14-7133108`$PM10)
Statistik(`StundeSensor20-7133655`$PM10)
Statistik(`StundeSensor14-7133108`$PM5)
Statistik(`StundeSensor20-7133655`$PM5)
Statistik(`StundeSensor14-7133108`$Hum)
Statistik(`StundeSensor20-7133655`$Hum)
Statistik(`StundeSensor14-7133108`$Temp)
Statistik(`StundeSensor20-7133655`$Temp)

##############
#difference
diff1<-`StundeSensor14-7133108`$PM10-`StundeSensor20-7133655`$PM10
Statistik(diff) 
