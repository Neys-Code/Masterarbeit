#################################
#
#correct with a linear model (without humidity correction)
#
#########################################

#Compare Sensor 16 to Station Ostalee 
#ZIMEN<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Sommer.csv", stringsAsFactors=FALSE)
ZIMEN<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Herbst.csv", stringsAsFactors=FALSE)
ZIMEN$time<-paste0(substr(ZIMEN$Zeitpunkt,7,10),"/",substr(ZIMEN$Zeitpunkt ,4,5),"/",substr(ZIMEN$Zeitpunkt,1,2)," ",substr(ZIMEN$Zeitpunkt,12,17),":00")
ZIMEN$pm10<-as.numeric(ZIMEN$Trier.Ostallee.PM10)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor16- 713582`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$PM10,b$Hum)
names(ds1)<-c("Date","PM_SDS","Hum")
ds2<-data.frame(ZIMEN$time,ZIMEN$pm10)
names(ds2)<-c("Date","PM_ZIMEN")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3=na.omit(ds3)
#difference
ds3$diff<-ds3$PM_SDS-ds3$PM_ZIMEN

#######################linear models
#correlation 
#model<-lm(ds3$PM_SDS~ds3$PM_ZIMEN)
model<-lm(ds3$PM_ZIMEN~ds3$PM_SDS)
model
a<-coef(model)
slope<-a[2]
intercept<-a[1]
summary(model)

#use model
#ds3$lin.corr<-(ds3$PM_SDS-intercept)/slope
ds3$lin.corr<-(ds3$PM_SDS*slope)+intercept

#rmse
RMSE(ds3$PM_SDS,ds3$PM_ZIMEN)
RMSE(ds3$lin.corr,ds3$PM_ZIMEN)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$PM_ZIMEN ~ Date2, data, mean)[1]),c(aggregate(data$PM_ZIMEN ~ Date2, data, mean)[2]),c(aggregate(data$lin.corr ~ Date2, data, mean)[2]),c(aggregate(data$PM_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.PM_SDS,pm$data.PM_ZIMEN)
RMSE(pm$data.lin.corr,pm$data.PM_ZIMEN)


#plot PMidity and time
windows()
plot(as.numeric(ds3$Date),ds3$PM_SDS,col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor(rot) Messtation Ostallee(blau) SDS 011 korrigiert (grün)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$lin.corr,col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$PM_ZIMEN,ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")

par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.lin.corr,lwd="2",col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "") 
par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.PM_ZIMEN,lwd="2",ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.PM_SDS,lwd="2",ylim =c(0,50), axes=FALSE, col="red", xlab="",ylab="",type="l")


#running median
windows()
plot(as.numeric(ds3$Date),runmed(ds3$PM_SDS,3,algorithm="Turlach",endrule = c("keep")),col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor(rot) Messtation Ostallee(blau) SDS 011 korrigiert (grün)") 
par(new=T)
plot(as.numeric(ds3$Date),runmed(ds3$lin.corr,3,algorithm="Turlach",endrule = c("keep")),col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "") 
par(new=T)
plot(as.numeric(ds3$Date),runmed(ds3$PM_ZIMEN,3,algorithm="Turlach",endrule = c("keep")),ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")

par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.lin.corr,3,algorithm="Turlach",endrule = c("keep")),lwd="2",col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "") 
par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.PM_ZIMEN,3,algorithm="Turlach",endrule = c("keep")),lwd="2",ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.PM_SDS,3,algorithm="Turlach",endrule = c("keep")),lwd="2",ylim =c(0,50), axes=FALSE, col="red", xlab="",ylab="",type="l")


##########################################################################################
#Compare Sensor 4 to Station pfalzel
############################################################################################
#ZIMEN<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Sommer.csv", stringsAsFactors=FALSE)
ZIMEN<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Herbst.csv", stringsAsFactors=FALSE)
ZIMEN$time<-paste0(substr(ZIMEN$Zeitpunkt,7,10),"/",substr(ZIMEN$Zeitpunkt ,4,5),"/",substr(ZIMEN$Zeitpunkt,1,2)," ",substr(ZIMEN$Zeitpunkt,12,17),":00")
ZIMEN$pm25<-as.numeric(ZIMEN$Trier.Ostallee.PM10)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor4-7131992`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$PM10,b$Hum)
names(ds1)<-c("Date","PM_SDS","Hum")
ds2<-data.frame(ZIMEN$time,ZIMEN$pm25)
names(ds2)<-c("Date","PM_ZIMEN")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3=na.omit(ds3)
#difference
ds3$diff<-ds3$PM_SDS-ds3$PM_ZIMEN

#######################models
#correlation 
#model<-lm(ds3$PM_SDS~ds3$PM_ZIMEN)
model<-lm(ds3$PM_ZIMEN~ds3$PM_SDS)
model
a<-coef(model)
slope<-a[2]
intercept<-a[1]
summary(model)

#use model
#ds3$lin.corr<-(ds3$PM_SDS-intercept)/slope
ds3$lin.corr<-(ds3$PM_SDS*slope)+intercept

#rmse
RMSE(ds3$PM_SDS,ds3$PM_ZIMEN)
RMSE(ds3$lin.corr,ds3$PM_ZIMEN)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$PM_ZIMEN ~ Date2, data, mean)[1]),c(aggregate(data$PM_ZIMEN ~ Date2, data, mean)[2]),c(aggregate(data$lin.corr ~ Date2, data, mean)[2]),c(aggregate(data$PM_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.PM_SDS,pm$data.PM_ZIMEN)
RMSE(pm$data.lin.corr,pm$data.PM_ZIMEN)


#plot PMidity and time
windows()
plot(as.numeric(ds3$Date),ds3$PM_SDS,col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 2,5 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor(rot) Messtation Ostallee(blau) SDS 011 korrigiert (grün)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$lin.corr,col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="", main= "") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$PM_ZIMEN,ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")

par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.lin.corr,lwd="2",col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="", main= "") 
par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.PM_ZIMEN,lwd="2",ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.PM_SDS,lwd="2",ylim =c(0,50), axes=FALSE, col="red", xlab="",ylab="",type="l")

#running median
windows()
plot(as.numeric(ds3$Date),runmed(ds3$PM_SDS,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 2.5 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor(rot) Messtation Ostallee(blau) SDS 011 korrigiert (grün)") 
par(new=T)
plot(as.numeric(ds3$Date),runmed(ds3$lin.corr,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="", main= "") 
par(new=T)
plot(as.numeric(ds3$Date),runmed(ds3$PM_ZIMEN,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")

par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.lin.corr,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),lwd="2",col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="", main= "") 
par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.PM_ZIMEN,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),lwd="2",ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.PM_SDS,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),lwd="2",ylim =c(0,50), axes=FALSE, col="red", xlab="",ylab="",type="l")


########################################
#
#correct with a linear model (with humidity correction)
#
#########################################

#Compare Sensor 16 to Station Ostalee 
#ZIMEN<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Sommer.csv", stringsAsFactors=FALSE)
ZIMEN<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Herbst.csv", stringsAsFactors=FALSE)
ZIMEN$time<-paste0(substr(ZIMEN$Zeitpunkt,7,10),"/",substr(ZIMEN$Zeitpunkt ,4,5),"/",substr(ZIMEN$Zeitpunkt,1,2)," ",substr(ZIMEN$Zeitpunkt,12,17),":00")
ZIMEN$pm10<-as.numeric(ZIMEN$Trier.Ostallee.PM10)
# read humidity corrected data 
pfad<-("E:/Dropbox/Dropbox/Masterarbeit/Korr/Stunden")#desktop1

setwd(pfad)# set woring directory
# Longlist on Directory and read
f<-list.files(path = pfad, pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
liste<-c()
for (i in 1:length(f))
{
  x<-read.csv(f[i],sep = ",")
  assign(substr(f[i],1,16),x)
  liste[i]<-substr(f[i],1,16)
}

#Sensor data to hourly values
b<-c.SommerSensor16
#b<-c.HerbstSensor16

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$PM10,b$Hum)
names(ds1)<-c("Date","PM_SDS","Hum")
ds2<-data.frame(ZIMEN$time,ZIMEN$pm10)
names(ds2)<-c("Date","PM_ZIMEN")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3=na.omit(ds3)
#difference
ds3$diff<-ds3$PM_SDS-ds3$PM_ZIMEN

#######################models
#correlation 
#model<-lm(ds3$PM_SDS~ds3$PM_ZIMEN)
model<-lm(ds3$PM_ZIMEN~ds3$PM_SDS)
model
a<-coef(model)
slope<-a[2]
intercept<-a[1]
summary(model)

#use model
#ds3$lin.corr<-(ds3$PM_SDS-intercept)/slope
ds3$lin.corr<-(ds3$PM_SDS*slope)+intercept

#rmse
RMSE(ds3$PM_SDS,ds3$PM_ZIMEN)
RMSE(ds3$lin.corr,ds3$PM_ZIMEN)
#daylymeans
day16<-Tageswerte(`c.SommerSensor16`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$PM_ZIMEN ~ Date2, data, mean)[1]),c(aggregate(data$PM_ZIMEN ~ Date2, data, mean)[2]),c(aggregate(data$lin.corr ~ Date2, data, mean)[2]),c(aggregate(data$PM_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.PM_SDS,pm$data.PM_ZIMEN)
RMSE(pm$data.lin.corr,pm$data.PM_ZIMEN)

#model
model<-lm(pm$data.PM_SDS~pm$data.PM_ZIMEN)
summary(model)


#plot PMidity and time
windows()
plot(as.numeric(ds3$Date),ds3$PM_SDS,col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor(rot) Messtation Ostallee(blau) SDS 011 korrigiert (grün)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$lin.corr,col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$PM_ZIMEN,ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")

par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.lin.corr,lwd="2",col="darkgreen",type="l",axes=FALSE,ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "") 
par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.PM_ZIMEN,lwd="2",ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.PM_SDS,lwd="2",ylim =c(0,50), axes=FALSE, col="red", xlab="",ylab="",type="l")


#running median
windows()
plot(as.numeric(ds3$Date),runmed(ds3$PM_SDS,3,algorithm="Turlach",endrule = c("keep")),col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor(rot) Messtation Ostallee(blau) SDS 011 korrigiert (grün)") 
par(new=T)
plot(as.numeric(ds3$Date),runmed(ds3$lin.corr,3,algorithm="Turlach",endrule = c("keep")),col="darkgreen",type="l",axes=FALSE,ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "") 
par(new=T)
plot(as.numeric(ds3$Date),runmed(ds3$PM_ZIMEN,3,algorithm="Turlach",endrule = c("keep")),ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")

par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.lin.corr,3,algorithm="Turlach",endrule = c("keep")),lwd="2",col="darkgreen",axes=FALSE,type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "") 
par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.PM_ZIMEN,3,algorithm="Turlach",endrule = c("keep")),lwd="2",ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.PM_SDS,3,algorithm="Turlach",endrule = c("keep")),lwd="2",ylim =c(0,50), axes=FALSE, col="red", xlab="",ylab="",type="l")

##########################################################################################
#
#Compare Sensor 4 to Station pfalzel
ZIMEN<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Sommer.csv", stringsAsFactors=FALSE)
#ZIMEN<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Herbst.csv", stringsAsFactors=FALSE)
ZIMEN$time<-paste0(substr(ZIMEN$Zeitpunkt,7,10),"/",substr(ZIMEN$Zeitpunkt ,4,5),"/",substr(ZIMEN$Zeitpunkt,1,2)," ",substr(ZIMEN$Zeitpunkt,12,17),":00")
ZIMEN$pm25<-as.numeric(ZIMEN$Trier.Ostallee.PM10)

#Sensor data to hourly values
b<-`c.SommerSensor4-`

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$PM10,b$Hum)
names(ds1)<-c("Date","PM_SDS","Hum")
ds2<-data.frame(ZIMEN$time,ZIMEN$pm25)
names(ds2)<-c("Date","PM_ZIMEN")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3=na.omit(ds3)
#difference
ds3$diff<-ds3$PM_SDS-ds3$PM_ZIMEN


#######################models
#correlation 
#model<-lm(ds3$PM_SDS~ds3$PM_ZIMEN)
model<-lm(ds3$PM_ZIMEN~ds3$PM_SDS)
model
a<-coef(model)
slope<-a[2]
intercept<-a[1]
summary(model)

#use model
#ds3$lin.corr<-(ds3$PM_SDS-intercept)/slope
ds3$lin.corr<-(ds3$PM_SDS*slope)+intercept

#rmse
RMSE(ds3$PM_SDS,ds3$PM_ZIMEN)
RMSE(ds3$lin.corr,ds3$PM_ZIMEN)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$PM_ZIMEN ~ Date2, data, mean)[1]),c(aggregate(data$PM_ZIMEN ~ Date2, data, mean)[2]),c(aggregate(data$lin.corr ~ Date2, data, mean)[2]),c(aggregate(data$PM_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.PM_SDS,pm$data.PM_ZIMEN)
RMSE(pm$data.lin.corr,pm$data.PM_ZIMEN)


#plot PMidity and time
windows()
plot(as.numeric(ds3$Date),ds3$PM_SDS,col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 2,5 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor(rot) Messtation Ostallee(blau) SDS 011 korrigiert (grün)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$lin.corr,col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="", main= "") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$PM_ZIMEN,ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")

par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.lin.corr,lwd="2",col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="", main= "") 
par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.PM_ZIMEN,lwd="2",ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
par(new=T)
plot((as.numeric(pm$Date2)*24),pm$data.PM_SDS,lwd="2",ylim =c(0,50), axes=FALSE, col="red", xlab="",ylab="",type="l")


#running median
windows()
plot(as.numeric(ds3$Date),runmed(ds3$PM_SDS,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 2.5 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor(rot) Messtation Ostallee(blau) SDS 011 korrigiert (grün)") 
par(new=T)
plot(as.numeric(ds3$Date),runmed(ds3$lin.corr,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="", main= "") 
par(new=T)
plot(as.numeric(ds3$Date),runmed(ds3$PM_ZIMEN,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")

par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.lin.corr,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),lwd="2",col="darkgreen",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="", main= "") 
par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.PM_ZIMEN,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),lwd="2",ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
par(new=T)
plot((as.numeric(pm$Date2)*24),runmed(pm$data.PM_SDS,3,algorithm="Turlach",endrule = c("keep")),xlim=c(0,1400),lwd="2",ylim =c(0,50), axes=FALSE, col="red", xlab="",ylab="",type="l")


############################################

# use calibration on all sensors
# run script 2 before
#use model
#Summer
#for pm10 from Sensor 16
S.slope<-2.190295 
S.intercept<- 4.37239
#for pm25 from Sensor 4
S.intercept2<- 0.1150618 
S.slope2<- 1.423552
  
#for pm10 from Sensor 16
H.intercept<-17.70941
H.slope<-0.4652399
#for pm25 from Sensor 4
H.intercept2<-14.41529
H.slope2<-0.4746258

S.mittelwert<-c()
S.mittelwert2<-c()
for (i in 1:length(liste))
{ 
  x<-get(liste2[i])
  result1<-(x$PM10*S.slope)+S.intercept
  result2<-(x$PM5*S.slope2)+S.intercept2
  S.mittelwert[i]<-mean(result1)
  S.mittelwert2[i]<-mean(result2)
  #assign(paste0("Stunde",liste[i]), result)
  #liste2<-rbind(liste2,paste0("Stunde",liste[i]))
} 

H.mittelwert<-c()
H.mittelwert2<-c()
for (i in 1:length(liste))
{ 
  x<-get(liste2[i])
  result1<-(x$PM10*H.slope)+H.intercept
  result2<-(x$PM5*H.slope2)+H.intercept2
  H.mittelwert[i]<-mean(result1)
  H.mittelwert2[i]<-mean(result2)
  #assign(paste0("Stunde",liste[i]), result)
  #liste2<-rbind(liste2,paste0("Stunde",liste[i]))
} 
