#####################################################################
#
#compare Sensor with oficcial data Sensor 16
#
######################################################################

######################################################################
#1. Humidity
######################################################################
#fall
#Compare Sensor 16 to Station Ostalee 
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/Ostalee_Herbst.csv", stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$AVG_RH350<-as.numeric(dwd$AVG_RH350)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor16- 713582`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$Hum)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_RH350)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3=na.omit(ds3)

#plot humidity and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",ylim =c(20,100),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="Relative Luftfeuchtigkeit [%] ", main= "Vergleich Luftfeuchtigkeit DHT 22 Sensor (rot) und Messtation Ostallee (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,ylim =c(20,100), axes=FALSE, col="blue", xlab="",ylab="")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="Relative Luftfeuchtigkeit [%]",main="Differenz zwischen DHT 22 Sensor und der Messtation Ostallee")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)

#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)


####################################################################################
#Summer values
####################################################################################
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/Ostalee_Sommer.csv", header=TRUE,stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$AVG_RH350<-as.numeric(dwd$AVG_RH350)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor16- 713582`)

#01.06 - 31.08.2019 merging Data
ds1<-data.frame(b$Date,b$Hum)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_RH350)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3=na.omit(ds3)

#plot humidity and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",ylim =c(20,100),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="Relative Luftfeuchtigkeit [%] ", main= "Vergleich Luftfeuchtigkeit DHT 22 Sensor (rot) und Messtation Ostallee (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,ylim =c(20,100), axes=FALSE, col="blue", xlab="",ylab="")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

# remove false values from sensor breakage
ds3<- ds3[-c(288:370),]
# stats
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="Relative Luftfeuchtigkeit [%]",main="Differenz zwischen DHT 22 Sensor und der Messtation Ostallee")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)

#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse und mae
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
MAE(ds3$Hum_SDS,ds3$Hum_DWD)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
MAE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)

######################################################################
#2.Temperature 
#####################################################################

#summer
#Compare Sensor 16 to Station Ostalee 
Temperatur<-read.csv("E:\\Dropbox\\Dropbox\\Masterarbeit\\Temp_Sommer_WZ_UTC.csv",sep=",")

dwd<-Temperatur
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$AVG_RH350<-as.numeric(dwd$Ostallee)

# Same code as for humidity variable names not changed
#Sensor data to hourly values
b<-Stundenwerte(`Sensor16- 713582`)

#comparison
#01.06.2019-31.8.2019
ds1<-data.frame(b$Date,b$Temp)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_RH350)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3$pm10<-b$PM10
ds3$pm5<-b$PM5
ds3=na.omit(ds3)

# remove false values from sensor breakage
ds3<- ds3[-c(288:370),]

#plot Temperature and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",ylim =c(10,40),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="Temperatur °C", main= "Vergleich Temperatur DHT 22 Sensor (rot) und Messtation Ostallee (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,ylim =c(10,40), axes=FALSE, col="blue", xlab="",ylab="")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,ylim=c(-20,20),cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="Temperatur °C",main="Differenz zwischen DHT 22 Sensor und der Messtation Ostallee")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)

#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)

#####################################################################
#fall
#######################################################################
#Compare Sensor 16 to Station Ostalee 
Temperatur<-read.csv("E:\\Dropbox\\Dropbox\\Masterarbeit\\Temp_Herbst_WZ_UTC.csv",sep=",")
dwd<-Temperatur
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$AVG_RH350<-as.numeric(dwd$Ostallee)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor16- 713582`)
#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$Temp)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_RH350)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3$pm10<-b$PM10
ds3$pm5<-b$PM5
ds3=na.omit(ds3)

#plot Temperature and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",ylim =c(0,35),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="Temperatur °C", main= "Vergleich Temperatur DHT 22 Sensor (rot) und Messtation Ostallee (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,ylim =c(0,35), axes=FALSE, col="blue", xlab="",ylab="")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,ylim=c(-20,20),cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="Temperatur °C",main="Differenz zwischen DHT 22 Sensor und der Messtation Ostallee")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)

#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#dailymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)


#####################################################################
#
#compare Sensor with oficcial data Sensor 15
#
######################################################################

######################################################################
#1. Humidity
######################################################################
#fall
#Compare Sensor 15 to Station Zewen 
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/zewen_herbst.csv", stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$AVG_RH200<-as.numeric(dwd$AVG_RH200)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor15-7136444`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$Hum)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_RH200)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3$pm10<-b$PM10
ds3$pm5<-b$PM5
ds3=na.omit(ds3)

#plot humidity and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,type="l",col="red",ylim =c(20,100),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="Relative Luftfeuchtigkeit [%] ", main= "Vergleich Luftfeuchtigkeit DHT 22 Sensor (rot) und Messtation Ostallee (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,type="l",ylim =c(20,100), axes=FALSE, col="blue", xlab="",ylab="")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="Relative Luftfeuchtigkeit [%]",main="Differenz zwischen DHT 22 Sensor und der Messtation Ostallee")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)
#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)

#############################
#Summer values
#############################
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/zewen_sommer.csv", header=TRUE,stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$AVG_RH200<-as.numeric(dwd$AVG_RH200)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor15-7136444`)

#01.06 - 31.08.2019 merging Data
ds1<-data.frame(b$Date,b$Hum)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_RH200)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3$pm10<-b$PM10
ds3$pm5<-b$PM5
ds3=na.omit(ds3)

#plot humidity and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,type="l",col="red",ylim =c(20,100),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="Relative Luftfeuchtigkeit [%] ", main= "Vergleich Luftfeuchtigkeit DHT 22 Sensor (rot) und Messtation Zewen (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,type="l",ylim =c(20,100), axes=FALSE, col="blue", xlab="",ylab="")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="Relative Luftfeuchtigkeit [%]",main="Differenz zwischen DHT 22 Sensor und der Messtation Zewen")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)
#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)

######################################################################
#2.Temperature 
#####################################################################

#summer
#Compare Sensor 15 to Station zewen
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/zewen_sommer.csv", header=TRUE,stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$AVG_TA200<-as.numeric(dwd$AVG_TA200)

# Same code as for humidity variable names not changed
#Sensor data to hourly values
b<-Stundenwerte(`Sensor15-7136444`)

#comparison
#01.06.2019-31.8.2019
ds1<-data.frame(b$Date,b$Temp)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_TA200)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3$pm10<-b$PM10
ds3$pm5<-b$PM5
ds3=na.omit(ds3)

#plot Temperature and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",type="l",ylim =c(10,40),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="Temperatur °C", main= "Vergleich Temperatur DHT 22 Sensor (rot) und Messtation Zewen (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,type="l",ylim =c(10,40), axes=FALSE, col="blue", xlab="",ylab="")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,ylim=c(-20,20),cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="Temperatur °C",main="Differenz zwischen DHT 22 Sensor und der Messtation Zewen")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)

#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)
####################################
#fall
#######################################
#Compare Sensor 15 to Station Ostallee 
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/zewen_herbst.csv", header=TRUE,stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$AVG_TA200<-as.numeric(dwd$AVG_TA200)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor15-7136444`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$Temp)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_TA200)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3$pm10<-b$PM10
ds3$pm5<-b$PM5
ds3=na.omit(ds3)

#plot Temperature and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,type="l",col="red",ylim =c(0,35),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="Temperatur °C", main= "Vergleich Temperatur DHT 22 Sensor (rot) und Messtation Ostallee (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,type="l",ylim =c(0,35), axes=FALSE, col="blue", xlab="",ylab="")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,ylim=c(-20,20),cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="Temperatur °C",main="Differenz zwischen DHT 22 Sensor und der Messtation Ostallee")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)
#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)

######################################################################
#
#compare PM Value
#
######################################################################
#####################################################################
#
#compare Sensor with oficcial data station Pfalzel
#
######################################################################

######################################################################
#3. PM 2,5
######################################################################

#fall
#Compare Sensor 4 to Station Pfalzel 
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/Pfalzel_Herbst.csv", stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$pm25<-as.numeric(dwd$pm25)

#Sensor data to hourly values
#b<-Stundenwerte(`Sensor4-7131992`)
#b<-Stundenwerte(`Sensor3-7131580`)
b<-Stundenwerte(`Sensor15-7136444`)
#b<-Stundenwerte(`Sensor8-7131428`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$PM5)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$pm25)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
#ds3$pm10<-b$PM10
#ds3$pm5<-b$PM5
ds3=na.omit(ds3)

#plot humidity and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",ylim =c(0,20),xlab="Zeit in Stunden",type = "l",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 2,5 µg/m³", main= "Vergleich Luftfeuchtigkeit SDS 011 Sensor (rot) und Messtation Eltzstraße (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,ylim =c(0,20), axes=FALSE, col="blue", xlab="",ylab="",type = "l")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="PM 2,5 µg/m³",main="Differenz zwischen SDS 011 Sensor und der Messtation Eltzstraße")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)

#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)


######################
#Summer
#######################
#Compare Sensor 4/8 to Station Pfalzel 
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/Pfalzel_Sommer.csv", stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Tag,7,10),"/",substr(dwd$Tag ,4,5),"/",substr(dwd$Tag,1,2)," ",dwd$Stunde,":00")
dwd$pm25<-as.numeric(dwd$pm25)

#Sensor data to hourly values
#b<-Stundenwerte(`Sensor4-7131992`)
#b<-Stundenwerte(`Sensor3-7131580`)
b<-Stundenwerte(`Sensor15-7136444`)
#b<-Stundenwerte(`Sensor8-7131428`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$PM5)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$pm25)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
#ds3$pm10<-b$PM10
#ds3$pm5<-b$PM5
ds3=na.omit(ds3)

#plot humidity and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",ylim =c(0,20),xlab="Zeit in Stunden",type = "l",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 2,5 µg/m³", main= "Vergleich Luftfeuchtigkeit SDS 011 Sensor (rot) und Messtation Pfalzel (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,ylim =c(0,20), axes=FALSE, col="blue", xlab="",ylab="",type = "l")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,ylim =c(-20,20),lwd=1,cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="PM 2,5 µg/m³",main="Differenz zwischen SDS 011 Sensor und der Messtation Pfalzel")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)
#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)
#####################################################################
#
#compare Sensor with oficcial data station Ostallee
#
######################################################################

######################################################################
#4. PM 10
######################################################################
#fall
#Compare Sensor 16 to Station Ostalee 
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Herbst.csv", stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Zeitpunkt,7,10),"/",substr(dwd$Zeitpunkt ,4,5),"/",substr(dwd$Zeitpunkt,1,2)," ",substr(dwd$Zeitpunkt,12,17),":00")
dwd$pm10<-as.numeric(dwd$Trier.Ostallee.PM10)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor16- 713582`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$PM10,b$Hum)
names(ds1)<-c("Date","Hum_SDS","Hum")
ds2<-data.frame(dwd$time,dwd$pm10)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3=na.omit(ds3)

#plot humidity and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor (rot) und Messtation Ostallee (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,ylim =c(-40,10),cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="PM 10 µg/m³",main="Differenz zwischen SDS 011 Sensor und der Messtation Ostallee")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test welch test
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)

#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

# Using ggplot2
library(ggplot2)

# qplot()
windows()
qplot(ds3$Hum_DWD,ds3$Hum_SDS, colour = ds3$Hum,ylim =c(0,40),xlim =c(0,40), scale_fill_brewer(palette="Spectral"))
p<-ggplot(ds3, aes(x=ds3$Hum_DWD, y=ds3$Hum_SDS, colour=ds3$Hum)) + geom_point() + 
  scale_colour_gradientn(colours=rainbow(4))
p<-p + labs(title = "Korrelation zwischen PM 10 gemessen SDS 011 und der Station Ostallee ", x = "PM 10 µg/m³ Station Ostallee", y = "PM 10 µg/m³ vom SDS 011") +
  scale_x_discrete(drop=TRUE) + ylim(c(0, 40) )
p<-p + theme_bw()
p<-p + theme(text = element_text(size=16)) + geom_abline()
plot(p)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)
 

#############################
#Summer values
#############################
######################################################################
#4. PM 10
######################################################################
#Compare Sensor 16 to Station Ostalee 
dwd<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Sommer.csv", stringsAsFactors=FALSE)
dwd$time<-paste0(substr(dwd$Zeitpunkt,7,10),"/",substr(dwd$Zeitpunkt ,4,5),"/",substr(dwd$Zeitpunkt,1,2)," ",substr(dwd$Zeitpunkt,12,17),":00")
dwd$pm10<-as.numeric(dwd$Trier.Ostallee.PM10)

#Sensor data to hourly values
b<-Stundenwerte(`Sensor16- 713582`)

#comparison
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$PM10,b$Hum)
names(ds1)<-c("Date","Hum_SDS","Hum")
ds2<-data.frame(dwd$time,dwd$pm10)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3=na.omit(ds3)

#plot humidity and time
windows()
plot(as.numeric(ds3$Date),ds3$Hum_SDS,col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, ylab="PM 10 µg/m³", main= "Vergleich PM 10 und SDS 011 Sensor (rot) und Messtation Ostallee (blau)") 
par(new=T)
plot(as.numeric(ds3$Date),ds3$Hum_DWD,ylim =c(0,50), axes=FALSE, col="blue", xlab="",ylab="",type="l")
Statistik(ds3$Hum_SDS)
Statistik(ds3$Hum_DWD)

#difference of both timeseries
windows()
ds3$diff<-ds3$Hum_SDS-ds3$Hum_DWD
Statistik(abs(ds3$diff))
plot(as.numeric(ds3$Date),ds3$diff,lwd=1,cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5, type="p",xlab="Zeit in Stunden",ylab="PM 10 µg/m³",main="Differenz zwischen SDS 011 Sensor und der Messtation Ostallee")
par(new=T)
abline(h=0, col="red",lwd=4)
abline(h=10, col="blue",lwd=2)
abline(h=-10, col="blue",lwd=2)

# t-test 
ttest<-t.test(ds3$Hum_SDS,ds3$Hum_DWD , alternative = "two.sided", var.equal = FALSE)

#correlation 
model<-lm(ds3$Hum_SDS~ds3$Hum_DWD)
model
summary(model)
plot(ds3$Hum_SDS~ds3$Hum_DWD,ylim =c(0,40),xlim =c(0,40))
abline(a=1,b=1, col="black",lwd=4)

#rmse
RMSE(ds3$Hum_SDS,ds3$Hum_DWD)
#daylymeans
day16<-Tageswerte(`Sensor16- 713582`)
mean(day16$PM10)
#daily
data<-ds3
data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
pm<-data.frame(c(aggregate(data$Hum_DWD ~ Date2, data, mean)[1]),c(aggregate(data$Hum_DWD ~ Date2, data, mean)[2]),c(aggregate(data$Hum_SDS ~ Date2, data, mean)[2]))
#rmse
RMSE(pm$data.Hum_SDS,pm$data.Hum_DWD)
#model
model<-lm(pm$data.Hum_SDS~pm$data.Hum_DWD)
model
summary(model)

# Using ggplot2
library(ggplot2)

# qplot()
windows()
qplot(ds3$Hum_DWD,ds3$Hum_SDS, colour = ds3$Hum,ylim =c(0,40),xlim =c(0,40), scale_fill_brewer(palette="Spectral"))
p<-ggplot(ds3, aes(x=ds3$Hum_DWD, y=ds3$Hum_SDS, colour=ds3$Hum)) + geom_point() + 
  scale_colour_gradientn(colours=rainbow(4))
p<-p + labs(title = "Korrelation zwischen PM 10 gemessen SDS 011 und der Station Ostallee ", x = "PM 10 µg/m³ Station Ostallee", y = "PM 10 µg/m³ vom SDS 011") +
  scale_x_discrete(drop=TRUE) + ylim(c(0, 40) )
p<-p + theme_bw()
p<-p + theme(text = element_text(size=16)) + geom_abline()
plot(p)

