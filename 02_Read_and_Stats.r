#####################################################################
#
#Read Data 
#
#####################################################################
# Load Library
library(readr)
library(caret)
library(e1071)
library(MASS)
library(outliers)
library(e1071)
#my libary #set right path!
source("E:\\Dropbox\\Dropbox\\Masterarbeit\\02_R-Code\\abgabe\\lib.r")

# set working directory for different machines 
# choose dataset
# pfad<-("C:/Users/jana/Dropbox/Masterarbeit/Daten3")#laptop
pfad<-("E:/Dropbox/Dropbox/Masterarbeit/Daten2")#desktop

setwd(pfad)# set woring directory
# longlist all files
f<-list.files(path = pfad, pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# create all matrices to be filled within loop 
n=c()
i=1
liste=c()
normal<-mittel<-median<-min<-max<-sd<-var<-rse<-matrix(rep(0,times=length(f)),ncol=4,nrow =length(f) )
colnames(normal)<-colnames(max)<-colnames(mittel)<-colnames(median)<-colnames(min)<-colnames(max)<-colnames(sd)<-colnames(var)<-colnames(rse)<-c("PM10","PM5","Temp","Hum")
rownames(normal)<-rownames(max)<-rownames(mittel)<-rownames(median)<-rownames(min)<-rownames(max)<-rownames(sd)<-rownames(var)<-rownames(rse)<-f

#variables different correlation coefficients
chi<-linear<-pearson<-spearman<-cubic<-matrix(rep(0,times=length(f)),ncol=6,nrow =length(f) )
colnames(cubic)<-colnames(linear)<-colnames(pearson)<-colnames(spearman)<-c("Hum_PM10","Hum_PM5","PM5_PM10","Temp_PM10","Temp_PM5","Temp_Hum")
rownames(cubic)<-rownames(linear)<-rownames(pearson)<-rownames(spearman)<-f


#for loop to read all data and perform several stats
for (i in 1:20 )
{
  y=j=k=1 # reset counter Variables 
  Date=Time=Hum=PM5=PM10=Temp=c() # reset help variables
  #e list to be iterated
  e<-list.files(path = paste0(pfad,"/",f[i]), pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  for (k in 1:length(e))
  { y=1
  n<-e[k]
  # list to be iterated
  l<-list.files(path = paste0(pfad,"/",f[i],"/",e[k]), pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  #reset "inner" help variables
  Temp2<-c()
  Date2=Time4=Hum2=PM52=PM102=c()
  
  #inner Loop #read csv files
  for (y in 1:length(l))
  {j=1
  x<-read_delim(paste0(pfad,"/",f[i],"/",e[k],"/",l[y]),";", escape_double = FALSE, trim_ws = TRUE, na = c("", "NA"),quoted_na = TRUE)
  
  #correct different variable names
  if (is.na(x$BMP_temperature[1])==FALSE)
  {x$Temp<-x$BMP_temperature
  x$Humidity<-x$BMP_pressure}
  
  #time in minutes
  Time2=mapply(motj,c(x$Time),  USE.NAMES = FALSE)#minuites as decimal
  x=cbind(x,Time2)
  x<-x[-c(2,3,4,5,6,7,10,11,14,15,16,17,18)]
  rm(Time2)
  
  x<-Runden(x)
  
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
  
  #name<-paste0(substr (f[i],1,10),".csv")
  
  #round 
  PM10<-round(PM10,digits=2)
  PM5<-round(PM5,digits=2)
  Temp<-round(Temp,digits=2)
  Hum<-round(Hum,digits=2)
  
  #combine variables
  Sensor<-data.frame(Time,PM10,PM5,Temp,Hum,Date)
  
  #Different methothods for outliers
  #Sensor$PM10<-runmed(Sensor$PM10,3,algorithm="Turlach",endrule = c("keep"))
  #Sensor$Hum<-runmed(Sensor$Hum,3,algorithm="Turlach",endrule = c("keep"))
  #Sensor$PM5<-runmed(Sensor$PM5,3,algorithm="Turlach",endrule = c("keep"))
  #Sensor$Temp<-runmed(Sensor$Temp,3,algorithm="Turlach",endrule = c("keep"))
  
  #Sensor$PM10[Sensor$PM10 >= min(outlier(Sensor$PM10),na.rm = TRUE)]<-NA
  #Sensor$Hum[Sensor$Hum >= min(outlier(Sensor$Hum))]<-NA
  #Sensor$PM5[Sensor$PM5 >= min(outlier(Sensor$PM5),na.rm = TRUE)]<-NA
  #Sensor$Temp[Sensor$Temp >= min(outlier(Sensor$Temp))]<-NA
  
  #Sensor$PM10[Sensor$PM10>(quantile(Sensor$PM10, probs = seq(0, 1, 0.01), na.rm = TRUE))[100]]<-NaN
  #Sensor$PM5[Sensor$PM5>(quantile(Sensor$PM5, probs = seq(0, 1, 0.01), na.rm = TRUE))[100]]<-NaN
  #Sensor$Temp[Sensor$Temp>(quantile(Sensor$Temp, probs = seq(0, 1, 0.01), na.rm = TRUE))[100]]<-NaN
  #Sensor$Hum[Sensor$Hum>(quantile(Sensor$Hum, probs = seq(0, 1, 0.01), na.rm = TRUE))[100]]<-NaN
  #331.63 yg/m Und für pm 2,5 bei 110
  
  Sensor$PM10[Sensor$PM10 >= 332]<-NA
  Sensor$PM5[Sensor$PM5 >= 110]<-NA
  
  #thresholds
  GrenzzeitPM10<-data.frame(Sensor$Time[Sensor$PM10>=50],Sensor$PM10[Sensor$PM10>=50])
  GrenzzeitPM5<-data.frame(Sensor$Time[Sensor$PM5>=25],Sensor$PM5[Sensor$PM5>=25])
  GrenzwertPM10<-data.frame(Sensor$PM10[Sensor$PM10>=50],Sensor$PM10[Sensor$PM10>=50])
  GrenzwertPM5<-data.frame(Sensor$PM5[Sensor$PM5>=25],Sensor$PM5[Sensor$PM5>=25])
  assign(paste0("GrenzwertPM10",substr (f[i],1,10)),GrenzwertPM10)
  assign(paste0("GrenzwertPM5",substr (f[i],1,10)),GrenzwertPM5)
  
  
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
  
  rse[i,1]<-round((sd(Sensor$PM10,na.rm = TRUE)/sqrt(length(c(sd(Sensor$PM10,na.rm=TRUE)))))/mean(Sensor$PM10,na.rm = TRUE),digits = 2)
  rse[i,2]<-round((sd(Sensor$PM5,na.rm = TRUE)/sqrt(length(c(sd(Sensor$PM5,na.rm=TRUE)))))/mean(Sensor$PM5,na.rm = TRUE),digits = 2)
  rse[i,3]<-round((sd(Sensor$Temp,na.rm = TRUE)/sqrt(length(c(sd(Sensor$Temp,na.rm=TRUE)))))/mean(Sensor$Temp,na.rm = TRUE),digits = 2)
  rse[i,4]<-round((sd(Sensor$Hum,na.rm = TRUE) /sqrt(length(c(sd(Sensor$Hum,na.rm=TRUE)))))/mean(Sensor$Hum,na.rm = TRUE),digits = 2)
  
  #test normal distribution
 # normal[i,1]<-toString(shapiro.test(Stundenwerte(`Sensor1-7132425`)$PM10)$p.value) > 0.05
  #normal[i,2]<-toString(shapiro.test(Stundenwerte(`Sensor1-7132425`)$PM5)$p.value)>0.05
 # normal[i,3]<-toString(shapiro.test(Stundenwerte(`Sensor1-7132425`)$Temp)$p.value)>0.05
#  normal[i,4]<-toString(shapiro.test(Stundenwerte(`Sensor1-7132425`)$Hum)$p.value)>0.05
  
  #spearman correlation
  spearman[i,1]<-cor.test(Sensor$Hum,Sensor$PM10,method="spearman")[["estimate"]]
  spearman[i,2]<-cor.test(Sensor$Hum,Sensor$PM5,method="spearman")[["estimate"]]
  spearman[i,3]<-cor.test(Sensor$PM5,Sensor$PM10,method="spearman")[["estimate"]]
  spearman[i,4]<-cor.test(Sensor$Temp,Sensor$PM10,method="spearman")[["estimate"]]
  spearman[i,5]<-cor.test(Sensor$Temp,Sensor$PM5,method="spearman")[["estimate"]]
  spearman[i,6]<-cor.test(Sensor$Hum,Sensor$Temp,method="spearman")[["estimate"]]
  
  #pearson correlation
  pearson[i,1]<-cor.test(Sensor$Hum,Sensor$PM10,method="pearson")[["estimate"]]
  pearson[i,2]<-cor.test(Sensor$Hum,Sensor$PM5,method="pearson")[["estimate"]]
  pearson[i,3]<-cor.test(Sensor$PM5,Sensor$PM10,method="pearson")[["estimate"]]
  pearson[i,4]<-cor.test(Sensor$Temp,Sensor$PM10,method="pearson")[["estimate"]]
  pearson[i,5]<-cor.test(Sensor$Temp,Sensor$PM5,method="pearson")[["estimate"]]
  pearson[i,6]<-cor.test(Sensor$Hum,Sensor$Temp,method="pearson")[["estimate"]]
  
  #regression cubic transformation
  cubic[i,1]<-summary(lm(Sensor$Hum ~ poly(Sensor$PM10, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,2]<-summary(lm(Sensor$Hum ~ poly(Sensor$PM5, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,3]<-summary(lm(Sensor$PM5 ~ poly(Sensor$PM10, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,4]<-summary(lm(Sensor$Temp ~ poly(Sensor$PM10, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,5]<-summary(lm(Sensor$Temp ~ poly(Sensor$PM5, 3, raw=TRUE)))[["adj.r.squared"]]
  cubic[i,6]<-summary(lm(Sensor$Hum ~ poly(Sensor$Temp, 3, raw=TRUE)))[["adj.r.squared"]]
  #regression no transformation
  #cubic[i,1]<-summary(lm(Sensor$Hum ~ Sensor$PM10))[["adj.r.squared"]]
  #cubic[i,2]<-summary(lm(Sensor$Hum ~ Sensor$PM5))[["adj.r.squared"]]
  #cubic[i,3]<-summary(lm(Sensor$PM5 ~ Sensor$PM10))[["adj.r.squared"]]
  #cubic[i,4]<-summary(lm(Sensor$Temp ~ Sensor$PM10))[["adj.r.squared"]]
  #cubic[i,5]<-summary(lm(Sensor$Temp ~ Sensor$PM5))[["adj.r.squared"]]
  #cubic[i,6]<-summary(lm(Sensor$Hum ~ Sensor$Temp))[["adj.r.squared"]]
  
  #write values to dataframe
  assign(paste0("Sensor",substr (f[i],1,10)),Sensor)
  liste<-c(liste,(paste0("Sensor",substr (f[i],1,10))))
}
cubic<-round(cubic,digits=2)
linear<-round(linear,digits=2)
pearson<-round(pearson,digits=2)
spearman<-round(spearman,digits=2)
 
##################################################
#
#Data prearation
#
#####################################################
########## create hourly and daily values
liste2<-c()
Tagesmittel<-c()

for (i in 1:length(liste))
{ 
  s<-get(liste[i])
  result<-Stundenwerte(s)
  result2<-Tageswerte(s)
  assign(paste0("Stunde",liste[i]), result)
  assign(paste0("Tag",liste[i]), result2)
  liste2<-rbind(liste2,paste0("Stunde",liste[i]))
  Tagesmittel<-rbind(Tagesmittel,paste0("Tag",liste[i]))
} 
#print all daily maxima of PM10
for (i in 1:length(Tagesmittel)) {
  v<-get(Tagesmittel[i])
  m<-max(v$PM10,na.rm=TRUE)
  print(m)}
#print all daily maxima of PM25
for (i in 1:length(Tagesmittel)) {
  v<-get(Tagesmittel[i])
  m<-max(v$PM5,na.rm=TRUE)
  print(m)}

#stats for houry values
mittel2<-median2<-min2<-max2<-sd2<-var2<-rse2<-matrix(rep(0,times=length(f)),ncol=4,nrow =length(f) )
colnames(max2)<-colnames(mittel2)<-colnames(median2)<-colnames(min2)<-colnames(max2)<-colnames(sd2)<-colnames(var2)<-c("PM10","PM5","Temp","Hum")
rownames(max2)<-rownames(mittel2)<-rownames(median2)<-rownames(min2)<-rownames(max2)<-rownames(sd2)<-rownames(var2)<-f

for (i in 1:length(liste))
{ 
  Sensor<-get(liste2[i])
  mittel2[i,1]<-round(mean(Sensor$PM10,na.rm = TRUE),digits = 2)
  mittel2[i,2]<-round(mean(Sensor$PM5,na.rm = TRUE),digits = 2)
  mittel2[i,3]<-round(mean(Sensor$Temp,na.rm = TRUE),digits = 2)
  mittel2[i,4]<-round(mean(Sensor$Hum,na.rm = TRUE),digits = 2)
  
  median2[i,1]<-round(median(Sensor$PM10,na.rm = TRUE),digits = 2)
  median2[i,2]<-round(median(Sensor$PM5,na.rm = TRUE),digits = 2)
  median2[i,3]<-round(median(Sensor$Temp,na.rm = TRUE),digits = 2)
  median2[i,4]<-round(median(Sensor$Hum,na.rm = TRUE),digits = 2)
  
  min2[i,1]<-round(min(Sensor$PM10,na.rm = TRUE),digits = 2)
  min2[i,2]<-round(min(Sensor$PM5,na.rm = TRUE),digits = 2)
  min2[i,3]<-round(min(Sensor$Temp,na.rm = TRUE),digits = 2)
  min2[i,4]<-round(min(Sensor$Hum,na.rm = TRUE),digits = 2)
  
  max2[i,1]<-round(max(Sensor$PM10,na.rm = TRUE),digits = 2)
  max2[i,2]<-round(max(Sensor$PM5,na.rm = TRUE),digits = 2)
  max2[i,3]<-round(max(Sensor$Temp,na.rm = TRUE),digits = 2)
  max2[i,4]<-round(max(Sensor$Hum,na.rm = TRUE),digits = 2)  
  
  sd2[i,1]<-round(sd(Sensor$PM10,na.rm = TRUE),digits = 2)
  sd2[i,2]<-round(sd(Sensor$PM5,na.rm = TRUE),digits = 2)
  sd2[i,3]<-round(sd(Sensor$Temp,na.rm = TRUE),digits = 2)
  sd2[i,4]<-round(sd(Sensor$Hum,na.rm = TRUE),digits = 2)  
  
  var2[i,1]<-round(var(Sensor$PM10,na.rm = TRUE),digits = 2)
  var2[i,2]<-round(var(Sensor$PM5,na.rm = TRUE),digits = 2)
  var2[i,3]<-round(var(Sensor$Temp,na.rm = TRUE),digits = 2)
  var2[i,4]<-round(var(Sensor$Hum,na.rm = TRUE),digits = 2)  
} 

#one sensor over whole time period
#dummy<-data.frame(`StundeSensor6-7134028`$Date,`StundeSensor6-7134028`$PM10)#summer 
dummy<-data.frame(`Sensor12-7134748`$Date,`Sensor12-7134748`$PM10)#fall
#dummy<-data.frame(`StundeSensor10-7131453`$Date,`StundeSensor10-7131453`$PM10)#winter

names(dummy)<-c("Date","dummy")

#merging  Data in one Dataframe
s<-dummy
PM10<-PM5<-Hum<-Temp<-data.frame(s$Date,s$dummy)
names(PM10)<-c("Date","dummy")
names(PM5)<-c("Date","dummy")
names(Hum)<-c("Date","dummy")
names(Temp)<-c("Date","dummy")

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

######only run for seasonal comparison !!!!!!!
#summer
PM10<-PM10[-c(3,4,9,10,13,14,15,16,18)]
PM5<-PM5[-c(3,4,9,10,13,14,15,16,18)]
#fall
PM10<-PM10[-c(3,4,7,11,12,14,15,16,17,18)]
PM5<-PM5[-c(3,4,7,11,12,14,15,16,17,18)]
#Winter
PM10<-PM10[-c(7)]
PM5<-PM5[-c(7)]

##############################################################
#
#stats
#
##############################################################

#mean value of all sensors at a specific time
RMPM10<-rowMeans(PM10[-c(1)], na.rm=TRUE)
RMPM5<-rowMeans(PM5[-c(1)], na.rm=TRUE)
RMHum<-rowMeans(Hum[-c(1)], na.rm=TRUE)
RMTemp<-rowMeans(Temp[-c(1)], na.rm=TRUE)

#add date RUN ONLY FOR EXPORT!!
PM10null<-PM10
PM10null[is.na(PM10)==T]<-0
PM5null<-PM5
PM5null[is.na(PM5)==T]<-0
RMPM10<-rowMeans(PM10null[-c(1)])
RMPM5<-rowMeans(PM5null[-c(1)])
RMPM10d<-data.frame(RMPM10,dummy$Date)
RMPM5d<-data.frame(RMPM5,dummy$Date)

#write it
write.csv(RMPM5d,file="winter_pm25_mittel.csv")
write.csv(RMPM10d,file="winter_pm10_mittel.csv")

#stats
#mean value of all sensors at a specific time
Statistik(RMPM10)
Statistik(RMPM5)
Statistik(RMHum)
Statistik(RMTemp)

#mean value for every sensor over whole time period
CMPM10<-colMeans(PM10[-c(1)], na.rm=TRUE)
CMPM5<-colMeans(PM5[-c(1)], na.rm=TRUE)
CMHum<-colMeans(Hum[-c(1)], na.rm=TRUE)
CMTemp<-colMeans(Temp[-c(1)], na.rm=TRUE)

#write values aggregated by hour
for (i in 1:19)
{ 
  data<-get(liste[i])
  data<-Stundenwerte(data)

    #write data set path
 # write.csv(data,file=(paste0("E:/Dropbox/Dropbox/Masterarbeit/UnKorr","/Sommer",substr (f[i],1,10),".csv")))
 # write.csv(data,file=(paste0("E:/Dropbox/Dropbox/Masterarbeit/UnKorr","/Herbst",substr (f[i],1,10),".csv")))
} 

##################################################
#
#Plots
#
##################################################

# scatter plots
windows()
plot(RMPM10~RMPM5,xlab="PM 2.5 µg/m³", ylab="PM 10 µg/m³", lwd = 1.5, main="Beziehung zwischen gemittelten PM 10 und PM 2,5 Werten",cex.axis=1.5,cex.lab=1.5)
plot(RMPM10~RMHum,xlab="Luftfeuchtigkeit %", ylab="PM 10 µg/m³", lwd = 1.5, main="Beziehung zwischen den gemittelten PM 10 Werten und der Luftfeuchtigkeit",cex.axis=1.5,cex.lab=1.5)
plot(RMPM10~RMTemp,xlab="Temperatur °C", ylab="PM 10 µg/m³", lwd = 1.5, main="Beziehung zwischen den gemittelten PM 10 Werten und Temperatur in °C",cex.axis=1.5,cex.lab=1.5)
plot(RMPM5~RMTemp,xlab="Temperatur °C", ylab="PM 2,5 µg/m³", lwd = 1.5, main="Beziehung zwischen den gemittelten PM 2,5 Werten und Temperatur in °C",cex.axis=1.5,cex.lab=1.5)
plot(RMPM5~RMHum,xlab="Luftfeuchtigkeit %", ylab="PM 2,5 µg/m³", lwd = 1.5, main="Beziehung zwischen den gemittelten PM 2,5 Werten und der Luftfeuchtigkeit",cex.axis=1.5,cex.lab=1.5)
plot(RMTemp~RMHum,xlab="Luftfeuchtigkeit %", ylab="Temperatur °C", lwd = 1.5, main="Beziehung zwischen der Temperatur und der Luftfeuchtigkeit",cex.axis=1.5,cex.lab=1.5)

#####################################################################
#
#plot fall
#
#######################################################################
time<-as.numeric(dummy$Date)
################################
#plotloop  Temperatur 
windows()
plot(time,Temp$`Sensor6-7134028`,ylim=c(0,50), type="l", col = "magenta",cex.axis=1.5,cex.lab=1.5, xlab="Zeit in Stunden", ylab="Temperatur �C", lwd = 1.5, main="Ver�nderung der Temperatur nach Messger�ten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,Temp[,k], xlab="",ylab="",type="l",col = "magenta", axes=FALSE,ylim=c(0,50))
par(new=T)}
par(new=T)
plot(time,RMTemp, xlab="",ylab="",type="l",col = "black", axes=FALSE ,ylim=c(0,50),lwd=1)

#plotloop  Humidity 
windows()
plot(time,Hum$`Sensor6-7134028`,ylim=c(15,100), type="l", col = "magenta", xlab="Zeit in Stunden", cex.axis=1.5,cex.lab=1.5, ylab="relative Luftfeuchtigkeit %", lwd = 1.5, main="Ver�nderung der Luftfeuchtigkeit nach Messger�ten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,Hum[,k], xlab="",ylab="",type="l",col = "blue", axes=FALSE,ylim=c(15,100))
par(new=T)}
par(new=T)
plot(time,RMHum, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(15,100),lwd=2)

#plotloop  PM10
windows()
plot(time,PM10$`Sensor6-7134028`,ylim=c(10,50), type="l", col = "magenta", cex.axis=1.5,cex.lab=1.5, xlab="Zeit in Stunden", ylab="PM 10 �g/m�", lwd = 1.5, main="Ver�nderung der Feinstaubkonzentration nach Messger�ten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,PM10[,k], xlab="",ylab="",type="l",col = "darkgreen", axes=FALSE,ylim=c(0,50))
par(new=T)}
par(new=T)
plot(time,RMPM10, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(0,50),lwd=1)

#plotloop  PM25
windows()
plot(time,PM5$`Sensor6-7134028`,ylim=c(0,20), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="PM 2,5 �g/m�", lwd = 1.5, main="Ver�nderung der Feinstaubkonzentration nach Messger�ten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,PM5[,k], xlab="",ylab="",type="l",col = "darkorange", axes=FALSE,ylim=c(0,20))
par(new=T)}
par(new=T)
plot(time,RMPM5, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(0,20),lwd=1)



#####################################################################
#
#plot summer
#
#######################################################################
time<-as.numeric(dummy$Date)
################################
#plotloop  Temperatur 
windows()
plot(time,Temp$`Sensor6-7134028`,ylim=c(10,50), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="Temperatur °C", lwd = 1.5, main="Veränderung der Temperatur nach Messgeräten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,Temp[,k], xlab="",ylab="",type="l",col = "magenta", axes=FALSE,ylim=c(10,50))
par(new=T)}
par(new=T)
plot(time,RMTemp, xlab="",ylab="",type="l",col = "black", axes=FALSE ,ylim=c(10,50),lwd=2)

#plotloop  Humidity 
windows()
plot(time,Hum$`Sensor6-7134028`,ylim=c(15,100), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="Luftfeuchtigkeit %", lwd = 1.5, main="Veränderung der Luftfeuchtigkeit nach Messgeräten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,Hum[,k], xlab="",ylab="",type="l",col = "blue", axes=FALSE,ylim=c(15,100))
par(new=T)}
par(new=T)
plot(time,RMHum, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(15,100),lwd=2)

#plotloop  PM10
windows()
plot(time,PM10$`Sensor6-7134028`,ylim=c(0,50), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="PM 10 µg/m³", lwd = 1.5, main="Veränderung der Feinstaubkonzentration nach Messgeräten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,PM10[,k], xlab="",ylab="",type="l",col = "darkgreen", axes=FALSE,ylim=c(0,50))
par(new=T)}
par(new=T)
plot(time,RMPM10, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(0,50),lwd=1)

#plotloop  PM10
windows()
plot(time,PM5$`Sensor6-7134028`,ylim=c(0,20), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="PM 2,5 µg/m³", lwd = 1.5, main="Veränderung der Feinstaubkonzentration nach Messgeräten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,PM5[,k], xlab="",ylab="",type="l",col = "darkorange", axes=FALSE,ylim=c(0,20))
par(new=T)}
par(new=T)
plot(time,RMPM5, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(0,20),lwd=1)


#####################################################################
#
#plot winter
#
#######################################################################
dummy<-data.frame(`StundeSensor1-7132425`$Date,`StundeSensor1-7132425`$PM10)#fall and summer 
time<-as.numeric(dummy$Date)
################################
#plotloop  Temperatur 
windows()
plot(time,Temp$`Sensor1-7132425`,ylim=c(10,50), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="Temperatur °C", lwd = 1.5, main="Veränderung der Temperatur nach Messgeräten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,Temp[,k], xlab="",ylab="",type="l",col = "magenta", axes=FALSE,ylim=c(10,50))
par(new=T)}
par(new=T)
plot(time,RMTemp, xlab="",ylab="",type="l",col = "black", axes=FALSE ,ylim=c(10,50),lwd=2)

#plotloop  Humidity 
windows()
plot(time,Hum$`Sensor1-7132425`,ylim=c(15,100), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="Luftfeuchtigkeit %", lwd = 1.5, main="Veränderung der Luftfeuchtigkeit nach Messgeräten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,Hum[,k], xlab="",ylab="",type="l",col = "blue", axes=FALSE,ylim=c(15,100))
par(new=T)}
par(new=T)
plot(time,RMHum, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(15,100),lwd=2)

#plotloop  PM10
windows()
plot(time,PM10$`Sensor1-7132425`,ylim=c(0,50), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="PM 10 �g/m�", lwd = 1.5, main="Ver�nderung der Feinstaubkonzentration nach Messgeräten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,PM10[,k], xlab="",ylab="",type="l",col = "darkgreen", axes=FALSE,ylim=c(0,50))
par(new=T)}
par(new=T)
plot(time,RMPM10, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(0,50),lwd=1)

#plotloop  PM10
windows()
plot(time,PM5$`Sensor1-7132425`,ylim=c(0,20), type="l", col = "magenta", xlab="Zeit in Stunden",cex.axis=1.5,cex.lab=1.5, ylab="PM 2,5 �g/m�", lwd = 1.5, main="Ver�nderung der Feinstaubkonzentration nach Messgeräten")
par(new=T)
for (i in 1:18)
{k=i+1
plot(time,PM5[,k], xlab="",ylab="",type="l",col = "darkorange", axes=FALSE,ylim=c(0,20))
par(new=T)}
par(new=T)
plot(time,RMPM5, xlab="",ylab="",type="l",col = "red", axes=FALSE ,ylim=c(0,20),lwd=1)






##################################################
#
# compare seasons
#
##################################################
#Sensors Working through all seasons
jahreszeiten<-c("StundeSensor1-7132425","StundeSensor12-7134748","StundeSensor13-7132608", "StundeSensor15-7136444","StundeSensor19-7133183", "StundeSensor2-7133765","StundeSensor8-7131428" )
#one sensor over whole time period
dummy1<-data.frame(`StundeSensor6-7134028`$Date,`StundeSensor6-7134028`$PM10)#fall and summer 
dummy2<-data.frame(`StundeSensor1-7132425`$Date,seq(1:2184))#fall and summer 



#run with Data

#run with Data2

#run with Data3 






