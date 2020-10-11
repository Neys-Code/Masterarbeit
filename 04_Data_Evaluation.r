############################################################
#
#Data Evaluation
#
#######################################################
#my libary #set right path!
source("E:\\Dropbox\\Dropbox\\Masterarbeit\\02_R-Code\\abgabe\\lib.r")
# read humidity corrected data 
# set working directory for different machines 

#choose data provided by script 2 or 3 / without or with hunidity correction 
#pfad<-("C:/Users/jana/Dropbox/Masterarbeit/Korr/Stunden")#laptop
#pfad<-("E:/Dropbox/Dropbox/Masterarbeit/Korr/Stunden")#desktop1
pfad<-("E:/Dropbox/Dropbox/Masterarbeit/UnKorr")#desktop1

setwd(pfad)# set woring directory
# Longlist on Directory
f<-list.files(path = pfad, pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
liste<-c()
for (i in 1:length(f))
{
  x<-read.csv(f[i],sep = ",")
  assign(substr(f[i],1,16),x)
  liste[i]<-substr(f[i],1,16)
}
#split Fall an summer
Sommer<-liste[18:35]
Herbst<-liste[1:17]

#split fall and Summer
l1<-Sommer[-c(6,15)]
l2<-Herbst[-c(17)]

#############################################################
#
#Daily means and thresholds
#
#############################################################

a<-aggregate(`c.SommerSensor6-`$PM10 ~ `c.SommerSensor6-`$Tag, `c.SommerSensor6-`, mean)
dummy1<-a[1]#sommer
colnames(dummy1)<-c("data.Tag")

LM<-Sommer
LM2<-Herbst
liste8<-c()
liste8h<-c()
for (i in 1:length(LM))
{
  data<-get(LM[i])
  a1<-aggregate(data$PM10 ~ data$Tag, data, mean)
  a2<-aggregate(data$PM5 ~ data$Tag, data, mean)
  a3<-aggregate(data$Temp ~ data$Tag, data, mean)
  a4<-aggregate(data$Hum ~ data$Tag, data, mean)
  result<-data.frame(a1[1],a1[2],a2[2],a3[2],a4[2])
  merge(x=dummy1, y= result, by= 'data.Tag', all.x= T)
  names(result)<-c("Tag","PM10","PM5","Temp","Hum")
  assign(paste0("S.Tagesmittel",substr(LM[i],15,25)),result)
  liste8[i]<-paste0("S.Tagesmittel",substr(LM[i],15,25))
}

a<-aggregate(c.HerbstSensor10$PM10 ~ c.HerbstSensor10$Tag, c.HerbstSensor10, mean)
dummy2<-a[1]#fall
colnames(dummy2)<-c("data.Tag")

for (i in 1:length(LM2))
{
  data<-get(LM2[i])
  a1<-aggregate(data$PM10 ~ data$Tag, data, mean)
  a2<-aggregate(data$PM5 ~ data$Tag, data, mean)
  a3<-aggregate(data$Temp ~ data$Tag, data, mean)
  a4<-aggregate(data$Hum ~ data$Tag, data, mean)
  result<-data.frame(a1[1],a1[2],a2[2],a3[2],a4[2])
  merge(x=dummy2, y= result, by= 'data.Tag', all.x= T)
  names(result)<-c("Tag","PM10","PM5","Temp","Hum")
  assign(paste0("H.Tagesmittel",substr(LM2[i],15,25)),result)
  liste8h[i]<-paste0("H.Tagesmittel",substr(LM2[i],15,25))
}

# merge in one Dataset
#create dummy table to be filled
dummy3<-data.frame(`S.Tagesmittel6-`$Tag,`S.Tagesmittel6-`$PM10)##summer
dummy4<-data.frame(H.Tagesmittel10$Tag,H.Tagesmittel10$PM10)#fall
names(dummy3)<-c("Tag","dummy")
names(dummy4)<-c("Tag","dummy")
s<-dummy3
s2<-dummy4
S.TM.PM10<-S.TM.PM5<-S.TM.Temp<-S.TM.Hum<-data.frame(s$Tag,s$dummy)
H.TM.PM10<-H.TM.PM5<-H.TM.Temp<-H.TM.Hum<-data.frame(s2$Tag,s2$dummy)
names(S.TM.PM10)<-c("Tag","dummy")
names(S.TM.PM5)<-c("Tag","dummy")
names(S.TM.Temp)<-c("Tag","dummy")
names(S.TM.Hum)<-c("Tag","dummy")
names(H.TM.PM10)<-c("Tag","dummy")
names(H.TM.PM5)<-c("Tag","dummy")
names(H.TM.Temp)<-c("Tag","dummy")
names(H.TM.Hum)<-c("Tag","dummy")

#merging  Data summer
for (i in 1:length(liste8))
{ 
  s5<-get(liste8[i])
  S.TM.PM10<-merge(x=S.TM.PM10 , y= s5[1:2], by= 'Tag', all.x= T)
  S.TM.PM5<-merge(x=S.TM.PM5 , y=s5[,c(1,3)] , by= 'Tag', all.x= T)
  S.TM.Hum<-merge(x=S.TM.PM10 , y= s5[,c(1,4)], by= 'Tag', all.x= T)
  S.TM.Temp<-merge(x=S.TM.PM5 , y=s5[,c(1,5)] , by= 'Tag', all.x= T)
  names(S.TM.PM10)<-c("Tag","dummy",liste8[1:i])
  names(S.TM.PM5)<-c("Tag","dummy",liste8[1:i])
} 
S.TM.PM10<-S.TM.PM10[-c(2)]
S.TM.PM5<-S.TM.PM5[-c(2)]
S.TM.Hum<-S.TM.Hum[-c(2)]
S.TM.Temp<-S.TM.Temp[-c(2)]

#merging  Data fall
for (i in 1:length(liste8h))
{ 
  s6<-get(liste8h[i])
  H.TM.PM10<-merge(x=H.TM.PM10 , y= s6[1:2], by= 'Tag', all.x= T)
  H.TM.PM5<-merge(x=H.TM.PM5 , y=s6[,c(1,3)] , by= 'Tag', all.x= T)
  H.TM.Hum<-merge(x=H.TM.Hum , y= s6[,c(1,4)], by= 'Tag', all.x= T)
  H.TM.Temp<-merge(x=H.TM.Temp , y=s6[,c(1,5)] , by= 'Tag', all.x= T)
  names(H.TM.PM10)<-c("Tag","dummy",liste8h[1:i])
  names(H.TM.PM5)<-c("Tag","dummy",liste8h[1:i])
} 
H.TM.PM10<-H.TM.PM10[-c(2)]
H.TM.PM5<-H.TM.PM5[-c(2)]
H.TM.Hum<-H.TM.Hum[-c(2)]
H.TM.Temp<-H.TM.Temp[-c(2)]

#write data
write.csv(S.TM.PM10,file="PM10Sommer_TM.csv")
write.csv(S.TM.PM5,file="PM25Sommer_TM.csv")
write.csv(S.TM.Temp,file="TempSommer_TM.csv")
write.csv(S.TM.Hum,file="HumSommer_TM.csv")

write.csv(H.TM.PM10,file="PM10Herbst_TM.csv")
write.csv(H.TM.PM5,file="PM25Herbst_TM.csv")
write.csv(H.TM.Temp,file="TempHerbst_TM.csv")
write.csv(H.TM.Hum,file="HumHerbst_TM.csv")

##############################################
#maps

for (i in 1:length(liste8))
{ 
  s5<-get(liste8[i])
  S.TM.PM10<-merge(x=S.TM.PM10 , y= s5[1:2], by= 'Tag', all.x= T)
  S.TM.PM5<-merge(x=S.TM.PM5 , y=s5[,c(1,3)] , by= 'Tag', all.x= T)
  S.TM.Hum<-merge(x=S.TM.PM10 , y= s5[,c(1,4)], by= 'Tag', all.x= T)
  S.TM.Temp<-merge(x=S.TM.PM5 , y=s5[,c(1,5)] , by= 'Tag', all.x= T)
  
  write.csv(S.TM.PM10,file=paste0(liste8[i]))
  write.csv(S.TM.PM5,file=paste0(liste8[i]))
  write.csv(S.TM.Temp,file=paste0(liste8[i]))
  write.csv(S.TM.Hum,file=paste0(liste8[i]))
  
  names(S.TM.PM10)<-c("Tag","dummy",liste8[1:i])
  names(S.TM.PM5)<-c("Tag","dummy",liste8[1:i])
} 


#merging  Data fall
for (i in 1:length(liste8h))
{ 
  s6<-get(liste8h[i])
  H.TM.PM10<-merge(x=H.TM.PM10 , y= s6[1:2], by= 'Tag', all.x= T)
  H.TM.PM5<-merge(x=H.TM.PM5 , y=s6[,c(1,3)] , by= 'Tag', all.x= T)
  H.TM.Hum<-merge(x=H.TM.Hum , y= s6[,c(1,4)], by= 'Tag', all.x= T)
  H.TM.Temp<-merge(x=H.TM.Temp , y=s6[,c(1,5)] , by= 'Tag', all.x= T)
  
  write.csv(H.TM.PM10,file="PM10Herbst_TM.csv")
  write.csv(H.TM.PM5,file="PM25Herbst_TM.csv")
  write.csv(H.TM.Temp,file="TempHerbst_TM.csv")
  write.csv(H.TM.Hum,file="HumHerbst_TM.csv")
  
  names(H.TM.PM10)<-c("Tag","dummy",liste8h[1:i])
  names(H.TM.PM5)<-c("Tag","dummy",liste8h[1:i])
} 


#######################################################################
#compare to Trier Ostalee and Pfalzel (threshold)
#######################################################################
#set path
ZIMEN_Sommer<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Sommer.csv", stringsAsFactors=FALSE)
ZIMEN_Herbst<- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Herbst.csv", stringsAsFactors=FALSE)

ZIMEN_Sommer$Tag<-substr(ZIMEN_Sommer$Zeitpunkt,1,10)
ZIMEN_Herbst$Tag<-substr(ZIMEN_Herbst$Zeitpunkt,1,10)
ZIMEN_Sommer$Trier.Ostallee.PM10<-as.numeric(ZIMEN_Sommer$Trier.Ostallee.PM10)
ZIMEN_Herbst$Trier.Ostallee.PM10<-as.numeric(ZIMEN_Herbst$Trier.Ostallee.PM10)
ZIMEN_Sommer$Trier.Pfalzel.PM2.5<-as.numeric(ZIMEN_Sommer$Trier.Pfalzel.PM2.5)
ZIMEN_Herbst$Trier.Pfalzel.PM2.5<-as.numeric(ZIMEN_Herbst$Trier.Pfalzel.PM2.5)

ZIMEN_Sommer_TM_PM10<- aggregate(ZIMEN_Sommer$Trier.Ostallee.PM10 ~ ZIMEN_Sommer$Tag, ZIMEN_Sommer, mean)
ZIMEN_Sommer_TM_PM25<- aggregate(ZIMEN_Sommer$Trier.Pfalzel.PM2.5 ~ ZIMEN_Sommer$Tag, ZIMEN_Sommer, mean)

ZIMEN_Herbst_TM_PM10<- aggregate(ZIMEN_Herbst$Trier.Ostallee.PM10 ~ ZIMEN_Herbst$Tag, ZIMEN_Herbst, mean)
ZIMEN_Herbst_TM_PM25<- aggregate(ZIMEN_Herbst$Trier.Pfalzel.PM2.5 ~ ZIMEN_Herbst$Tag, ZIMEN_Herbst, mean)

#maximum values
Statistik(ZIMEN_Sommer_TM_PM10$`ZIMEN_Sommer$Trier.Ostallee.PM10`)
Statistik(ZIMEN_Sommer_TM_PM25$`ZIMEN_Sommer$Trier.Pfalzel.PM2.5`)
  
Statistik(ZIMEN_Herbst_TM_PM10$`ZIMEN_Herbst$Trier.Ostallee.PM10`)
Statistik(ZIMEN_Herbst_TM_PM25$`ZIMEN_Herbst$Trier.Pfalzel.PM2.5`)

##############################################################
#
# Inversions
#
##################################################################
#set directory to DWD data
#Temperatur<-read.csv("E:\\Dropbox\\Dropbox\\Masterarbeit\\inversion.csv",sep=",")
Temperatur<-read.csv("E:\\Dropbox\\Dropbox\\Masterarbeit\\inversion_wz.csv",sep=",")
####create difference
T.diff<-Temperatur$Petrisberg-Temperatur$Ostallee
inversion=rep(0,times=length(T.diff))
INV<-data.frame(Temperatur,T.diff,inversion)
INV$inversion[INV$T.diff>0]<-1

#remove all non iversion rows
INV_true<-INV
INV_true<-subset(INV_true, INV$inversion==1)
# how many inversions
dim(INV_true)
#--> 202 Inversion Hours
#gesamt
#Juni	17
#Juli	12
#August	30
#September	34
#Oktober	83
#November	26
monate<-as.factor(c(rep("Juni",times=17),rep("Juli"	,times=12),rep("August",	times=30),rep("September",times=34),rep("Oktober",	times=83),rep("November",	times=26)))
monate.num<-c(rep(6,times=17),rep(7	,times=12),rep(8,	times=30),rep(9,times=34),rep(10,	times=83),rep(11,	times=26))
mo<-data.frame(monate.num,monate)
INV_true<-data.frame(INV_true,monate)

#Hours with inversion
H.monate<-table(monate.num)
H.monate<-matrix(c(17,12,30,34,83,26),ncol=6)
colnames(H.monate) <-c("Juni","Juli","August","September","Oktober","November")

H.monate_hoch<-table(monate.num[INV_true$T.diff>1])
H.monate_hoch <- matrix(c(1,1,0,3,31,9), ncol=6)
colnames(H.monate_hoch) <-c("Juni","Juli","August","September","Oktober","November")

H.Uhrzeit<-table(INV_true$Stunde)
H.Uhrzeit_hoch<-table(INV_true$Stunde[INV_true$T.diff>1])#values over 1

# barplots
barplot(H.Uhrzeit, col="blue",ylim=c(0,19),main="Häufigkeitsverteilung von Inversionen nach Uhrzeit",xlab = "Uhrzeit",ylab="Häufigkeit",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5)
par(new=T)
barplot(H.Uhrzeit_hoch, col="red",ylim=c(0,19),cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5)
legend(16,19,legend=c("Alle Inversionen","Inversionen mit Differenz > 1"),col=c("blue","red"), lty=1:1,lwd=3, cex=1.3,text.font=4)

barplot(H.monate, col="blue",ylim=c(0,93),main="Häufigkeitsverteilung von Inversionen nach Monat",xlab = "Monat",ylab="Häufigkeit",cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5)
par(new=T)
barplot(H.monate_hoch, col="red",ylim=c(0,93),cex.main=1.5,cex.names = 1.5, cex.axis = 1.5, cex.lab=1.5)
legend(0,93,legend=c("Alle Inversionen","Inversionen mit Differenz > 1"),col=c("blue","red"), lty=1:1,lwd=3, cex=1.3,text.font=4)

##########################################

#correlartions with low lying stations
#city stations

# compare to Sensors 
vgl<-read.csv("E:\\Dropbox\\Dropbox\\Masterarbeit\\inversion_pm.csv",sep=",")

T.diff<-vgl$Petrisberg-vgl$Ostallee
inversion=rep(0,times=length(T.diff))
INV<-data.frame(vgl,T.diff,inversion)
INV$inversion[INV$T.diff>0]<-1
INV$T.diff[INV$T.diff<0]<-0

#model correlation
#sensor2
m<-lm(INV$T.diff~INV$S2.PM10)
m2<-cor.test(INV$T.diff,INV$S2.PM10, method = c("spearman"))
m3<-lm(INV$T.diff~INV$S2.PM25)
m4<-cor.test(INV$T.diff,INV$S2.PM25, method = c("spearman"))
#sensor4
m5<-lm(INV$T.diff~INV$S4.PM10)
m6<-cor.test(INV$T.diff,INV$S4.PM10, method = c("spearman"))
m7<-lm(INV$T.diff~INV$S4.PM25)
m8<-cor.test(INV$T.diff,INV$S4.PM25, method = c("spearman"))
#sensor mean
m9<-lm(INV$T.diff~INV$Mean.PM10)
m10<-cor.test(INV$T.diff,INV$Mean.PM10, method = c("spearman"))
m11<-lm(INV$T.diff~INV$Mean.PM25)
m12<-cor.test(INV$T.diff,INV$Mean.PM25, method = c("spearman"))


#subset data to true or false inversion
INV_true<-subset(INV, INV$inversion==1)
Statistik(INV$S2.PM10)#mean 4.76
Statistik(INV$S2.PM25)#mean 2.58
Statistik(INV$Mean.PM10)#mean 3.91 
Statistik(INV$Mean.PM25)#mean 2.11 

# right of median 3.92 and 1.91
medi<- rep(0,times=c(202))
medi.diff<-medi
INV_true<-data.frame(INV_true,medi,medi.diff)
INV_true$medi[INV_true$S2.PM10>3.92]<-1

#divide Dataset in 2 Parts, with Inversion Without inversion
INV_true$Mean.PM10
INV_true$Mean.PM25

length(INV_true$Mean.PM10)
length(INV_true$Mean.PM25)

#quantile 
q1<-quantile(INV$Mean.PM10,'na.rm' = TRUE,probs=c(0,0.1,0.5,0.6,0.7,0.8,0.9))
q2<-quantile(INV$Mean.PM25,'na.rm' = TRUE,probs=c(0,0.1,0.5,0.6,0.7,0.8,0.9))

#how many values are above median or above q3
pm10.q50<-INV_true$Mean.PM10[INV_true$Mean.PM10>=3.22]
pm10.q75<-INV_true$Mean.PM10[INV_true$Mean.PM10>=4.83]
length(pm10.q50)
length(pm10.q75)
pm25.q50<-INV_true$Mean.PM25[INV_true$Mean.PM25>=1.60]
pm25.q75<-INV_true$Mean.PM25[INV_true$Mean.PM25>=2.69]
length(pm25.q50)
length(pm25.q75)

q3<-quantile(INV_true$Mean.PM10,'na.rm' = TRUE)
q4<-quantile(INV_true$Mean.PM25,'na.rm' = TRUE)

#oktober
okt<-read.csv("E:\\Dropbox\\Dropbox\\Masterarbeit\\Oktober_inversion.csv",sep=",")
T.diff<-okt$Petrisberg-okt$Ostallee
inversion=rep(0,times=length(T.diff))
OKT<-data.frame(okt,T.diff,inversion)
OKT$inversion[OKT$T.diff>0]<-1
OKT$T.diff[OKT$T.diff<0]<-0

#correlation Oktober
m9<-lm(OKT$T.diff~OKT$Mean.PM10)
m10<-cor.test(OKT$T.diff,OKT$Mean.PM10, method = c("spearman"))
m11<-lm(OKT$T.diff~OKT$Mean.PM25)
m12<-cor.test(OKT$T.diff,OKT$Mean.PM25, method = c("spearman"))

m9<-lm(OKT$inversion~OKT$Mean.PM10)
m10<-cor.test(OKT$inversion,OKT$Mean.PM10, method = c("spearman"))
m11<-lm(OKT$inversion~OKT$Mean.PM25)
m12<-cor.test(OKT$inversion,OKT$Mean.PM25, method = c("spearman"))

#quantiles oktober
OKT_true<-subset(OKT, OKT$inversion==1)
q5<-quantile(OKT$Mean.PM10,'na.rm' = TRUE)
q6<-quantile(OKT$Mean.PM25,'na.rm' = TRUE)
q7<-quantile(OKT_true$Mean.PM10,'na.rm' = TRUE)
q8<-quantile(OKT_true$Mean.PM25,'na.rm' = TRUE)

#how many values are above median or above q3
pm10.q50<-OKT_true$Mean.PM10[OKT_true$Mean.PM10>=2.31]
pm10.q75<-OKT_true$Mean.PM10[OKT_true$Mean.PM10>=4.01]
length(pm10.q50)
length(pm10.q75)
pm25.q50<-OKT_true$Mean.PM25[OKT_true$Mean.PM25>=0.99]
pm25.q75<-OKT_true$Mean.PM25[OKT_true$Mean.PM25>=1.84]
length(pm25.q50)
length(pm25.q75)


##############################################################
# Are fall values in pm10 and Pm2,5 higher than the Summer values ?
##################################################################

s2<-data.frame(c.HerbstSensor10$Date,c.HerbstSensor10$PM10)#herbst
s1<-data.frame(`c.SommerSensor6-`$Date,`c.SommerSensor6-`$PM10)#sommer
names(s2)<-c("Date","dummy")
names(s1)<-c("Date","dummy")
#merging  Data 
c.s.PM10<-c.s.PM5<-data.frame(s1$Date,s1$dummy)
names(c.s.PM10)<-c("Date","dummy")
names(c.s.PM5)<-c("Date","dummy")
c.h.PM10<-c.h.PM5<-data.frame(s2$Date,s2$dummy)
names(c.h.PM10)<-c("Date","dummy")
names(c.h.PM5)<-c("Date","dummy")

for (i in 1:19)
{ 
  s1<-get(l1[i])
  s2<-get(l2[i])
  c.s.PM10<-merge(x=c.s.PM10 , y= s1[2:3], by= 'Date', all.x= T)
  c.s.PM5<-merge(x=c.s.PM5 , y=s1[,c(2,4)] , by= 'Date', all.x= T)
  names(c.s.PM10)<-c("Date","dummy",Sommer[1:i])
  names(c.s.PM5)<-c("Date","dummy",Sommer[1:i])
  c.h.PM10<-merge(x=c.h.PM10 , y= s2[2:3], by= 'Date', all.x= T)
  c.h.PM5<-merge(x=c.h.PM5 , y=s2[,c(2,4)] , by= 'Date', all.x= T)
  names(c.h.PM10)<-c("Date","dummy",Herbst[1:i])
  names(c.h.PM5)<-c("Date","dummy",Herbst[1:i])
} 
c.s.PM10<-c.s.PM10[-c(2)]
c.s.PM5<-c.s.PM5[-c(2)]
c.h.PM10<-c.h.PM10[-c(2)]
c.h.PM5<-c.h.PM5[-c(2)]

# calculate row means as above
c.s.RMPM10<-rowMeans(c.s.PM10[-c(1)], na.rm=TRUE)
c.s.RMPM5<-rowMeans(c.s.PM5[-c(1)], na.rm=TRUE)
c.h.RMPM10<-rowMeans(c.h.PM10[-c(1)], na.rm=TRUE)
c.h.RMPM5<-rowMeans(c.h.PM5[-c(1)], na.rm=TRUE)

#########
#compare
View(c.h.RMPM10)
View(c.s.RMPM10)

length(c.h.RMPM10)
length(c.s.RMPM10)

# same length
c.s.RMPM10<-c.s.RMPM10[-c(2185:2208)]

plot(c.s.RMPM10~c.h.RMPM10)
#if negativ summer > fall

diff.PM10<-c.h.RMPM10-c.s.RMPM10
plot(diff.PM10)

# mean daily cycle
liste.h<-c()
for (i in 1:length(Herbst))
{
  data<-get(l2[i])
  a1<-aggregate(data$PM10 ~ data$Zeit, data, mean)
  a2<-aggregate(data$PM5 ~ data$Zeit, data, mean)
  a3<-aggregate(data$Temp ~ data$Zeit, data, mean)
  a4<-aggregate(data$Hum ~ data$Zeit, data, mean)
  result<-data.frame(c(0:23),a1[2],a2[2],a3[2],a4[2])
  names(result)<-c("Zeit","PM10","PM5","Temp","Hum")
  assign(paste0("mittel.",l2[i]),result)
  liste.h[i]<-paste0("mittel.",l2[i])
}
liste.s<-c()
for (i in 1:17)
{
  data<-get(l1[i])
  a1<-aggregate(data$PM10 ~ data$Zeit, data, mean)
  a2<-aggregate(data$PM5 ~ data$Zeit, data, mean)
  a3<-aggregate(data$Temp ~ data$Zeit, data, mean)
  a4<-aggregate(data$Hum ~ data$Zeit, data, mean)
  result<-data.frame(c(0:23),a1[2],a2[2],a3[2],a4[2])
  names(result)<-c("Zeit","PM10","PM5","Temp","Hum")
  assign(paste0("mittel.",l1[i]),result)
  liste.s[i]<-paste0("mittel.",l1[i])
}

Standort<-c("Nord","Irsch","Mariahof","Feyen","Quint","Zewen","Gartenfeld","Olewig","Schneidershof","Kernscheid","Maximin","Ruwer","Pfalzel","Ehrang","Tarforst","Irminenfreihof","Campus 2")

#plot for every sensor
par(new=T)
for (i in 1:19)
{windows()
  data2<-get(liste.h[i])
  data1<-get(liste.s[i])
  #main=paste0("Gemittelter Tagesverlauf (Sommer) ",substr(liste8[i],9,10)," ",Standort[i])
  main=paste0("Gemittelter Tagesverlauf Vergleich ",substr(l2[i],15,15)," ",Standort[i])
  par(mar=c(5, 5, 5, 5) + 0.1)
  plot(data1$Zeit,data1$PM5, xlim=c(0,23), ylim=c(min(data2$PM5),max(data1$PM10)),main=main, xlab="Uhrzeit",col="darkgreen",lwd=2,ylab="PM 10 und PM 2.5 µg/m³",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data1$Zeit,data1$PM10,axes=FALSE, xlim=c(0,23),ylim=c(min(data2$PM5),max(data1$PM10)),main=main, xlab="",col="green",lwd=2,ylab="",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data2$Zeit,data2$PM5, xlim=c(0,23), ylim=c(min(data2$PM5),max(data1$PM10)),main=main, xlab="Uhrzeit",col="darkorange",lwd=2,ylab="PM 10 und PM 2.5 µg/m³",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data2$Zeit,data2$PM10,axes=FALSE, xlim=c(0,23),ylim=c(min(data2$PM5),max(data1$PM10)),main=main, xlab="",col="red",lwd=2,ylab="",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  legend(x=1,y=10,legend=c("PM 10 Sommer", "PM 2.5 Sommer","PM 10 Herbst","PM 2.5 Herbst"),lwd=2,col=c("darkgreen","green","darkorange","red"))
  
  dev.copy(png,filename=paste0(main,".png"))
  dev.off ()
}


######################################################################
#longterm daily mean 
######################################################################
#humidity corrected Data (after executing script nr 2)
#liste7<-fall
liste8<-c()
for (i in 1:length(liste7))
{
  data<-get(liste7[i])
  a1<-aggregate(data$PM10 ~ data$Zeit, data, mean)
  a2<-aggregate(data$PM5 ~ data$Zeit, data, mean)
  a3<-aggregate(data$Temp ~ data$Zeit, data, mean)
  a4<-aggregate(data$Hum ~ data$Zeit, data, mean)
  result<-data.frame(c(0:23),a1[2],a2[2],a3[2],a4[2])
  names(result)<-c("Zeit","PM10","PM5","Temp","Hum")
  assign(paste0("c.mittel",substr(liste7[i],15,25)),result)
  liste8[i]<-paste0("c.mittel",substr(liste7[i],15,25))
}

#uncorrected Data after exec. read loop
liste8<-c()
for (i in 1:length(liste))
{
  data<-get(liste[i])
  data<-Stundenwerte(data)
  a1<-aggregate(data$PM10 ~ data$Zeit, data, mean)
  a2<-aggregate(data$PM5 ~ data$Zeit, data, mean)
  a3<-aggregate(data$Temp ~ data$Zeit, data, mean)
  a4<-aggregate(data$Hum ~ data$Zeit, data, mean)
  result<-data.frame(c(0:23),a1[2],a2[2],a3[2],a4[2])
  names(result)<-c("Zeit","PM10","PM5","Temp","Hum")
  assign(paste0("c.mittel",substr(liste[i],7,15)),result)
  liste8[i]<-paste0("c.mittel",substr(liste[i],7,15))
}

#set working directory for output
setwd("E:/Dropbox/Dropbox/Masterarbeit/plots")
#plots
#summer
Standort<-c("Nord","Irsch","Mariahof","Feyen","Quint","Süd","Zewen","Gartenfeld","Olewig","Schneidershof","Kernscheid","Maximin","Ruwer","Pfalzel","Aveler Tal","Ehrang","Tarforst","Irminenfreihof")
#fall
Standort<-c("Nord","Irsch","Mariahof","Feyen","Quint","Zewen","Gartenfeld","Olewig","Schneidershof","Kernscheid","Maximin","Ruwer","Pfalzel","Ehrang","Tarforst","Irminenfreihof","Campus 2")
farben<-c("gold", "darkorange", "red","darkred","hotpink","plum3","darkmagenta","cornflowerblue","blue","darkblue","turquoise","turquoise4","green","darkgreen","tan2","navajowhite4","grey","black")

###plots

#PM10
windows()
plot(`c.mittel8-7131428`$Zeit,`c.mittel8-7131428`$PM10, xlim=c(0,23),ylim=c(1,10),main="Gemittelter Tagesverlauf", xlab="Uhrzeit",lwd=2,ylab="PM 10 µg/m³",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
par(new=T)
for (i in 1:length(liste8))
{data<-get(liste8[i])
plot(data$Zeit,data$PM10,type="b", xlim=c(0,23),ylim=c(1,10),col=farben[i],lwd=2,xlab="",ylab="",axes=FALSE)
par(new=T)}

#PM25
windows()
plot(`c.mittel8-7131428`$Zeit,`c.mittel8-7131428`$PM5, xlim=c(0,23),ylim=c(1,5),main="Gemittelter Tagesverlauf", xlab="Uhrzeit",lwd=2,ylab="PM 2.5 µg/m³",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
par(new=T)
for (i in 1:length(liste8))
{ data<-get(liste8[i])
plot(data$Zeit,data$PM5,type="b", xlim=c(0,23),ylim=c(1,5),col=farben[i],lwd=2,xlab="",ylab="",axes=FALSE)
par(new=T)}

#Temperatur
windows()
plot(`c.mittel8-7131428`$Zeit,`c.mittel8-7131428`$Temp, xlim=c(0,23),ylim=c(5,30),main="Gemittelter Tagesverlauf", xlab="Uhrzeit",lwd=2,ylab="Temperatur °C",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
par(new=T)
for (i in 1:length(liste8))
{data<-get(liste8[i])
plot(data$Zeit,data$Temp,type="b", xlim=c(0,23),ylim=c(5,30),col=farben[i],lwd=2,xlab="",ylab="",axes=FALSE)
par(new=T)}

#luftfeuchtigkeit
windows()
plot(`c.mittel8-7131428`$Zeit,`c.mittel8-7131428`$Hum, xlim=c(0,23),ylim=c(20,100),main="Gemittelter Tagesverlauf", xlab="Uhrzeit",lwd=2,ylab="Temperatur °C",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
par(new=T)
for (i in 1:length(liste8))
{data<-get(liste8[i])
plot(data$Zeit,data$Hum,type="b", xlim=c(0,23),ylim=c(20,100),col=farben[i],lwd=2,xlab="",ylab="",axes=FALSE)
par(new=T)}

#Plots for every sensor
par(new=T)
for (i in 1:19)
{windows()
  data<-get(liste8[i])
  #main=paste0("Gemittelter Tagesverlauf (Sommer) ",substr(liste8[i],9,10)," ",Standort[i])
  main=paste0("Gemittelter Tagesverlauf (Herbst) ",substr(liste8[i],9,9)," ",Standort[i])
  par(mar=c(5, 5, 5, 5) + 0.1)
  plot(data$Zeit,data$PM5, xlim=c(0,23), ylim=c(1,max(data$PM10)),main=main, xlab="Uhrzeit",col="darkgreen",lwd=2,ylab="PM 10 und PM 2.5 µg/m³",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data$Zeit,data$PM10,axes=FALSE, xlim=c(0,23),ylim=c(1,max(data$PM10)),main=main, xlab="",col="darkorange",lwd=2,ylab="",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data$Zeit,data$Temp,axes=FALSE, xlim=c(0,23),ylim=c(15,max(data$Hum)),main=main, xlab="",col="red",lwd=2,ylab="",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data$Zeit,data$Hum,axes=FALSE, xlim=c(0,23),ylim=c(15,max(data$Hum)),main=main, xlab="",col="blue",lwd=2,ylab="",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  axis(4, ylim=c(0,max(data$Hum)),col= "black",lwd=1, cex.axis=1.5,cex.lab=1.5)
  mtext(4,text="Luftfeuchtigkeit % / Temperatur °C",line=3,cex.lab=1.5,lwd=3 ,cex=1.5)
  
  legend(x=10,y=3,legend=c("PM 10", "PM 2.5","Temperatur","Luftfeuchtigkeit"),col=c("darkgreen","darkorange","red","blue"))
  
  dev.copy(png,filename=paste0(main,".png"))
  dev.off ()
}

#############################################################################
#
#differences between days of the week
#
############################################################################
# corrected values 
#aggregate after execute script above
WT.h<-c()
for (i in 1:16)
{
  data<-get(l2[i])
  #group by Day of the week
  a1<-aggregate(data$PM10 ~ data$Wochentag, data, mean)
  a2<-aggregate(data$PM5 ~ data$Wochentag, data, mean)
  a3<-aggregate(data$Temp ~ data$Wochentag, data, mean)
  a4<-aggregate(data$Hum ~ data$Wochentag, data, mean)
  #sort
  result<-data.frame(a1[1],a1[2],a2[2],a3[2],a4[2])
  result2<-result[2,]
  result<-rbind(result2,result[6,],result[7,],result[5,],result[1,],result[3,],result[4,])
  names(result)<-c("Wochentag","PM10","PM5","Temp","Hum")
  result$Tag<-c(4,6,7,1,2,5,3)
  rownames(result)<-c(4,6,7,1,2,5,3)
  result <-result[order(result$Tag),]
  #schreiben
  assign(paste0("WT.",l2[i]),result)
  WT.h[i]<-paste0("WT.",l2[i])
}
WT.s<-c()
for (i in 1:16)
{
  data<-get(l1[i])
  a1<-aggregate(data$PM10 ~ data$Wochentag, data, mean)
  a2<-aggregate(data$PM5 ~ data$Wochentag, data, mean)
  a3<-aggregate(data$Temp ~ data$Wochentag, data, mean)
  a4<-aggregate(data$Hum ~ data$Wochentag, data, mean)
  result<-data.frame(a1[1],a1[2],a2[2],a3[2],a4[2])
  result2<-result[2,]
  result<-rbind(result2,result[6,],result[7,],result[5,],result[1,],result[3,],result[4,])
  names(result)<-c("Wochentag","PM10","PM5","Temp","Hum")
  result$Tag<-c(4,6,7,1,2,5,3)
  rownames(result)<-c(4,6,7,1,2,5,3)
  result <-result[order(result$Tag),]
  assign(paste0("WT.",l1[i]),result)
  WT.s[i]<-paste0("WT.",l1[i])
}


Standort<-c("Nord","Irsch","Mariahof","Feyen","Quint","Zewen","Gartenfeld","Olewig","Schneidershof","Kernscheid","Maximin","Ruwer","Pfalzel","Ehrang","Tarforst","Irminenfreihof")

#summer
s.Standort<-c("Nord","Irsch","Mariahof","Feyen","Quint","Süd","Zewen","Gartenfeld","Olewig","Schneidershof","Kernscheid","Maximin","Ruwer","Pfalzel","Aveler Tal","Ehrang","Tarforst","Irminenfreihof")
#fall
h.Standort<-c("Nord","Irsch","Mariahof","Feyen","Quint","Zewen","Gartenfeld","Olewig","Schneidershof","Kernscheid","Maximin","Ruwer","Pfalzel","Ehrang","Tarforst","Irminenfreihof","Campus 2")
woche<-c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")
par(new=T)
for (i in 1:19)
{windows()
  data2<-get(WT.h[i])
  data1<-get(WT.s[i])
  max<-max(c(data1$PM10,data2$PM10))
  main=paste0("Gemittelte Wochentage ",substr(l1[i],15,15)," ",Standort[i])
  par(mar=c(4, 5, 5, 5) + 0.1)
  plot(data1$Tag,data1$PM5, xlim=c(1,7), xaxt='n', ylim=c(min(data2$PM5),max),main=main, xlab="",col="darkgreen",lwd=4,ylab="PM 10 und PM 2.5 µg/m³",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data1$Tag,data1$PM10,axes=FALSE, xlim=c(1,7),ylim=c(min(data2$PM5),max),main=main, xlab="",col="green",lwd=4,ylab="",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data2$Tag,data2$PM5, axes=FALSE, xlim=c(1,7), ylim=c(min(data2$PM5),max),main=main, xlab="",col="darkorange",lwd=4,ylab="PM 10 und PM 2.5 µg/m³",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  plot(data2$Tag,data2$PM10,axes=FALSE, xlim=c(1,7),ylim=c(min(data2$PM5),max),main=main, xlab="",col="red",lwd=4,ylab="",cex.lab=1.5, cex.axis=1.5, cex.main=1.5,type="b" )
  par(new=T)
  axis(1, ylim=c(1,7),col= "black",lwd=1, cex.axis=1.5,cex.lab=1.5,labels =woche,at=c(1:7) )

  mtext(1,text="Wochentage",line=3,cex.lab=1.5,lwd=3 ,cex=1.5)
  #legend(x=1,y=5,legend=c("PM 10 Sommer", "PM 2.5 Sommer","PM 10 Herbst","PM 2.5 Herbst"),lwd=2,col=c("darkgreen","green","darkorange","red"))
  
  dev.copy(png,filename=paste0(main,".png"))
  dev.off ()
}
