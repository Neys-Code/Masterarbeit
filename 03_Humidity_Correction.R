########################################################
#
#Humidity correction
#
######################################################## 

#run script 2 first !!!!!!

liste3<-c()
# best parameter for every sensor and correction formula
# create variables
parameter.PM10<-c()
corr.PM10<-c()
parameter.PM25<-c()
corr.PM25<-c()

#iterate though every sensor getting the best set of parameters
# long runtime !
for (i in 1:5)
{ 
  b<-get(liste[i]) # read data
  b<-na.omit(b)
  PM=b$PM10
  PM2=b$PM5
  Hum=b$Hum
  h3<-minify(PM,Hum,200, meth="pearson",fun="haenel") #run minify function
  h4<-minify(PM2,Hum,200, meth="pearson",fun="haenel")
  parameter.PM10[i]<-h3[2] # write parameter
  corr.PM10[i]<-h3[1] # write corresponding R² for minimal correlation after correction with certain parameter
  parameter.PM25[i]<-h4[2]
  corr.PM25[i]<-h4[1]
} 
#print values
parameter.PM10
corr.PM10
parameter.PM25
corr.PM25

#saved parameters:
# summer period
# hänel parameters pearson r
parameter.PM10.haenel<-c( 0.16, 0.01, 0.13, 0.10, 0.08, 0.10, 0.05, 0.01, 0.10, 0.05, 0.08, 0.05, 0.01, 0.07, 0.11, 0.33 ,0.07, 0.04)
corr.PM10.haenel<-c(3.016909e-04, 5.464012e-03, 2.954449e-04, 5.016221e-03, 2.705385e-04 ,4.871921e-03, 6.477679e-03, 4.479281e-03, 6.758295e-04, 5.805504e-04 ,5.467699e-05, 3.738570e-04, 1.243516e-03 ,1.850964e-03,6.222919e-03, 3.852336e-04 ,6.399280e-04 ,0.00433258)
parameter.PM25.haenel<-c(0.16 ,0.02 ,0.15 ,0.07 ,0.15 ,0.11 ,0.05 ,0.08 ,0.01 ,0.11, 0.09, 0.01, 0.01, 0.06, 0.16 ,0.73 ,0.11 ,0.02)
corr.PM25.haenel<-c(0.0006027008, 0.0107193796 ,0.0074230416 ,0.0031312635, 0.0017701254 ,0.0026194102, 0.0060226544, 0.0056408189, 0.0064777976 ,0.0014677468, 0.0074307741 ,0.0032105885, 0.0016125007, 0.0023624328,0.0023525978 ,0.0003923794, 0.0041374884, 0.00487286)

# fall
#Hänel pearson 
parameter.PM10.haenel<-c(0.09, 0.03, 0.08, 0.05, 0.08, 0.08, 0.17, 0.08, 0.01, 0.01, 0.09, 0.01, 0.01, 0.37, 0.14, 0.13, 0.03)
corr.PM10.haenel<-c(0.011874971 ,0.001176366 ,0.012782197, 0.002973503 ,0.003198040, 0.001206695, 0.004409697, 0.006643064 ,0.000909106 ,0.032864726 ,0.005654078, 0.040340846 ,0.184692998, 0.001490821, 0.006113390, 0.005903745,0.005049788)
parameter.PM25.haenel<-c(0.11, 0.05, 0.12, 0.10, 0.14, 0.11, 0.28, 0.10, 0.07 ,0.02, 0.10 ,0.01, 0.01, 0.48, 0.16, 0.10 ,0.10)
corr.PM25.haenel<-c(0.010041799, 0.008299816 ,0.011599963, 0.001410846 ,0.005704815, 0.002258389 ,0.002207879 ,0.003622712, 0.001878455, 0.003513041,0.008423391, 0.024696667, 0.157034945, 0.003227737, 0.003986456 ,0.012807499,0.000841304)

#######################
#Plot growthfunction
########################

# growth factor for every Sensor
gf.S<-c()
gf.H<-c()
for (i in 1:19)
{ 
  c.haenel<-get(liste[i])
  c.soneja<-c.haenel
  #growth haenel functiom
  c.haenel$PM10<-gf.haenel(c.haenel$PM10,c.haenel$Hum,parameter.PM10.haenel[i])
  c.haenel$PM5<-gf.haenel(c.haenel$PM5,c.haenel$Hum,parameter.PM25.haenel[i])
  #growth factor soneja function
  #c.soneja$PM10<-gf.soneja(c.soneja$PM10,c.soneja$Hum,parameter.PM10.soneja[i])
  #c.soneja$PM5<-gf.soneja(c.soneja$PM5,c.soneja$Hum,parameter.PM25.soneja[i])
  #write data
  assign(paste0("gf.haenel",liste[i]),c.haenel)
  gf.H[i]<-paste0("gf.haenel",liste[i])
  #assign(paste0("gf.soneja",liste[i]),c.soneja)
  #gf.S[i]<-paste0("gf.soneja",liste[i])
} 

#set colors or plot
farben<-c("gold", "darkorange", "red","darkred","hotpink","plum3","darkmagenta","cornflowerblue","blue","darkblue","turquoise","turquoise4","green","darkgreen","tan2","navajowhite4","grey","black")

#Plotloop growthfunction PM10 Hänel
windows()
plot(`gf.haenelSensor1-7132425`$Hum,`gf.haenelSensor1-7132425`$PM10, type="n", col = "magenta", ylab="Wachstumsfaktor",cex.axis=1.5,cex.lab=1.5, xlab="Luftfeuchtigkeit %", lwd = 1, main="Wachstumsfunktion für PM 10 (Funktion nach Hänel)")
par(new=T)
for (i in 1:19)
{ Sensor<-get(gf.H[i])
  plot(Sensor$Hum,Sensor$PM10, xlab="",ylab="",type="p",col = farben[i], axes=FALSE)
  par(new=T)}

#Plotloop growthfunction PM 2,5 Hänel
windows()
plot(`gf.haenelSensor1-7132425`$Hum,`gf.haenelSensor1-7132425`$PM5, type="n", col = "magenta", ylab="Wachstumsfaktor",cex.axis=1.5,cex.lab=1.5, xlab="Luftfeuchtigkeit %", lwd = 1, main="Wachstumsfunktion für PM 2,5 (Funktion nach Hänel)")
par(new=T)
for (i in 1:19)
{ Sensor<-get(gf.H[i])
  plot(Sensor$Hum,Sensor$PM5, xlab="",ylab="",type="p",col = farben[i], axes=FALSE)
  par(new=T)}


############################################
#
#Correct and write Data 
#
###########################################
# corret PM10 and PM 25 with best parameters for correction function
liste4<-c()
for (i in 1:19)
{ 
  c.haenel<-get(liste[i])
  #c.soneja<-c.haenel
  #correct with haenel functiom
  c.haenel$PM10<-haenel(c.haenel$PM10,c.haenel$Hum,parameter.PM10.haenel[i])
  c.haenel$PM5<-haenel(c.haenel$PM5,c.haenel$Hum,parameter.PM25.haenel[i])
  #write data
  assign(paste0("Haenel",liste[i]),c.haenel)
  liste4[i]<-paste0("Haenel",liste[i])
 # write.csv(c.haenel,file=(paste0("E:/Dropbox/Dropbox/Masterarbeit","/Sommerhaenel",substr (f[i],1,10),".csv")))
  #write.csv(c.haenel,file=(paste0("E:/Dropbox/Dropbox/Masterarbeit","/Herbsthaenel",substr (f[i],1,10),".csv")))
  } 


########################
#Classification for corrected values
########################
########## hourly values

#switch between seasons
liste7<-c()
liste7h<-c()
for (i in 1:length(liste))
{ 
  s<-get(liste4[i])
  result<-Stundenwerte(s)
  assign(paste0("c.Herbst",liste[i]), result)
  #assign(paste0("c.Sommer",liste[i]), result)
  #liste7<-rbind(liste7,paste0("c.Sommer",liste[i]))
  liste7h<-rbind(liste7h,paste0("c.Herbst",liste[i]))
  #write.csv(result,file=paste0(substr(liste7[i],1,16),"Sommer.csv"))
  write.csv(result,file=paste0(substr(liste7h[i],1,16),"Herbst.csv"))
} 

# merge in one Dataset
dummy2<-data.frame(`c.HerbstSensor10-7131453`$Date,`c.HerbstSensor10-7131453`$PM10)#fall
#dummy2<-data.frame(`c.SommerSensor6-7134028`$Date,`c.SommerSensor6-7134028`$PM10)#sommer
names(dummy2)<-c("Date","dummy")
#merging  Data 
s<-dummy2
c.PM10<-c.PM5<-data.frame(s$Date,s$dummy)
names(c.PM10)<-c("Date","dummy")
names(c.PM5)<-c("Date","dummy")

liste7<-na.omit(liste7)
liste7h<-na.omit(liste7h)

for (i in 1:length(liste7h))
{ #s<-get(liste7[i])
  s<-get(liste7h[i])
  c.PM10<-merge(x=c.PM10 , y= s[1:2], by= 'Date', all.x= T)
  c.PM5<-merge(x=c.PM5 , y=s[,c(1,3)] , by= 'Date', all.x= T)
  names(c.PM10)<-c("Date","dummy",liste[1:i])
  names(c.PM5)<-c("Date","dummy",liste[1:i])
} 
c.PM10<-c.PM10[-c(2)]
c.PM5<-c.PM5[-c(2)]

#write.csv(c.PM10,file="PM10SommerR.csv")
#write.csv(c.PM5,file="PM25SommerR.csv")

write.csv(c.PM10,file="PM10HerbstR.csv")
write.csv(c.PM5,file="PM25HerbstR.csv")


# calculate row means as above
c.RMPM10<-rowMeans(c.PM10[-c(1)], na.rm=TRUE)
c.RMPM5<-rowMeans(c.PM5[-c(1)], na.rm=TRUE)
#calculate column means
c.CMPM10<-colMeans(c.PM10[-c(1)], na.rm=TRUE)
c.CMPM5<-colMeans(c.PM5[-c(1)], na.rm=TRUE)


### difference between corrected PM and uncorrected PM 
#datacloud for basestatistics

#summer
w.Hum <-c(Hum[,2],Hum[,3],Hum[,4],Hum[,5],Hum[,6],Hum[,7],Hum[,8],Hum[,9],Hum[,10],Hum[,11],Hum[,12],Hum[,13],Hum[,14],Hum[,15],Hum[,16],Hum[,17],Hum[,18])
w.PM10 <-c(PM10[,2],PM10[,3],PM10[,4],PM10[,5],PM10[,6],PM10[,7],PM10[,8],PM10[,9],PM10[,10],PM10[,11],PM10[,12],PM10[,13],PM10[,14],PM10[,15],PM10[,16],PM10[,17],PM10[,18])
w.PM5 <-c(PM5[,2],PM5[,3],PM5[,4],PM5[,5],PM5[,6],PM5[,7],PM5[,8],PM5[,9],PM5[,10],PM5[,11],PM5[,12],PM5[,13],PM5[,14],PM5[,15],PM5[,16],PM5[,17],PM5[,18])
w.c.PM10 <-c(c.PM10[,2],c.PM10[,3],c.PM10[,4],c.PM10[,5],c.PM10[,6],c.PM10[,7],c.PM10[,8],c.PM10[,9],c.PM10[,10],c.PM10[,11],c.PM10[,12],c.PM10[,13],c.PM10[,14],c.PM10[,15],c.PM10[,16],c.PM10[,17],c.PM10[,18])
w.c.PM5 <-c(c.PM5[,2],c.PM5[,3],c.PM5[,4],c.PM5[,5],c.PM5[,6],c.PM5[,7],c.PM5[,8],c.PM5[,9],c.PM5[,10],c.PM5[,11],c.PM5[,12],c.PM5[,13],c.PM5[,14],c.PM5[,15],c.PM5[,16],c.PM5[,17],c.PM5[,18])
w.Temp<-c(Temp[,2],Temp[,3],Temp[,4],Temp[,5],Temp[,6],Temp[,7],Temp[,8],Temp[,9],Temp[,10],Temp[,11],Temp[,12],Temp[,13],Temp[,14],Temp[,15],Temp[,16],Temp[,17],Temp[,18])

#fall
w.Hum <-c(Hum[,2],Hum[,3],Hum[,4],Hum[,5],Hum[,6],Hum[,7],Hum[,8],Hum[,9],Hum[,10],Hum[,11],Hum[,12],Hum[,13],Hum[,14],Hum[,15])
w.PM10 <-c(PM10[,2],PM10[,3],PM10[,4],PM10[,5],PM10[,6],PM10[,7],PM10[,8],PM10[,9],PM10[,10],PM10[,11],PM10[,12],PM10[,13],PM10[,14],PM10[,15])
w.PM5 <-c(PM5[,2],PM5[,3],PM5[,4],PM5[,5],PM5[,6],PM5[,7],PM5[,8],PM5[,9],PM5[,10],PM5[,11],PM5[,12],PM5[,13],PM5[,14],PM5[,15])
w.c.PM10 <-c(c.PM10[,2],c.PM10[,3],c.PM10[,4],c.PM10[,5],c.PM10[,6],c.PM10[,7],c.PM10[,8],c.PM10[,9],c.PM10[,10],c.PM10[,11],c.PM10[,12],c.PM10[,13],c.PM10[,14],c.PM10[,15])
w.c.PM5 <-c(c.PM5[,2],c.PM5[,3],c.PM5[,4],c.PM5[,5],c.PM5[,6],c.PM5[,7],c.PM5[,8],c.PM5[,9],c.PM5[,10],c.PM5[,11],c.PM5[,12],c.PM5[,13],c.PM5[,14],c.PM5[,15])
w.Temp<-c(Temp[,2],Temp[,3],Temp[,4],Temp[,5],Temp[,6],Temp[,7],Temp[,8],Temp[,9],Temp[,10],Temp[,11],Temp[,12],Temp[,13],Temp[,14],Temp[,15])


Statistik(w.PM10)
Statistik(w.PM5)
Statistik(w.c.PM10)
Statistik(w.c.PM5)

diff.PM10<-w.c.PM10-w.PM10
diff.PM5<-w.c.PM5-w.PM5

Statistik(diff.PM10)
Statistik(diff.PM5)

####################################
#significance humidity correction

PM=`Sensor16- 713582`$PM10
Hum=`Sensor16- 713582`$Hum

res<-list()
for (i in 1:17)
{ 
  b<-get(liste[i]) # read data
  b<-na.omit(b)
  PM=b$PM10
  PM2=b$PM5
  Hum=b$Hum
  iter=50

  corr<-c()
  pval<-c()
  a=0.01
  
  for ( j in 1: iter)
  { NPM<-mapply(haenel,PM,Hum,a)
    corr[j]<-cor.test(NPM,Hum,method="pearson")[["estimate"]]#korrelation berechnen # korrelationskoeffizient
    param[j]<-cor.test(NPM,Hum,method="pearson")[["p.value"]]#p value
    a=a+0.01
    NPM<-c()}
  g<-data.frame(abs(corr),param)
  mini<-min(g$abs.corr., na.rm = TRUE)
  par<-g$param[g$abs.corr.==mini]
  res[i]<-c(mini,par)
} 


# Quality of implementation of the humidity correction
######################################
#Compare to Data provided by Streibl
######################################

#set path
pfad<-("E:/Dropbox/Dropbox/Masterarbeit/Silberberg")#desktop1
setwd(pfad)# set woring directory
f<-list.files(path = pfad, pattern = NULL, all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# create all matrices to be filled within loop 
n=c()
i=1
liste=c()
#for loop to read all data and perform several stats 
for (i in 1:2 )
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
  
  #correct different variable names
  if (is.na(x$BMP_temperature[1])==FALSE)
  {x$Temp<-x$BMP_temperature
  x$Humidity<-x$BMP_pressure}
  x<-Runden(x)
  Temp2<-c(Temp2,x$Temp)
  PM102=c(PM102,x$SDS_P1)
  PM52=c(PM52,x$SDS_P2)
  Hum2=c(Hum2,x$Humidity)
  }
  Temp<-c(Temp,Temp2)
  PM10=c(PM10,PM102)
  PM5=c(PM5,PM52)
  Hum=c(Hum,Hum2)
  }
  PM10<-round(PM10,digits=2)
  PM5<-round(PM5,digits=2)
  Temp<-round(Temp,digits=2)
  Hum<-round(Hum,digits=2)
  
  Sensor<-data.frame(PM10,PM5,Temp,Hum)
  #write values to dataframe
  assign(paste0("Sensor",substr (f[i],1,10)),Sensor)
  liste<-c(liste,(paste0("Sensor",substr (f[i],1,10))))
}
PM<-as.numeric(Sensor746774$PM10)
PM2<-as.numeric(Sensor746774$PM5)
Hum<-as.numeric(Sensor746774$Hum)

#Hänel
#log-transformed
h8<-minify(PM2,Hum,2000, meth="log",fun="haenel")
#spearman
h10<-minify(PM2,Hum,2000, meth="spearman",fun="haenel")
#pearson
h12<-minify(PM2,Hum,2000, meth="pearson",fun="haenel")

#Soneja
#log-transformed
h2<-minify(PM2,Hum,2000, meth="log",fun="soneja")
#spearman
h4<-minify(PM2,Hum,2000, meth="spearman",fun="soneja")
#pearson
h6<-minify(PM2,Hum,2000, meth="pearson",fun="soneja")

#Köhler
#log-transformed
h14<-minify(PM2,Hum,2000, meth="log",fun="koehler")
#spearman
h16<-minify(PM2,Hum,2000, meth="spearman",fun="koehler")
#pearson
h18<-minify(PM2,Hum,2000, meth="pearson",fun="koehler")


#############################################################

# find best fitting method with Sensor 16 
PM<-`Sensor16- 713582`$PM10
PM2<-`Sensor16- 713582`$PM5
Hum<-`Sensor16- 713582`$Hum
#Soneja
#log-transformed
h1<-minify(PM,Hum,2000, meth="log",fun="soneja")
h2<-minify(PM2,Hum,2000, meth="log",fun="soneja")
#spearman
h3<-minify(PM,Hum,2000, meth="spearman",fun="soneja")
h4<-minify(PM2,Hum,2000, meth="spearman",fun="soneja")
#pearson
h5<-minify(PM,Hum,2000, meth="pearson",fun="soneja")
h6<-minify(PM2,Hum,2000, meth="pearson",fun="soneja")

#Hänel
#log-transformed
h7<-minify(PM,Hum,2000, meth="log",fun="haenel")
h8<-minify(PM2,Hum,2000, meth="log",fun="haenel")
#spearman
h9<-minify(PM,Hum,2000, meth="spearman",fun="haenel")
h10<-minify(PM2,Hum,2000, meth="spearman",fun="haenel")
#pearson
h11<-minify(PM,Hum,2000, meth="pearson",fun="haenel")
h12<-minify(PM2,Hum,2000, meth="pearson",fun="haenel")

#Köhler
#log-transformed
h13<-minify(PM,Hum,2000, meth="log",fun="koehler") 
h14<-minify(PM2,Hum,2000, meth="log",fun="koehler")
#spearman
h15<-minify(PM,Hum,2000, meth="spearman",fun="koehler") 
h16<-minify(PM2,Hum,2000, meth="spearman",fun="koehler")
#pearson
h17<-minify(PM,Hum,2000, meth="pearson",fun="koehler") 
h18<-minify(PM2,Hum,2000, meth="pearson",fun="koehler")



#######################################
#compare Sensor with oficcial data 
#######################################
#set path
#ZIMEN Data
PM_Sommer <- read.csv("E:/Dropbox/Dropbox/Masterarbeit/PM_Herbst.csv", header=TRUE,stringsAsFactors=FALSE)
View(PM_Sommer)
PM_Sommer$Date<-paste0(substr(PM_Sommer$Zeitpunkt,7,10),"/",substr(PM_Sommer$Zeitpunkt ,4,5),"/",substr(PM_Sommer$Zeitpunkt,1,2)," ",substr(PM_Sommer$Zeitpunkt,12,17),":00")
PM_Sommer$Trier.Ostallee.PM10<-as.numeric(PM_Sommer$Trier.Ostallee.PM10)
PM_Sommer$Trier.Pfalzel.PM2.5<-as.numeric(PM_Sommer$Trier.Pfalzel.PM2.5)

# hourly values
a<-`Sensor16- 713582`
a$Date2<-paste0(substr(a$Date,1,14),"00:00")
PM10 <- aggregate(a$PM10 ~ Date2, a, mean) 
PM5 <- aggregate(a$PM5 ~ Date2, a, mean) 
Temp <- aggregate(a$Temp ~ Date2, a, mean) 
Hum <- aggregate(a$Hum ~ Date2, a, mean) 
tag<- substr(PM10[[1]],1,10)
stunde<-substr(PM10[[1]],12,22)
b<- data.frame(c(PM10[1]),c(tag),c(stunde),c(PM10[2]),c(PM5[2]),rbind(Temp[2]),rbind(Hum[2]))
names(b)<-c("Date","tag","Stunde","PM10","PM5","Temp","Hum")

#compare data 
#01.09.2019-24.11.2019
ds1<-data.frame(b$Date,b$Hum)
names(ds1)<-c("Date","Hum_SDS")
ds2<-data.frame(dwd$time,dwd$AVG_RH350)
names(ds2)<-c("Date","Hum_DWD")
ds3<-merge(x= ds1, y= ds2, by= 'Date', all.x= T)
ds3$pm10<-b$PM10
ds3$pm5<-b$PM5
ds3=na.omit(ds3)

ds4<-data.frame(PM_Sommer$Date,PM_Sommer$Trier.Ostallee.PM10,PM_Sommer$Trier.Pfalzel.PM2.5)
names(ds4)<-c("Date","Ostalee","Pfalzel")
ds5<-merge(x= ds3, y= ds4, by= 'Date', all.x= T)
plot(ds5$pm10~ds5$Ostalee)
diff<-ds5$pm10-ds5$Ostalee
diff=na.omit(diff)
Statistik(diff)

#correct with haenel and Soneja and Köhler
s16HP<-mapply(haenel,ds3$pm10,ds3$Hum_SDS,0.46)
s16SP<-mapply(soneja,ds3$pm10,ds3$Hum_SDS,0.17)
s16KP<-mapply(koehler,ds3$pm10,ds3$Hum_SDS,0.17)

#plot Feuchtigkeit über die zeit 
windows()
plot(as.numeric(ds5$Date),ds5$pm10,col="red",type="l",ylim =c(0,50),xlab="Zeit in Stunden",cex.lab=1.5,cex.axis=1.5, ylab="PM 10 µg/m³", main= "Vergleich Sensor 16 und Messstation Ostallee") 
par(new=T)
plot(as.numeric(ds5$Date),ds5$Ostalee,ylim =c(0,50),type="l", axes=FALSE, col="grey", xlab="",ylab="")
par(new=T)
plot(as.numeric(ds3$Date),s16HP, type="l",ylim=c(0,50),col="green", xlab="",ylab="", axes=FALSE)
par(new=T)
plot(as.numeric(ds3$Date),s16SP, type="l",ylim=c(0,50),col="darkgreen", xlab="",ylab="", axes=FALSE)
par(new=T)
plot(as.numeric(ds3$Date),s16KP, type="l",ylim=c(0,50),col="blue", xlab="",ylab="", axes=FALSE)
legend(17,50,legend=c("Messstation Ostallee","Sensor 16 unkorrigiert", "Sensor 16 Korrektur nach Hänel", "Sensor 16 Korrektur nach Soneja", "Sensor 16 Korrektur nach Köhler"),col=c("grey","red","green","darkgreen","blue"), bg="white", lty=1:1,lwd=3, cex=0.8,text.font=4)




