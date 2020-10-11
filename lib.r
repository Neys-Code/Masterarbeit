###############################################
#
#Libary
#
###############################################
# all function names are in german to provide conflicts 

#base fuctions
#relative standart error
rse<-function(x)
  {round((sd(x,na.rm = TRUE)/sqrt(length(c(sd(x,na.rm=TRUE)))))/mean(x,na.rm = TRUE),digits = 2)}

Statistik<- function(a)
  {v<-c(min(a,na.rm=TRUE),max(a,na.rm=TRUE),mean(a,na.rm=TRUE),median(a,na.rm=TRUE),var(a,na.rm=TRUE),sd(a,na.rm=TRUE))
  names(v)<-c("min","max","mean","median","variance","standartabweichung")
  v<-round(v,digits=2)
  return(v)}


# create hourly mean values
Stundenwerte<-function(data){
  data$Date2<-paste0(substr(data$Date,1,14),"00:00")
  pm<-data.frame(c(aggregate(data$PM10 ~ Date2, data, mean)[1]),c(aggregate(data$PM10 ~ Date2, data, mean)[2]),c(aggregate(data$PM5 ~ Date2, data, mean)[2]))
  names(pm)<-c("Date","PM10","PM5")
  dht<-data.frame(c(aggregate(data$Temp ~ Date2, data, mean)[1]),c(aggregate(data$Temp ~ Date2, data, mean)[2]),c(aggregate(data$Hum ~ Date2, data, mean)[2]))
  names(dht)<-c("Date","Temp","Hum")
  b<-merge(x= pm, y= dht, by= 'Date', all.x= T)
  b$Zeit<-paste0(substr(b$Date,12,14),"00:00")
  b$Tag<-paste0(substr(b$Date,1,10))
  b$Wochentag<-weekdays(as.Date(b$Tag,'%Y/%m/%d'))
  names(b)<-c("Date","PM10","PM5","Temp","Hum","Zeit","Tag","Wochentag")
  b<-na.omit(b)
  return(b)
}

# create hourly mean values
Tageswerte<-function(data){
  data$Date2<-paste0(substr(data$Date,1,11),"00:00:00")
  pm<-data.frame(c(aggregate(data$PM10 ~ Date2, data, mean)[1]),c(aggregate(data$PM10 ~ Date2, data, mean)[2]),c(aggregate(data$PM5 ~ Date2, data, mean)[2]))
  names(pm)<-c("Date","PM10","PM5")
  dht<-data.frame(c(aggregate(data$Temp ~ Date2, data, mean)[1]),c(aggregate(data$Temp ~ Date2, data, mean)[2]),c(aggregate(data$Hum ~ Date2, data, mean)[2]))
  names(dht)<-c("Date","Temp","Hum")
  b<-merge(x= pm, y= dht, by= 'Date', all.x= T)
  b$Zeit<-paste0(substr(b$Date,12,14),"00:00")
  b$Tag<-paste0(substr(b$Date,1,10))
  b$Wochentag<-weekdays(as.Date(b$Tag,'%Y/%m/%d'))
  names(b)<-c("Date","PM10","PM5","Temp","Hum","Zeit","Tag","Wochentag")
  b<-na.omit(b)
  return(b)
}

#round variables
Runden<-function(b){
  b$PM10<-round(b$SDS_P1, digits = 2)
  b$PM5<-round(b$SDS_P2, digits = 2)
  b$Hum<-round(b$Humidity, digits = 2)
  b$Temp<-round(b$Temp, digits = 2)
  return(b)
} 


#minuites as decimal
motj<-function(x){
  n<-substr(x,12,20)
  doy<-as.numeric(strftime(x, format = "%j"))*24*60
  t<-(as.numeric(substr(n,1,2))*60)+as.numeric(substr(n,4,5))+(as.numeric(substr(n,7,8))/60)+doy
  return(t)
}
# Humidity correction

# Implementing Soneja et al.
soneja<-function(PM,Hum,a)
{rh=Hum/100
GF=1 + a*rh^2/(1-rh)
result<-(PM/GF)
result<-round(result, digits=2)
return(result)}

# Hänel
haenel<-function(PM,Hum,a)
{rh<-Hum/100
gf<-1/((1-rh)^a)
result<-(PM/gf)
result<-round(result, digits=2)
return(result)}

# Koehler
koehler<-function(PM,Hum,a)
{rh=Hum/100
GF=1+a/(1/rh-1)
result<-(PM/GF)
result<-round(result, digits=2)
return(result)}

#####growthfactor
# Implementing Soneja et al.
gf.soneja<-function(PM,Hum,a)
{rh=Hum/100
GF=1 + a*rh^2/(1-rh)
return(GF)}

# Hänel
gf.haenel<-function(PM,Hum,a)
{rh<-Hum/100
gf<-1/((1-rh)^a)
return(gf)}

# Koehler
gf.koehler<-function(PM,Hum,a)
{rh=Hum/100
GF=1+a/(1/rh-1)
return(GF)}

#Iterate correction formula to find mimimum correlation
minify<-function(PM,Hum,iter,meth="",fun="") 
{ res<-c()
  corr<-c()
  param<-c()
  a=0.01
  for ( i in 1: iter)
  { if (fun=="haenel"){NPM<-mapply(haenel,PM,Hum,a)}#corrected pm-values
    if (fun=="soneja"){NPM<-mapply(soneja,PM,Hum,a)}
    if (fun=="koehler"){NPM<-mapply(koehler,PM,Hum,a)}

    if (meth=="log"){
      cl<-data.frame(NPM,Hum)
      cl$NPM[NPM==0]<-NA
      cl$Hum[NPM==0]<-NA
      cl<-na.omit(cl)
      corr[i]<-summary(lm(cl$Hum ~ log(cl$NPM)))[["adj.r.squared"]]}
    if (meth=="cubic"){corr[i]<-summary(lm(Hum ~ poly(NPM, 3, raw=TRUE)))[["adj.r.squared"]]}
    if (meth=="pearson"){corr[i]<-cor.test(NPM,Hum,method="pearson")[["estimate"]] }
    if (meth=="spearman"){corr[i]<-cor.test(NPM,Hum,method="spearman")[["estimate"]] }#korrelation berechnen # korrelationskoeffizient
    
    param[i]<-a# parameter der funktion
    a=a+0.01
    NPM<-c()}
  g<-data.frame(abs(corr),param)
  mini<-min(g$abs.corr., na.rm = TRUE)
  par<-g$param[g$abs.corr.==mini]
  res<-c(mini,par)
  return(res)}

