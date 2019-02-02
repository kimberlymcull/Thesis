####Read in DYNAMIC Data & Reformat####
cullout <- read.csv("~/Documents/OPERATIONS_RESEARCH/Thesis/ThesisData/cullout2.csv")
cullout$Time=as.POSIXct(as.numeric(cullout$Time),origin="1970-01-01",tz="GMT")

####Code for creating country id column####
cullout$Code=as.integer(substr(cullout$MMSI,0,3))
CountryCodes<-read.csv("~/Documents/OPERATIONS_RESEARCH/Thesis/ThesisData/MaritimeIdentificationDigits.csv",header=T)
CountryCodes$Allocated.to=as.character(CountryCodes$Allocated.to)
cullout$Country<-CountryCodes$Allocated.to[match(cullout$Code,CountryCodes$Digit)]
cullout=cullout[,-8]

####Reformat Static data files####
MMSIs=unique(cullout$MMSI)
setwd("/Users/kimberlydoughty/Documents/OPERATIONS_RESEARCH/Thesis/ThesisData/StaticData")
list.files()
names.vec=list.files()

#Static Data
library(stringr)
for (i in 1:length(names.vec)){
  static.data= read.csv(names.vec[i],header=FALSE)
  colnames(static.data)=c("MMSI","Destination","dim_b","dim_c","id_static","Callsign","ShipType","Time","dim_a","dim_d","Name")
  static.data$MMSI=as.integer(substr(static.data$MMSI,8,16))
  static.data=static.data[static.data$MMSI %in% MMSIs,]
  static.data$Time=substr(static.data$Time,8,17)
  static.data$Time=as.POSIXct(as.numeric(static.data$Time),origin="1970-01-01",tz="GMT")
  static.data$Destination=substr(static.data$Destination,14,45)
  static.data$dim_b=substr(static.data$dim_b,8,13)
  static.data$dim_c=substr(static.data$dim_c,8,13)
  static.data$dim_a=substr(static.data$dim_a,8,13)
  static.data$dim_d=substr(static.data$dim_d,8,13)
  static.data$id_static=substr(static.data$id_static,5,6)
  static.data$Callsign=substr(static.data$Callsign,11,20)
  static.data$ShipType=substr(static.data$ShipType,11,13)
  static.data$Name=substr(static.data$Name,7,40)
  static.data$Name=str_sub(static.data$Name,-50,-2)
  static.data=static.data[which(duplicated(static.data[,-c(8,12,13)])=="FALSE"),]
  Out.file=paste("static.data",i,sep=".")
  print((Out.file))
  assign(Out.file,static.data)
}
StaticData=rbind(static.data.1,static.data.2,static.data.3,static.data.4,static.data.5,static.data.6,
                 static.data.7,static.data.8,static.data.9, static.data.10,static.data.11,static.data.12,
                 static.data.13,static.data.14,static.data.15,static.data.16,static.data.17,static.data.18,
                 static.data.19,static.data.20,static.data.21,static.data.22,static.data.23,static.data.24,
                 static.data.25,static.data.26,static.data.27,static.data.28,static.data.29,static.data.30,
                 static.data.31)
StaticData=StaticData[,c(1,2,6,11,8,7,9,3,4,10)]
FebStat <- read.csv("~/Documents/OPERATIONS_RESEARCH/Thesis/ThesisData/Cull.5.csv")
colnames(FebStat)=c("MMSI","Destination","Callsign","Name","Time","ShipType","dim_a","dim_b","dim_c","dim_d")
FebStat$Time=as.POSIXct(as.numeric(FebStat$Time),origin="1970-01-01",tz="GMT")
FebStat=FebStat[which(duplicated(FebStat[,-c(5)])=="FALSE"),]
StaticData=rbind(StaticData,FebStat)

#Country codes to full static
StaticData$Code=as.integer(substr(StaticData$MMSI,0,3))
StaticData$Country<-CountryCodes$Allocated.to[match(StaticData$Code,CountryCodes$Digit)]
StaticData=StaticData[,-11]

#Keep only legitimate MMSIs
#remove NAs from country 
cullout= cullout[-(which(cullout$Country %in% NA)),]
#remove MMSIs that are not exactly 9 digits
cullout=cullout[nchar(cullout$MMSI)==9,]
#remove NAs from country 
StaticData= StaticData[-(which(StaticData$Country %in% NA)),]
StaticData= StaticData[,-11]
#remove MMSIs that are not exactly 9 digits
StaticData=StaticData[nchar(StaticData$MMSI)==9,]


#Write to CSV
write.csv(StaticData, file = "FullGoodStatic.csv")
write.csv(cullout, file = "FullGoodDynamic.csv")







