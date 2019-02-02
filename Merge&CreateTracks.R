####Import full data sets####
dynamic <- read.csv("~/Documents/OPERATIONS_RESEARCH/Thesis/ThesisData/FullGoodDynamic.csv")
dynamic=dynamic[,-1]
static <- read.csv("~/Documents/OPERATIONS_RESEARCH/Thesis/ThesisData/FullGoodStatic.csv")
static=static[,-1]

####Merge####
library(data.table)
dynamic=as.data.table(dynamic)
static=as.data.table(static)
dynamic=dynamic[order(MMSI,Time)]
static=static[order(MMSI,Time)]


#Combine data
data=merge(dynamic,static,by.x=c("MMSI"),by.y=c("MMSI"),allow.cartesian=TRUE) #Inner Join
data$dif=abs(difftime(data$Time.x, data$Time.y, units = "mins"))
data=data[order(MMSI,Time.x,dif)]
data=data[!duplicated(data[,c('MMSI','Time.x')]),]


#remove unwanted column
data=data[,-c(12,18)]
#rename columns
colnames(data)[4]="Time"
#write out to csv
write.csv(data, file = "CompleteData_BySignal.csv")


####Condense to Tracks####
#prep data
library(data.table)
CompleteData_BySignal <- read.csv("~/Documents/OPERATIONS_RESEARCH/R/CompleteData_BySignal.csv")
CompleteData_BySignal=as.data.table(CompleteData_BySignal)
CompleteData_BySignal=CompleteData_BySignal[,-1]
library(chron)
CompleteData_BySignal$Time=as.POSIXct(CompleteData_BySignal$Time,origin= "2013-12-30" ,tz="GMT")
#Break set by day
CompleteData_BySignal$increments=cut.POSIXt(CompleteData_BySignal$Time, breaks="day")
#create birds eye and total distance
CompleteData_BySignal$BirdsEyeDist=as.numeric(0)
CompleteData_BySignal$TotalDist=as.numeric(0)

#Calculate birdseye distances and total distances for every mmsi and day group
library(geosphere)
for (mmsi in unique(CompleteData_BySignal[1031780:1226479,]$MMSI)){
  ship=subset(CompleteData_BySignal[1031780:1226479,],MMSI==mmsi)
  for (day in unique(ship$increments)){
      sigByday=subset(ship,increments==day)
      dist=list()
      for (row in 1:nrow(sigByday)){
        if(row!=nrow(sigByday)){
            lat1=sigByday[row]$Lat
            long1=sigByday[row]$Long
            lat2=sigByday[row+1]$Lat
            long2= sigByday[row+1]$Long
            dist[[row]]= distCosine(c(long1,lat1),c(long2,lat2))
        }
        else{
            lat1=sigByday[row]$Lat
            long1=sigByday[row]$Long
            lat2=sigByday[1]$Lat
            long2=sigByday[1]$Long
            birds=distCosine(c(long1,lat1),c(long2,lat2))
          }
      }
      if (is.null(Reduce('+',dist))==TRUE){
        total=0
      }
      else{
        total=Reduce('+',dist)
      }
      CompleteData_BySignal[CompleteData_BySignal$MMSI==mmsi & CompleteData_BySignal$increments==day,]$BirdsEyeDist=birds
      CompleteData_BySignal[CompleteData_BySignal$MMSI==mmsi & CompleteData_BySignal$increments==day,]$TotalDist=total
      }
  }


#create new data set for tracks, summary data: averages, minimums, maximums, unique character fields, Num signals rolled into day
TrackData=CompleteData_BySignal[,list(Long_Avg = mean(Long), Long_max=max(Long), Long_min=min(Long), 
                                      Lat_mean=mean(Lat), Lat_max=max(Lat), Lat_min=min(Lat),
                                      Time_min=min(Time), Time_max=max(Time), Avg_COG=mean(cog), Avg_SOG=mean(sog),
                                      id=unique(id), Country=unique(Country), Destination=unique(Destination),
                                      Callsign=unique(Callsign), Name=unique(Name), ShipType=unique(ShipType),
                                      dim_a=unique(dim_a), dim_b=unique(dim_b), dim_c=unique(dim_c), 
                                      dim_d=unique(dim_d),Num_Signal=.N, BirdsEyeDist=unique(BirdsEyeDist),
                                      TotalDist=unique(TotalDist)), by = c("MMSI","increments")]
#add column for area calculation of operating box
TrackData$Area_m=as.numeric(0)
for (row in 1:nrow(TrackData)){
    TrackData[row,"Area_m"]=areaPolygon(rbind(c(TrackData[row,"Long_min"],TrackData[row,"Lat_min"]),
                                               c(TrackData[row,"Long_max"],TrackData[row,"Lat_min"]),
                                               c(TrackData[row,"Long_max"],TrackData[row,"Lat_max"]),
                                               c(TrackData[row,"Long_min"],TrackData[row,"Lat_max"])))}

write.csv(TrackData, file = "TrackData.csv")
write.csv(CompleteData_BySignal,file="By_SignalwDistCalcs")




