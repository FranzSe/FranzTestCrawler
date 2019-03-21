pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("stringr")
pkgTest("dplyr")
pkgTest("rvest")
pkgTest("tidyverse")
pkgTest("ggplot2")
pkgTest("lubridate")



#import data
rawdata <- read.csv("~/ForecastPH/Data/all_data_20181115.csv")
infoParkhaus <- read.csv("~/ForecastPH/Data/infoParkhaus.csv", 
                         row.names=NULL, sep=";")
colnames(infoParkhaus)[1] <- "ID"

#delete columns
rawdata$X1 <- NULL
rawdata$isotime <- NULL
#check if there are NAs
colSums(is.na.data.frame(rawdata))
#change dateformat and round to the next 15 minutes 
rawdata["datetime"] <-floor_date(as.POSIXct(rawdata$time,format="%d.%m.%Y%H:%M:%OS", 
                                            tz=toString(Sys.timezone(location = TRUE))),"15 minutes")

ggplot(rawdata,aes(as.factor(rawdata$ID)))+
  geom_histogram(stat = "count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Parkhaus ID")
  #hier ist erkenntlich, dass für Parkhaus 6813 & 6835 noch nich über den ganzen Zeitraum Daten vorliegen.

#get max and min date
minvalue<-aggregate.data.frame(list(Min=rawdata$datetime), by=list(ID=rawdata$ID),FUN =min)
maxvalue<-aggregate.data.frame(list(Max=rawdata$datetime), by=list(ID=rawdata$ID),FUN =max)
countvalue<-aggregate.data.frame(list(Count=rawdata$datetime), by=list(ID=rawdata$ID),FUN =length)
mergesDF<-merge.data.frame(minvalue,maxvalue,by = "ID")
mergesDF<- merge.data.frame(mergesDF,countvalue, by="ID")

  #es gibt kaum fehledne Werte. Maximal 6 Aufzeichnungen je Parkhause fehlen. Bei über 50.000 Aufzeichnungen
  # je Parkhaus ist dies zu vernachlässigen




#find missing datapoints
#creatate a timesequence with all possible timevalues between first and last rwo
timesequence<- seq(min(rawdata$datetime),max(rawdata$datetime), "15 min")
max(rawdata$datetime)

#create a dataframe with all time and ph-Id combination
allPossObserva <- expand.grid(datetime=as.POSIXct(timesequence),ID=infoParkhaus$ID)
#calculate number of missing observations
nrow(allPossObserva)- nrow(rawdata)
#merge DFs
allValues<- merge(rawdata,allPossObserva, by.x = c("datetime","ID"), by.y =c("datetime","ID"),all.y = T)

#plot NA values
test <- allValues[is.na(allValues$`current capacity`), ]

ggplot(test, aes(x=as.factor(ID)))+
  geom_histogram(stat = "count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#show NAs per PH
aggregate.data.frame(test$ID, by = list(test$ID),FUN = length)
#show NAs per Date
naDates <- aggregate.data.frame(test$ID, by = list(as.Date(test$datetime)),FUN = length)
naDates <- naDates[order(naDates$x,decreasing = T),]
head(naDates,50)
  #an zwei Tagen (04.-05.11.2017) fiel der Crawler komplett aus daher 2304 NAs (24PH*24H*4=2304)
  #ca. halbtägige Aussetzer gab es 06.11.2017 und am 03.03.2018
  #Fehlen die Daten für nur ein Parkhaus, so sind 96 NAs festzustellen (24h*4=96)


#save DF
write.csv(allValues,"~/ForecastPH/Data/allValues.csv")
saveRDS(allValues,"~/ForecastPH/Data/allValues.RDS")

