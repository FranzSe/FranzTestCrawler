##Adding Features for forecasting


pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("dplyr")
pkgTest("rvest")
pkgTest("tidyverse")
pkgTest("rdwd")
pkgTest("tidyquant")
pkgTest("lubridate")



#load Data
allValues <- readRDS("~/ForecastPH/Data/allValues.RDS")
#drop time column
allValues$time<-NULL

#add calender week (KW)
allValues["KW"]<- isoweek(allValues$datetime)
allValues["Month"]<- month(allValues$datetime)
allValues["DayOfWeek"]<- wday(allValues$datetime)
allValues["Hour"] <-hour(allValues$datetime)


#add IsSchoolHolidays
#1= is holiday
#load data

ferien <- read.csv("~/ForecastPH/Data/Ferien.csv", sep=";", stringsAsFactors=FALSE)
ferien[,"DTSTART.DATE"]<- as.Date(ferien$DTSTART.DATE,format ="%d.%m.%Y")
ferien[, "DTEND.DATE"]<- as.Date(ferien$DTEND.DATE,format ="%d.%m.%Y")

allValues["IsSchoolHolidays"]<-0

for (i in 1:nrow(ferien)) {
  allValues[between(as.Date(allValues$datetime),ferien[i,2],ferien[i,3]),"IsSchoolHolidays"]<-1
}


#add public holidays
#load data
feiertage <- read.csv("~/ForecastPH/Data/feiertage.csv", 
                      sep=";", stringsAsFactors=FALSE)
colnames(feiertage)<-c("Datum","Name")
feiertage["Datum"]<-as.Date(feiertage$Datum,format ="%d.%m.%Y")
feiertage <-feiertage[order(feiertage$Datum),]
singeleDates<-data.frame(list(Datum=unique(as.Date(allValues$datetime)),daysTillHolyday=NA))
numOfDays <-nrow(singeleDates)

for(n in 1:numOfDays){
  
  x<-1
  daystillholyday<- -1
  
  while (daystillholyday<0){
    daystillholyday<-as.numeric(difftime(feiertage[x,1],singeleDates[n,1], units = "days"))   #calculate how many days till the next hollyday
    x<-x+1
    
  }
  singeleDates[n,"daysTillHolyday"] <- daystillholyday    #safe results in dataframe
  print(paste("Day",n,"of",numOfDays))
}
#conert Postct to date to have a key
allValues["Datum"]<- as.Date(allValues$datetime)
allValues <- merge(allValues, singeleDates, by= "Datum", all.x = T)


#add weather
#download weather data from DWD with packages rdwd
link <- selectDWD(id = 5705, res="daily", var="kl", per="rh", outvec=T)
file <- dataDWD(link, read=T)
weatherData<-bind_rows(file, .id = "column_label")
#remove all not necessary data
weatherData <- filter(weatherData,weatherData$MESS_DATUM>=min(as.Date(allValues$datetime)),weatherData$MESS_DATUM<=max(as.Date(allValues$datetime)))
weatherData <- weatherData %>% distinct(MESS_DATUM, .keep_all = TRUE)

#variable selection:
#SDK = daily sunshine duration
#RSK = daily precipitation height
#TMK = daily mean of temperature
weatherData <- select(weatherData,MESS_DATUM,SDK,RSK,TMK)
weatherData$MESS_DATUM <- as.Date(weatherData$MESS_DATUM)
allValues <- merge.data.frame(allValues, weatherData, by.x = "Datum", by.y = "MESS_DATUM", all.x = T)
allValues <- allValues[order(allValues$ID, allValues$datetime),]

#impute missing values
colSums(is.na.data.frame(allValues))
allValues<-na.locf(allValues)
colSums(is.na.data.frame(allValues))

#save file
#write.csv(allValues,"~/ForecastPH/Data/finalData.csv")
saveRDS(allValues,"~/ForecastPH/Data/finalData.RDS")
