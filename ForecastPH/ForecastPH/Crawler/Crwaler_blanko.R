#Function to load necessary packages only if needed
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
pkgTest("tidyverse")
pkgTest("rvest")
pkgTest("tidyquant")
pkgTest("lubridate")
pkgTest("R.utils")
pkgTest("DataCombine")
pkgTest("keras")



#initialize dataframe

logfile<-setNames(data.frame(matrix(ncol = 4, nrow = 0)), 
                  c("ID","current.capacity","time","total.capacity"))

#load ph info file
infoParkhaus <- read.csv("~/ForecastPH/Data/infoParkhaus.csv", 
                         row.names=NULL, sep=";")
colnames(infoParkhaus)[1] <- "ID"

infoParkhaus["ID"]

#Specifying the url for desired website to be scraped
basURL <-'https://svg.parkinghq.com/parking_units/'
ids<-unlist(infoParkhaus["ID"], use.names=FALSE)

for (t in ids) {
  url<-paste0(basURL,t)
  #Reading the HTML code from the website
  webpage <- read_html(url)
  #Using CSS selectors to scrap the rankings section
  capacity_html <- html_nodes(webpage,'p')
  
  #Converting the ranking data to text
  caoacity <- html_text(capacity_html)
  
  # prepare regular expression
  regexp <- "[[:digit:]]+"
  
  # process string
  currentData <-as.integer(str_extract(caoacity, regexp))
  currentRow <- nrow(logfile)+1
  logfile[currentRow,1]<-t    
  logfile[currentRow,2]<- currentData[2]
  logfile[currentRow,3]<- as.character(floor_date((Sys.time()+hours(1)),"15 minutes"))
  logfile[currentRow,4]<- currentData[1]
}
logfile$datetime<-as.POSIXct(logfile$datetime)

#save logfile
path <- paste0("~/ForecastPH/Crawler/logfiles/log_", toString(Sys.time()),".csv")
path <- str_replace_all(path,c(" "="_",":"="_"))
write_csv(logfile,path)

#load data
if(!exists("rawdata")){
  rawdata <-readRDS("~/ForecastPH/Data/rawdata_new.RDS")
}

infoParkhaus <- read_delim("ForecastPH/Data/infoParkhaus.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#add latest results
rawdata<- rbind(rawdata,logfile)
#save new dataframe
saveRDS(rawdata,"~/ForecastPH/Data/rawdata_new.RDS")

#######################
#number of days for lagging 
daysForLagging <- 15


#use just latest data for prediction
dateofLastEntry <- max(rawdata$time)

currentData <- filter(rawdata, time>=(dateofLastEntry- days((daysForLagging+1))))

#check if data is complete 
#if not replace by group means

  #crate sequence with all possible values
  timesequence<-seq(as.POSIXct(max(currentData$time)),length.out = (24*4*(daysForLagging+1)),by = "-15 min")
  allPossObserva <- expand.grid(datetime=as.POSIXct(timesequence),ID=infoParkhaus$ID)
  colnames(allPossObserva)[1]<- "time"
  allPossObserva <- merge.data.frame(allPossObserva, currentData, by=c("ID","time"), all.x = T)

  #calculate col means for replacement
  repValues <- aggregate.data.frame(list(current.capacity = allPossObserva$current.capacity, 
                                         total.capacity=allPossObserva$total.capacity), 
                                    by = list(ID=allPossObserva$ID), FUN = mean,  na.rm=TRUE)
  
 for (i in unique(allPossObserva$ID)){
   allPossObserva[allPossObserva$ID==i & is.na(allPossObserva$current.capacity) ,3:4] <- round(repValues[repValues$ID==i,2:3])
   
 }
  currentData<- select(allPossObserva,ID,current.capacity,time,total.capacity)
  
  colSums(is.na.data.frame(currentData))


#add values for prediction (add next 5 hours for all PHs)
timesequence<-seq(as.POSIXct(max(logfile$time)),length.out = 21,by = "15 min")
timesequence<-timesequence[-1]

#create a dataframe with all time and ph-Id combination
allPossObserva <- expand.grid(datetime=as.POSIXct(timesequence),ID=infoParkhaus$ID)
allPossObserva2<- as.data.frame(allPossObserva$ID)
colnames(allPossObserva2)<- "ID"
allPossObserva2["current.capacity"]<-NA
allPossObserva2["time"]<-allPossObserva$datetime
allPossObserva2 <- merge(allPossObserva2,logfile[-c(2,3)],by.x = "ID",by.y = "ID",all.x = T)

currentData<- rbind(currentData,allPossObserva2)


#onehot encoding fir PH ID

for (i in sort(unique(currentData$ID))){
  currentData[toString(paste0("OneHot_",i))]<- 0
  currentData[currentData$ID==i,toString(paste0("OneHot_",i))]<-1
}

#add some calendar fetures
currentData["KW"]<- isoweek(currentData$time)
currentData["Month"]<- month(currentData$time)
currentData["DayOfWeek"]<- wday(currentData$time)
currentData["Hour"] <-hour(currentData$time)
currentData["QuarterOfHour"]<- minute(currentData$time)

#order on time
currentData<- currentData[order(currentData$time,decreasing=T),]

#################################################################################################

#add IsSchoolHolidays
#1= is holiday
#load data

ferien <- read.csv("~/ForecastPH/Data/Ferien.csv", sep=";", stringsAsFactors=FALSE)
ferien[,"DTSTART.DATE"]<- as.Date(ferien$DTSTART.DATE,format ="%d.%m.%Y")
ferien[, "DTEND.DATE"]<- as.Date(ferien$DTEND.DATE,format ="%d.%m.%Y")

currentData["IsSchoolHolidays"]<-0

for (i in 1:nrow(ferien)) {
  currentData[between(as.Date(currentData$time),ferien[i,2],ferien[i,3]),"IsSchoolHolidays"]<-1
}


#add public holidays
#load data
feiertage <- read.csv("~/ForecastPH/Data/feiertage.csv", 
                      sep=";", stringsAsFactors=FALSE)
colnames(feiertage)<-c("Datum","Name")
feiertage["Datum"]<-as.Date(feiertage$Datum,format ="%d.%m.%Y")
feiertage <-feiertage[order(feiertage$Datum),]
singeleDates<-data.frame(list(Datum=unique(as.Date(currentData$time)),daysTillHolyday=NA))
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
currentData["Datum"]<- as.Date(currentData$time)
currentData <- merge(currentData, singeleDates, by= "Datum", all.x = T)

currentData$Datum<-NULL

#add laggs
Lags <-NULL

for (i in 1:daysForLagging) {
  Lags <- c(Lags, seq((96*i-8),length.out = 17))
  
}


for (t in Lags){
  currentData<- slide(currentData, Var="current.capacity",GroupVar="ID", NewVar=paste0("lag_",t), 
                      slideBy = -t, keepInvalid = T, reminder = FALSE,TimeVar = "time")
  print(t)
}

currentData<- filter(currentData,currentData$time>max(logfile$time))
prefix<- select(currentData,ID,time)
colSums(is.na.data.frame(currentData))

############################
###checken ob daten passen

#prepare data
onehotVal <- currentData[,grep(pattern = "OneHot",x = colnames(currentData)) ]
ids <- currentData$ID
currentData <- currentData[,-grep(pattern = "OneHot",x = colnames(currentData)) ]

test_df <- currentData[,-(1:3)]

rm("currentData")

# Test data is *not* used when calculating the mean and std.

# Use means and standard deviations from training set to normalize test set
###############
###########
###########
#load center and scaled#
col_means_train<-readRDS("~/ForecastPH/Data/col_means_train.RDS")
col_stddevs_train<-readRDS("~/ForecastPH/Data/col_stddevs_train.RDS")
test_df <- scale(test_df, center = col_means_train, scale = col_stddevs_train)

#add onehot
test_df <- cbind(onehotVal,test_df)

#convert to matrix
test_df<- as.matrix(test_df)


colnames(test_df)
#load model
if(!exists("model")){
  model<-load_model_hdf5("~/ForecastPH/Modells/LSTM/LSTMModell.hdf", custom_objects = NULL, compile = TRUE)
}


predictions <- model %>% predict(test_df)

rm("prediction","test_df")
gc()
prediction <- cbind(prefix,round(predictions))
saveRDS(prediction,"~/ForecastPH/Crawler/logfiles/Predictions/forecast_current.RDS")
forecast_all <-readRDS("~/ForecastPH/Crawler/logfiles/Predictions/forecast_all.RDS")
forecast_all <- rbind(forecast_all,prediction)
saveRDS(forecast_all,"~/ForecastPH/Crawler/logfiles/Predictions/forecast_all.RDS")
