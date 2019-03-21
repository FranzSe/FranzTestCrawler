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
  pkgTest("rvest")
  pkgTest("tidyverse")
  pkgTest("rvest")
  pkgTest("rdwd")
  pkgTest("tidyquant")
  pkgTest("lubridate")
  pkgTest("R.utils")
  pkgTest("xgboost")

  #initialize dataframe
  
  logfile<-setNames(data.frame(matrix(ncol = 4, nrow = 0)), 
                    c("datetime", "ID", "current_capacity","totalCapacity"))
  
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
      logfile[currentRow,1]<- as.character(floor_date(Sys.time(),"15 minutes"))
      logfile[currentRow,2]<- t
      logfile[currentRow,3]<- currentData[2]
      logfile[currentRow,4]<- currentData[1]
    }
  logfile$datetime<-as.POSIXct(logfile$datetime)
  
  # path <- paste0("~/ForecastPH/Crawler/logfiles/log_", toString(Sys.time()),".csv")
  # path <- str_replace_all(path,c(" "="_",":"="_"))
  # write_csv(logfile,path)
  
#############Crawler ENDE################################
  
  
  allValues<- logfile
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
  link <- selectDWD(id = 5705, res="daily", var="kl", per="r", outvec=T)
  file <- dataDWD(link, read=T)
  file[550,2]<- "2019-02-27"
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
  sum(is.na.data.frame(allValues))
  allValues<-na.locf(allValues)
  sum(is.na.data.frame(allValues))
  
##################EDNE Feature ADDS###########################################################################
  #load hist data
  finalData <- readRDS("~/ForecastPH/Data/finalData.RDS")
  colnames(finalData)[4:5]<- c("current_capacity", "totalCapacity")
  #find missing datapoints
  #creatate a timesequence with all possible timevalues between first and last rwo
  timesequence<- seq(max(finalData$datetime),min(allValues$datetime), "15 min")
  timesequence<-timesequence[1:(length(timesequence)-1)]
  infoParkhaus <- read.csv("~/ForecastPH/Data/infoParkhaus.csv",row.names=NULL, sep=";")
  
  #create a dataframe with all time and ph-Id combination
  allPossObserva <- expand.grid(datetime=as.POSIXct(timesequence),ID=infoParkhaus$ID)
  allPossObserva<- cbind(as.Date(allPossObserva$datetime),allPossObserva)
  colnames(allPossObserva)[1]<-"Datum"

  allPossObserva["current_capacity"]<- NA
  allPossObserva["totalCapacity"]<- NA
  allPossObserva["KW"]<- NA
  allPossObserva["Month"]<- NA
  allPossObserva["DayOfWeek"]<- NA
  allPossObserva["Hour"]<- NA
  allPossObserva["IsSchoolHolidays"]<- NA
  allPossObserva["daysTillHolyday"]<- NA
  allPossObserva["SDK"]<- NA
  allPossObserva["RSK"]<- NA
  allPossObserva["TMK"]<- NA
  head(allPossObserva)
  tail(allPossObserva)
  
#add missing values
  finalData <- rbind(finalData,allPossObserva)
  saveRDS(finalData,"~/ForecastPH/Data/finalData.RDS")
#add next 5 hours
  timesequence<- seq(max(finalData$datetime),(max(finalData$datetime)+ (5*60*60)), "15 min")
  timesequence<-timesequence[2:length(timesequence)]
  infoParkhaus <- read.csv("~/ForecastPH/Data/infoParkhaus.csv",row.names=NULL, sep=";")
   #create a dataframe with all time and ph-Id combination
  allPossObserva <- expand.grid(datetime=as.POSIXct(timesequence),ID=infoParkhaus$ID)
  allPossObserva<- cbind(as.Date(allPossObserva$datetime),allPossObserva)
  colnames(allPossObserva)[1]<-"Datum"
  allPossObserva["current_capacity"]<- NA
  allPossObserva["totalCapacity"]<- NA
  allPossObserva["KW"]<- NA
  allPossObserva["Month"]<- NA
  allPossObserva["DayOfWeek"]<- NA
  allPossObserva["Hour"]<- NA
  allPossObserva["IsSchoolHolidays"]<- NA
  allPossObserva["daysTillHolyday"]<- NA
  allPossObserva["SDK"]<- NA
  allPossObserva["RSK"]<- NA
  allPossObserva["TMK"]<- NA
  
  finalData<- rbind(finalData,allPossObserva)
  
  finalData<-na.locf(finalData)
  sum(is.na.data.frame(finalData))
  #add future values
  finalData <- rbind(finalData,allValues)

  #filter to speed up
  finalData<- filter(finalData,finalData$datetime>=(max(finalData$datetime)-days(8)))

  
################################EDNDE Rohdaten########################
  
  lookup <- filter(finalData, ID==5325)
  lookup <- as.data.frame(lookup$datetime)
  colnames(lookup) <-"datetime"
  lookup["RoundHour"]<- floor_date(lookup$datetime,"hour")
  
  
  for (i in ids){
    currentPH <- filter(finalData, ID==i)
    lookup[toString(i)]<- currentPH[,4]
  }
  
  groupedLookUp <- aggregate.data.frame(lookup, by= list(hour=lookup$RoundHour),FUN = mean)
  groupedLookUp<- groupedLookUp[,-c(1,2)]
  
  finalData["RoundHour"]<- floor_date(finalData$datetime,"hour")
  
  finalDataMerged <- merge(finalData,groupedLookUp, all.x=TRUE)
  finalDataMerged<- finalDataMerged[order(finalDataMerged$ID,finalDataMerged$datetime),]
  
  colNamesVec <- as.character(ids)
  
  capaMatrxStartVal <- finalDataMerged[colNamesVec]
  completeData<- finalDataMerged[,1:14]
  
  #delete first 5h
  
  nameVector <- colnames(capaMatrxStartVal)
  laggseq <-c(seq(20,40),seq(90,110),seq(660,685))
  for (i in laggseq){
    capaMatrx <- capaMatrxStartVal
    colnames(capaMatrx) <- paste(nameVector, "lag",toString(i), sep = "_")
    NADataF <-data.frame(matrix(NA, nrow = i, ncol = 24))
    colnames(NADataF)<- colnames(capaMatrx)
    capaMatrx<- rbind(NADataF,capaMatrx)
    capaMatrx <- capaMatrx[1:nrow(capaMatrxStartVal),]
    completeData <- cbind(completeData,capaMatrx)
    print(i)
  }
  
  completeData <- completeData[order(completeData$datetime, completeData$ID),]
  

  ##############ENDE Features adding#####################################
  
  numOfRows <- (nrow(completeData)-480) 
  finalData <- completeData[numOfRows:nrow(completeData),-c(1,2)]
  #reorder dataframe
  finalData <- finalData[order(finalData$datetime, finalData$ID), ]
  timeVec<- finalData[,1:2]
  colnames(timeVec)[2]<-"ValidID"
  finalData <-finalData[,-1]
  
  # Build X_train, y_train, X_test, y_test
  #label index
  labelIndex= 2

  X_test <- as.matrix(finalData[ , -labelIndex])
  y_test <- as.matrix(finalData[ , labelIndex])
  dtest <- xgb.DMatrix(X_test, label=y_test)
  
  if (!exists("modelXGB")) {
    modelXGB <- xgb.load("xgbAllPH_Seq.model")
  } 
  
  prediction <- predict(modelXGB,dtest)
  
  timeVec["Forecast"]<- prediction
  write.csv(timeVec,"~/ForecastPH/Crawler/logfiles/forecast.csv")
  
  # path <- paste0("~/ForecastPH/Crawler/logfiles/log_", toString(Sys.time()),".csv")
  # path <- str_replace_all(path,c(" "="_",":"="_"))
  # write_csv(logfile,path)