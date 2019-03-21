#LSTM Modell

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
# Core Tidyverse
pkgTest("tidyverse")
pkgTest("lubridate")
pkgTest("keras")
pkgTest("dplyr")
pkgTest("tibble")
pkgTest("Metrics")
pkgTest("DescTools")
pkgTest("DataCombine")




#load data
rawdata <- read.csv("~/ForecastPH/Data/all_data_20181115.csv")
infoParkhaus <- read_delim("ForecastPH/Data/infoParkhaus.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#clean Data
rawdata$X <- NULL
rawdata$isotime <- NULL
rawdata$time <- floor_date(as.POSIXct(rawdata$time,format="%d.%m.%Y%H:%M:%OS", 
                                      tz=toString(Sys.timezone(location = TRUE))),"15 minutes")

currentData <- rawdata

#onehot encoding PH IDs
for (i in sort(unique(currentData$ID))){
  currentData[toString(paste0("OneHot_",i))]<- 0
  currentData[currentData$ID==i,toString(paste0("OneHot_",i))]<-1
}

currentData["KW"]<- isoweek(currentData$time)
currentData["Month"]<- month(currentData$time)
currentData["DayOfWeek"]<- wday(currentData$time)
currentData["Hour"] <-hour(currentData$time)
currentData["QuarterOfHour"]<- minute(currentData$time)

#############################################################################################################

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



######################################################################################################
#order on time
currentData<- currentData[order(currentData$ID, currentData$time),]

#add laggs
Lags <-NULL
numofDaysLag <- 15
for (i in 1:numofDaysLag) {
  Lags <- c(Lags, seq((96*i-8),length.out = 17))
  
}


for (t in Lags){
  currentData<- slide(currentData, Var="current.capacity",GroupVar="ID", NewVar=paste0("lag_",t), 
                      slideBy = -t, keepInvalid = T, reminder = FALSE)
  print(t)
}


currentData<- filter(currentData,currentData$time>="2017-06-15")

#fill NAs with col means
for(i in 1:ncol(currentData)){
  currentData[is.na(currentData[,i]), i] <- mean(currentData[,i], na.rm = TRUE)
  print(paste("Col",i,"of",ncol(currentData)))
}
sum(is.na.data.frame(currentData))

#train test split
onehotVal <- currentData[,grep(pattern = "OneHot",x = colnames(currentData)) ]
ids <- currentData$ID
currentData <- currentData[,-grep(pattern = "OneHot",x = colnames(currentData)) ]


train_df <- currentData[,-(1:3)]
traiLabel <-as.matrix(currentData$current.capacity)


# Test data is *not* used when calculating the mean and std.

# Normalize training data
train_df <- scale(train_df) 

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_df, "scaled:center") 
col_stddevs_train <- attr(train_df, "scaled:scale")
#save scale and center to convert testdata
saveRDS(col_means_train,"~/ForecastPH/Data/col_means_train.RDS")
saveRDS(col_stddevs_train,"~/ForecastPH/Data/col_stddevs_train.RDS")


#add onehot
train_df <- cbind(onehotVal,train_df)

train_df<- as.matrix(train_df)
dim(train_df)

saveRDS(colnames(train_df),"colnames.RDS")
rm("train_data","rawdata")
rm("currentData")
gc()

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 512, activation = "relu",
                input_shape = dim(train_df)[2]) %>%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  model
}

model <- build_model()
model %>% summary()


# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    
epochs <- 500

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 10)

model <- build_model()
history <- model %>% fit(
  train_df,
  traiLabel,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 5))

save_model_hdf5(model, "~/ForecastPH/Modells/LSTM/LSTMModell.hdf", overwrite = TRUE,  include_optimizer = TRUE)

# load_model_hdf5(filepath, custom_objects = NULL, compile = TRUE)
# 
test_predictions <- model %>% predict(test_df)
rmse(testLabel,test_predictions)
# 

# reviewDF <- as.data.frame(ID=ids[(split+1):nrow(currentData)],)
# colnames(reviewDF)[1]<-"ID"
# reviewDF["trueValues"]<- testLabel
# reviewDF["predicValues"]<- test_predictions
# reviewDF["AbsErr"]<- abs(reviewDF$trueValues-reviewDF$predicValues)
# reviewDF["APE"]<- reviewDF$AbsErr/max(reviewDF$trueValues,1)*100
# aggreviewDF <- aggregate.data.frame(list(MAE=reviewDF$AbsErr, MAPE=reviewDF$APE),by = list(ID=reviewDF$ID),FUN = mean)
