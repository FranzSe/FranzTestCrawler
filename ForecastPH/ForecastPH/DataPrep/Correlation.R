#find corrlations within the timeseries and with other timeseries

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
pkgTest("corrplot")


#import data
rawdata <- read_csv("~/ForecastPH/Data/all_data_20181115.csv")

#delete columns
rawdata$X1 <- NULL
rawdata$isotime <- NULL
#check if there are NAs
colSums(is.na.data.frame(rawdata))
#change dateformat and round to the next 15 minutes 
rawdata["datetime"] <-floor_date(as.POSIXct(rawdata$time,format="%d.%m.%Y%H:%M:%OS", 
                                            tz=toString(Sys.timezone(location = TRUE))),"15 minutes")

#load ph info file
infoParkhaus <- read.csv("~/ForecastPH/Data/infoParkhaus.csv", 
                         row.names=NULL, sep=";")

colnames(infoParkhaus)[1] <- "ID"





#get autocorellation within the timeseries
##############################################

acfTotal<-setNames(data.frame(matrix(ncol = 1, nrow = 1001)), 
                   c("ID"))



for (i in infoParkhaus$ID){
  test<-filter(rawdata,ID==i)
  testacf<- acf(test$`current capacity`,lag.max = 1000,plot = FALSE)
  
  acfTotal[toString(i)]<- testacf$acf
}
acfTotal$ID<-NULL

#convert to absolute values and calculate row mean
absAcfTotal <- abs(acfTotal)

absAcfTotal["Mean"]<-rowMeans(absAcfTotal)
absAcfTotal$idu <- as.numeric(row.names(absAcfTotal))

ggplot(absAcfTotal, aes(x= absAcfTotal$idu, y=absAcfTotal$Mean))+
  geom_line()

#find top correlation datapoints
absAcfTotal <-absAcfTotal[order(-absAcfTotal$Mean),]

#show top 50 correlation
top50corr<-absAcfTotal[1:50,c("Mean","idu")]
top50corr<- top50corr[order(top50corr$idu),]
top50corr["days"]<- top50corr$idu/(24*4)

ggplot(top50corr,aes(x=top50corr$days,y=top50corr$Mean))+
  geom_point()+
  scale_x_continuous(breaks = round(seq(0, 8, by = 0.25),1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Tage")+
  ylab("Mittlere Korrelation")
#es is deutlich zu erkkenen, dass eine hohe Korrelation mit den vergangenen 6h sowie dem Vorwochentag besteht.

write.csv(top50corr,"~/ForecastPH/Data/top50Corr.csv")
###############################################################################
#compare with other PH
####################################################

allValues <- read_csv("ForecastPH/Data/allValues.csv")
infoParkhaus <- read.csv("~/ForecastPH/Data/infoParkhaus.csv", 
                         row.names=NULL, sep=";")
colnames(infoParkhaus)[1] <- "ID"

#drop time column
allValues$time<-NULL

allValues$`total capacity`<-NULL

allValues<- allValues[order(c(allValues$ID, allValues$datetime)),]



aggregate.data.frame(allValues, by= list(allValues$ID),FUN=length)
phMatrix<-setNames(data.frame(matrix(ncol = 1, nrow = 51382)), 
                   c("Datetime"))
phMatrix["Datetime"] <- 

for (i in infoParkhaus$ID){
  
  filteredData <- filter(allValues,ID==i)
  phMatrix[toString(i)]<-filteredData[,3]
}

corrMatrix <- cor(phMatrix, method = "pearson", use = "complete.obs")
corrplot(corrMatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#fast alle Parkhäuser weisen eine starke correlation zueinader auf --> daher sollten alle im Forecast berücksichtig werden