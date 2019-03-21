
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("dplyr")
pkgTest("tidyverse")
pkgTest("tidyquant")


finalData <- readRDS("~/ForecastPH/Data/finalData.RDS")
finalData <- finalData[,2:14]
colnames(finalData)[colnames(finalData)=="current capacity"] <- "current_capacity"
aggregate(finalData$ID, by=list("ID"=finalData$ID),FUN=length)
ids <-(unique(finalData$ID))

lookup <- filter(finalData, ID==5325)
lookup <- lookup[,1]
lookup["RoundHour"]<- floor_date(lookup$datetime,"hour")

 
for (i in ids){
  currentPH <- filter(finalData, ID==i)
  lookup[toString(i)]<- currentPH[,3]
}

groupedLookUp <- aggregate.data.frame(lookup, by= list(hour=lookup$RoundHour),FUN = mean)
groupedLookUp<- groupedLookUp[,-c(1,2)]

finalData["RoundHour"]<- floor_date(finalData$datetime,"hour")

finalDataMerged <- merge(finalData,groupedLookUp, all.x=TRUE)

#reorder DF
finalDataMerged <- finalDataMerged[order(finalDataMerged$ID,finalDataMerged$datetime),]

#lag capacity data
colNamesVec <- as.character(ids)
# for (i in colNamesVec){
# allValues <-  tq_mutate(data = finalDataMerged,
#                         select     = i,
#                         mutate_fun = lag.xts,
#                         k          =seq(20,100))
# print(paste("PH",i,"finish"))
# }


capaMatrx <- finalDataMerged[colNamesVec]
completeData<- finalDataMerged[,1:14]

#delete first 5h
for (i in 1:20){
capaMatrx<- rbind( rep(NA,24),capaMatrx)
capaMatrx <- capaMatrx[-nrow(capaMatrx) ,]
}
nameVector <- colnames(capaMatrx)
for (i in 20:100){
colnames(capaMatrx) <- nameVector
colnames(capaMatrx) <- paste(colNamesVec, "lag",toString(i), sep = "_")
capaMatrx<- rbind( rep(NA,24),capaMatrx)
capaMatrx <- capaMatrx[-nrow(capaMatrx) ,]
completeData <- cbind(completeData,capaMatrx)
print(i)
}
nameList <- colnames(completeData)
head(nameList,100)
saveRDS(completeData,"~/ForecastPH/Data/completeDataWithAllPH.RDS")
