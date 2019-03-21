
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

nameList <- colnames(completeData)
head(nameList,100)
saveRDS(completeData,"~/ForecastPH/Data/completeDataWithAllPH_SeqLag.RDS")
