
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
pkgTest("caret")

#load data
inputdata <- readRDS("~/ForecastPH/Data/completeDataWithAllPH.RDS")
naValues <- as.data.frame(colSums(is.na.data.frame(inputdata)))

#fill NAs
inputdata<-na.locf(inputdata, fromLast = T)
naValues <- as.data.frame(colSums(is.na.data.frame(inputdata)))

head(inputdata[1:100,1:20])
inputdata <- inputdata[,-c(1,2)]
## Scale data for neural network

# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(inputdata, method=c("range"))
# summarize transform parameters
# transform the dataset using the parameters
transformed <- predict(preprocessParams, inputdata)