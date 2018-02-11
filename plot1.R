require("data.table")
require("dplyr")

library(data.table)
library(dplyr)

getData <- function(){
  
  dataFileName <- "household_power_consumption.txt"
  
  if(!file.exists(dataFileName)){
    download.file(
      "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
      "EPC.zip",
      mode = "wb"
    )
    
    unzip("EPC.zip")
  }
  
  if(!exists("householdPowerConsumption")){
    householdPowerConsumption <- fread(dataFileName, na.strings="?")
  }
  
  householdPowerConsumption
}

cleanUpData <- function(householdPowerConsumption){
  householdPowerConsumption <- 
    householdPowerConsumption %>%
    mutate(DateTime = paste(Date, Time)) %>%
    mutate(DateTime = as.POSIXct(DateTime, format="%d/%m/%Y %H:%M:%S"))
}

filterData <- function(householdPowerConsumption){
  earliestDateTime <- as.POSIXct("01/02/2007 00:00:00", format="%d/%m/%Y %H:%M:%S")
  latestDateTime <- as.POSIXct("02/02/2007 23:59:59", format="%d/%m/%Y %H:%M:%S")
  
  householdPowerConsumption <- cleanUpData(householdPowerConsumption)
  
  householdPowerConsumption <- 
    householdPowerConsumption %>%
    filter(DateTime >= earliestDateTime) %>%
    filter(DateTime <= latestDateTime)
  
  householdPowerConsumption
}

createPlot1 <- function(){
  
  householdPowerConsumption <- getData()
  householdPowerConsumption <- filterData(householdPowerConsumption)
  
  
  png(file="plot1.png")
  
  hist(
    householdPowerConsumption$Global_active_power,
    col = "red",
    main = "Global Active Power",
    xlab = "Global Active Power (kilowatts)"
  )
  
  dev.off()
}

createPlot1()
