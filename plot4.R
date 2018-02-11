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

createPlot4 <- function(){
  
  householdPowerConsumption <- getData()
  householdPowerConsumption <- filterData(householdPowerConsumption)
  
  
  png(file="plot4.png")
  
  par(
    mfrow = c(2,2),
    mar = c(6, 6, 3, 2)
  )
  
  createPlot4a(householdPowerConsumption)
  createPlot4b(householdPowerConsumption)
  createPlot4c(householdPowerConsumption)
  createPlot4d(householdPowerConsumption)
  
  dev.off()
}

createPlot4a <- function(householdPowerConsumption){
  
  with(householdPowerConsumption, {
    plot(
      x = DateTime,
      y = Global_active_power,
      type = "n",
      xlab = "",
      ylab = "Global Active Power"
    )
    
    lines(
      x = DateTime,
      y = Global_active_power,
      type = "l"
    )
  })
}

createPlot4b <- function(householdPowerConsumption){
  
  with(householdPowerConsumption, {
    plot(
      x = DateTime,
      y = Voltage,
      type = "n",
      xlab = "datetime",
      ylab = "Voltage"
    )
    
    lines(
      x = DateTime,
      y = Voltage,
      type = "l"
    )
  })
}

createPlot4c <- function(householdPowerConsumption){
  
  with(householdPowerConsumption, {
    plot(
      x = DateTime,
      y = Sub_metering_1,
      type = "n",
      xlab = "",
      ylab = "Energy sub metering"
    )
    
    lines(
      x = DateTime,
      y = Sub_metering_1,
      type = "l"
    )
    
    lines(
      x = DateTime,
      y = Sub_metering_2,
      type = "l",
      col = "red"
    )
    
    lines(
      x = DateTime,
      y = Sub_metering_3,
      type = "l",
      col = "blue"
    )
    
    legend(
      "topright",
      c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
      lty = c(1, 1, 1),
      col = c("black", "red", "blue"),
      bty = "n"
    )
  })
}

createPlot4d <- function(householdPowerConsumption){
  
  with(householdPowerConsumption, {
    plot(
      x = DateTime,
      y = Global_reactive_power,
      type = "n",
      xlab = "datetime"
    )
    
    lines(
      x = DateTime,
      y = Global_reactive_power,
      type = "l"
    )
  })
}

createPlot4()
