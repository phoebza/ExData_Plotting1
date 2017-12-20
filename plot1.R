############ EXPLORATORY DATA ANALYSIS ###################

getDownloadedFile <- function(){
     setwd("~/R/Coursera/submissions/ExData_Plotting1/")
     
     fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
     filename = "consumption.zip"
     download.file(url = fileUrl, destfile = filename)
      
     unzip(zipfile = filename)
     
     files <- list.files(path = getwd(), pattern = ".txt")
     return(files)
}

# Determine system time:
determineSystemPerm <- function(){
     files <- getDownloadedFile()
     
     library(data.table)
     
     print(system.time(data <- fread(input = files
                               ,header = TRUE, sep = ";", na.strings = "?")))
}

plottingGraph1 <- function(){
     rm(list = ls())
     
     files <- getDownloadedFile()
     
     library(data.table)
     
     # reading from ".txt" file
     consData <- fread(input = files
                       ,header = TRUE, sep = ";", na.strings = "?")
     
     # convert the date variable to Date class
     consData$Date <- as.Date(consData$Date, format = "%d/%m/%Y")
     
     # using data from the dates 2007-02-01 and 2007-02-02
     consData.subset <- subset(x = consData, subset = (Date == "2007-02-01" | Date == "2007-02-02"))
     consData.subset
     
     # Convert dates and times
     consData.subset$datetime <- strptime(paste(consData.subset$Date, consData.subset$Time), "%Y-%m-%d %H:%M:%S")
     
     # Plotting and Saving to file
     attach(consData.subset)
     png(file = "plot1.png", height = 480, width = 480)
     hist(Global_active_power, main = "Global Active Power", 
          xlab = "Global Active Power (kilowatts)", col = "Red")

     dev.off()
     detach(consData.subset)
}