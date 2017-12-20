############ EXPLORATORY DATA ANALYSIS ###################

getDownloadedFile <- function(){
     setwd("~/R/Coursera/submissions/ExData_Plotting1/")
     
     fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
     filename = "consumption.zip"
     download.file(url = fileUrl, destfile = filename)

     unzip(zipfile = filename )
     
     files <- list.files(path = getwd(), pattern = ".txt", recursive = TRUE)
     return(files)
}

# Determine system time:
determineSystemPerm <- function(){
     files <- getDownloadedFile()
     
     library(data.table)
     
     print(system.time(data <- fread(input = files
                               ,header = TRUE, sep = ";", na.strings = "?")))
}

plottingGraph3 <- function(){
     rm(list = ls())
     
     files <- getDownloadedFile()
     
     # reading from ".txt" file
     consData <- read.table(file = files
                       ,header = TRUE, sep = ";", na.strings = "?")
     
     # convert the date variable to Date class
     consData$Date <- as.Date(consData$Date, format = "%d/%m/%Y")
     
     # using data from the dates 2007-02-01 and 2007-02-02
     consData.subset <- subset(x = consData, subset = (Date == "2007-02-01" | Date == "2007-02-02"))
     print(head(consData.subset))
     
     # Convert dates and times
     consData.subset$datetime <- strptime(paste(consData.subset$Date, consData.subset$Time), "%Y-%m-%d %H:%M:%S")
     print(head(consData.subset))
     
     # Plotting and Saving to file
     consData.subset$datetime <- as.POSIXct(consData.subset$datetime)
     
     attach(consData.subset)
     plot(datetime, Sub_metering_1, type = "l", ylab = "Energy sub metering", xlab = "", cex.lab=1.5)
     lines(datetime, Sub_metering_2, col="red")
     lines(datetime, Sub_metering_3, col="blue")
     # Adding legends
     legend("topright", xpd = TRUE, cex=1, pt.cex(2), lty=1, col = c("black", "red", "blue")
            , legend=c('Sub_metering_1','Sub_metering_2','Sub_metering_3'))
     
     dev.copy(device = png, file = "plot3.png", height = 480, width = 480)
     
     dev.off()
     detach(consData.subset)
}