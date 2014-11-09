plot2 <- function () {
  ## ---- Read the data -----
  rawdata <- read.csv("./data/household_power_consumption.txt", header=T, sep=';', na.strings="?", 
                      nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')

  
  ## format Date
  rawdata$Date <- as.Date(rawdata$Date, format="%d/%m/%Y")

  ## create a subset of rawdata using the criteria Feb 1 & 2, 2007
  usagedata <- subset(rawdata, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))

  ## remove rawdata
  rm(rawdata)

  ## combine Date and Time, create a column (datetime)
  datetime <- paste(as.Date(usagedata$Date), usagedata$Time)
  usagedata$Datetime <- as.POSIXct(datetime)

  ## create Plot 2
  plot(usagedata$Global_active_power ~ usagedata$Datetime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")

  ## save the plot as a png file
  dev.copy(png, file="plot2.png", height=480, width=480)
  dev.off()
}