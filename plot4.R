plot4 <- function () {
  
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

  ## create Plot 4
  par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
  with(usagedata, {
    plot(Global_active_power~Datetime, type="l", 
         ylab="Global Active Power", xlab="")
    plot(Voltage~Datetime, type="l", 
         ylab="Voltage", xlab="datetime")
    plot(Sub_metering_1~Datetime, type="l", 
         ylab="Global Active Power", xlab="")
    lines(Sub_metering_2~Datetime,col='Red')
    lines(Sub_metering_3~Datetime,col='Blue')
    legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),bty="n", cex = .5)
    plot(Global_reactive_power~Datetime, type="l", 
         ylab="Global_Rective_Power",xlab="datetime")
  })
  ## save the plot as a png file
  dev.copy(png, file="plot4.png", height=480, width=480)
  dev.off()
}