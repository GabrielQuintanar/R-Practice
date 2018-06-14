#We will only be using dates from 2007-02-01 and 2007-02-02
png("plot1.png", width = 480, height = 480)
file <- "household_power_consumption.txt"
data <- read.table(file = file, header = TRUE,
                   sep = ";", skip = 66636,nrows = (69518-66638), na.strings = "?")
colnames(data) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
library(lubridate)
data$Date <- dmy(data$Date)
data$Time <- hms(data$Time)
par(mar = c(4, 4, 1, 1), omi = c(0.1, 0.1, 0.1, 0.1), mgp = c(3, 0.5, 0), las = 1,
    mex = 0.8, cex.main = 1, cex.lab = 1, cex.axis = 0.8)
with(data, hist(Global_active_power, main = "Global Active Power", 
                col = "red", xlab = "Global Active Powe (kilowatts)",
                ylab = "Frequency", breaks = 12))
dev.off()
