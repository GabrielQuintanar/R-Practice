#We will only be using dates from 2007-02-01 and 2007-02-02
png("plot2.png", width = 480, height = 480)
file <- "household_power_consumption.txt"
data <- read.table(file = file, header = TRUE,
                   sep = ";", skip = 66636,nrows = (69518-66638), na.strings = "?")
colnames(data) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
library(lubridate)
data$Date <- dmy(data$Date)
data$Time <- hms(data$Time)
par(mar = c(3, 4, 1, 1))
with(data, plot(x = Date + Time, y = Global_active_power, type = "l",
                xlab = "", ylab = "Global Active Power (kilowatts)"))
dev.off()
