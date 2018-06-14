#We will only be using dates from 2007-02-01 and 2007-02-02
png("plot4.png", width = 480, height = 480)
file <- "household_power_consumption.txt"
data <- read.table(file = file, header = TRUE,
                   sep = ";", skip = 66636,nrows = (69518-66638), na.strings = "?")
colnames(data) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
library(lubridate)
data$Date <- dmy(data$Date)
data$Time <- hms(data$Time)
par(mar = c(3, 4, 1, 1), mfrow = c(2, 2))
#Plot 1
with(data, plot(x = Date + Time, y = Global_active_power, type = "l",
                xlab = "", ylab = "Global Active Power (kilowatts)"))
#plot 2
with(data, plot(x = Date + Time, y = Voltage, type = "l" ,
                xlab = "datetime"))
#Plot 3
with(data, plot(x = Date + Time, y = Sub_metering_1, type = "l",
                xlab = "datetime", 
                ylab = "Global Active Power (kilowatts)"))
with(data, lines(x= Date + Time, y = Sub_metering_2, col = "red", type = "l"))
with(data, lines(x= Date + Time, y = Sub_metering_3, col = "blue", type = "l"))
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", 
                              "Sub_metering_3"), col = c("black",
                                                         "red", "blue"), lty = 1, lwd = 2)
#Plot 4
with(data, plot(x = Date + Time, y = Global_reactive_power, type = "l",
                xlab = "datetime"))
dev.off()
