
if(!file.exists("/FNEI_data")){
      url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
      download.file(url, destfile = "FNEI_data.zip")
      unzip(zipfile = "FNEI_data.zip")
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$EmissionsKt <- NEI$Emissions/1000
summs <- with(NEI, tapply(EmissionsKt, year, sum))

png("plot1.png", width = 480, height = 480)
par(mar = c(3, 5, 2, 1), omi = c(0.1, 0.1, 0.1, 0.1), mgp = c(4, 1, 0), las = 1,
    mex = 0.9, cex.main = 1, cex.lab = 1, cex.axis = 0.8)
plot(x = names(summs), y = summs, type = "l", main = "Total PM2.5 emissions 1999-2008",
     xlab = "", ylab = "Emissions (Kilotons)", col = "red", lwd = 2)
dev.off()
