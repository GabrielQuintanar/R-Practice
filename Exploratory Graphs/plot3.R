

if (!file.exists("/FNEI_data")) {
      url <-
            "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
      download.file(url, destfile = "FNEI_data.zip")
      unzip(zipfile = "FNEI_data.zip")
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$type <- as.factor(NEI$type)
NEI$year <- as.factor(NEI$year)

library(plyr)
library(dplyr)
library(ggplot2)

baltimore <- subset(NEI, fips == "24510")
sumBaltimore <- baltimore %>%
      group_by(type, year) %>%
      summarize(sumPerYear = sum(Emissions))
sumBaltimore <- as.data.frame(sumBaltimore)

g <-
      ggplot(data = sumBaltimore, aes(
            x = as.numeric(as.character(year)),
            y = sumPerYear,
            color = type
      ))
g + labs(title = "Baltimore Total PM2.5 Emissions", x = "Year", y = "Emissions (tons)") + 
      geom_line(stat = "identity", size = 1) +
      geom_point(size = 2.4)
ggsave("plot3.png",
       width = 12.7,
       height = 12.7,
       units = "cm")
