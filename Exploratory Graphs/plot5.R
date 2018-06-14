
if(!file.exists("/FNEI_data")){
      url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
      download.file(url, destfile = "FNEI_data.zip")
      unzip(zipfile = "FNEI_data.zip")
}
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$type <- as.factor(NEI$type)
NEI$year <- as.factor(NEI$year)

library(plyr);library(dplyr);library(ggplot2)

NEI_SCC <- left_join(NEI, SCC, by = "SCC")
NData <- NEI_SCC %>%
      filter(fips == "24510") %>%
      select(Emissions, year, EI.Sector) %>%
      slice(grep("[Mm]obile", EI.Sector)) %>%
      group_by(year) %>%
      summarize(sumPerYear = sum(Emissions))

g <- ggplot(data = NData, aes(x = as.numeric(as.character(year)), y = sumPerYear, color = "red"))
g + labs(title = "Baltimore City Mobile Combustion Total PM2.5 Emissions", x = "Year", y = "Emissions (tons)") + 
      geom_line(stat = "identity", size = 1) + geom_point(size = 2.4) + 
      scale_color_discrete(name = "State", breaks = c("red"), labels = c("Baltimore City"))
ggsave("plot5.png", width = 12.7, height = 12.7, units = "cm")
