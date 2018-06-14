#Example for multihostograms
g <- ggplot(data = sumBaltimore, aes(x = year, y = sumPerYear,  fill = type))
g  + labs(title = "Baltimore Total PM2.5 Emissions", x = "Year", y = "Emissions (tons)") + 
      geom_bar(stat = "identity", position = "dodge")