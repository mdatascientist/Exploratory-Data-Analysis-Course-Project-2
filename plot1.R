plot1 <- function(){
  
  # 1.
  # Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
  # Using the base plotting system, make a plot showing the total PM2.5 emission 
  # from all sources for each of the years 1999, 2002, 2005, and 2008.

  #Set working directory for development
  
  pm25 <- readRDS("./data/summarySCC_PM25.rds",refhook = NULL)
  
  pm25$Emissions <- as.numeric(pm25$Emissions) / 1000
  pm25$year <- as.numeric(pm25$year)
  
  emissions.year <- with(pm25,aggregate(Emissions, by = list(year), sum))
  
  png('plot1.png',width=480,height=480)
  
  plot(emissions.year, main="Total Emissions from PM2.5 by Year"
       ,xlab = "Year"
       ,ylab = "Emissions (Millions)"
       ,type = "l"
       ,col = "blue"
       ,xaxt = "n") #Supress x lables to customize axis
  
  axis(side = 1, at = c(emissions.year$Group.1))
  abline(v = emissions.year$Group.1, col='grey', lwd=0.5)
  
  #Finished plotting - cleanup
  dev.off()
  
}