plot2 <- function(){
  
  # 2.
  # Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
  # (fips == "24510") from 1999 to 2008? 
  # Use the base plotting system to make a plot answering this question.
  
  library(ggplot2)
  
  #Set working directory for development
  pm25 <- readRDS("./data/summarySCC_PM25.rds",refhook = NULL)
  
  pm25$Emissions <- as.numeric(pm25$Emissions)
  pm25$year <- as.numeric(pm25$year)
  
  balt.em <- pm25[which(pm25$fips == "24510"), ]
  
  emissions.year <- with( balt.em,aggregate(Emissions, by = list(year), sum))
  
  png('plot2.png',width=480,height=480)
  
  plot(emissions.year
       ,main = "Baltimore City, Maryland (Emissions from PM2.5 by Year)"
       ,xlab = "Year"
       ,ylab = "Emissions"
       ,type = "l"
       ,col = "blue"
       ,xaxt = "n") #Supress x lables to customize axis
  
  axis(side = 1, at = c(emissions.year$Group.1))
  abline(v = emissions.year$Group.1, col='grey', lwd=0.5)
  
  #Finished plotting - cleanup
  dev.off()
  
}