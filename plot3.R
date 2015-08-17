plot3 <- function(){
  
  # 3.
  # Of the four types of sources indicated by the type 
  # (point, nonpoint, onroad, nonroad) variable, which of these four sources 
  # have seen decreases in emissions from 1999-2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999-2008? 
  # Use the ggplot2 plotting system to make a plot answer this question.
  library(ggplot2)
  library(plyr)
  
  #Set working directory for development
  pm25 <- readRDS("./data/summarySCC_PM25.rds",refhook = NULL)
  
  pm25$Emissions <- as.numeric(pm25$Emissions)
  pm25$year <- as.numeric(pm25$year)

  balt.em <- pm25[which(pm25$fips == "24510"), ]
  names(balt.em)[names(balt.em)=="type"] <- "Pollutant_Type" #Rename type to Pollutant_Type
    
  sumEm <- ddply(balt.em,.(Pollutant_Type, year), summarize, emissions = sum(Emissions))

  png('plot3.png',width=480,height=480)
  
  qplot(year
         ,emissions
         ,data = sumEm
         ,group = Pollutant_Type
         ,color = Pollutant_Type   #Line Color
         ,geom = c("line")
         ,ylab = "Emissions" 
         ,xlab = "Year") +
  ggtitle(expression(atop("Baltimore City, Maryland", atop("Emissions by Year / Pollutant Type"), ""))) 


  #Finished plotting - cleanup
  dev.off()

}