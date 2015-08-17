plot4 <- function(){
  
  # 4.
  # Across the United States, who have emissions from coal combustion-related 
  # sources changed from 1999-2008?

  library(ggplot2)
  library(plyr)
  
  #Set working directory for development
  
  pm25 <- readRDS("./data/summarySCC_PM25.rds",refhook = NULL)
  scs <- readRDS("./data/Source_Classification_Code.rds",refhook = NULL)
  
  #Return the Coal SCC Identifiers used to filter pm25 by SCC Value
  scs_Coal <- scs[scs$SCC %in% scs[grep("Coal", scs$EI.Sector), 1], ]
  #Filter pm25 by SCC
  pm25_Coal <- pm25[pm25$SCC %in% scs_Coal$SCC,]

  #Merge the Two Data Frames
  pm25_scs <- merge(scs_Coal,pm25_Coal,by.x = "SCC", by.y = "SCC")
  
  names(pm25_scs)[names(pm25_scs)=="EI.Sector"] <- "Coal_Source" 
  
  sumEm <- ddply(pm25_scs,.(Coal_Source, year), summarize, emissions = sum(Emissions)/ 1000)
  
  png('plot4.png',width=480,height=480)
  
  qplot(year
     ,emissions
     ,data = sumEm
     ,group = Coal_Source
     ,color = Coal_Source   #Line Color
     ,geom = c("line")
     ,ylab = "Emissions (thousands)" 
     ,xlab = "Year") +
  ggtitle(expression(atop("Emissions from coal ", atop("combustion-related sources"), ""))) 
  
  
  #Finished plotting - cleanup
  dev.off()
  
}