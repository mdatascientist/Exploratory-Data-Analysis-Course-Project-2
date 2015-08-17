plot6 <- function(){
  
  # 6.
  # Compare emissions from motor vehicle sources in Baltimore City with emissions from 
  # motor vehicle sources in Los Angeles County, California (fips == "06037"). 
  # Which city has seen greater changes over time in motor vehicle emissions?
  
  library(plyr)
  library(ggplot2)
  
  #Set working directory for development
  pm25 <- readRDS("./data/summarySCC_PM25.rds",refhook = NULL)
  scs <- readRDS("./data/Source_Classification_Code.rds",refhook = NULL)
  
  #Return the Motor Vechicles SCC Identifiers used to filter pm25 by SCC Value
  scs_mv <- scs[scs$SCC %in% scs[grep("Onroad", scs$Data.Category), 1], ]
  
  #Filter pm25 by SCC
  pm25_balt <- pm25[which(pm25$fips == "24510"), ]
  pm25_balt$city <- "Baltimore City"
  
  pm25_la <- pm25[which(pm25$fips == "06037"), ]
  pm25_la$city <- "Los Angeles County"
  
  balt_la <- rbind(pm25_balt,pm25_la)
  
  #Merge the Two Data Frames
  pm25_balt_la <- merge(scs_mv,balt_la,by.x = "SCC", by.y = "SCC")
  names(pm25_balt_la)[names(pm25_balt_la)=="EI.Sector"] <- "Motor_Vehicle_Source" 
  
  pm25_balt_la$Motor_Vehicle_Source <- substr(pm25_balt_la$Motor_Vehicle_Source,18,50)
  
  sumEm <- ddply(pm25_balt_la,.(city, Motor_Vehicle_Source, year), summarize, emissions = sum(Emissions)/ 1000)
  
  qplot(sumEm$year
   ,sumEm$emissions 
   ,data = sumEm
   #,facets = ~ city
   ,group = Motor_Vehicle_Source
   ,color = Motor_Vehicle_Source   #Line Color
   ,geom = c("line")
   ,ylab = "Emissions (thousands)" 
   ,xlab = "Year") +
  facet_wrap(~ city, scales = "free_y", ncol = 1) + 
  ggtitle(expression(atop("Emissions from motor vehicle sources", atop(italic("Baltimore City vs Los Angles County"), "")))) +
  scale_fill_discrete(name="Experimental\nCondition") +
  theme(axis.text = element_text(size = 10)
    ,axis.line = element_line(size = 0.5,color = "grey")
    ,legend.key = element_rect(colour = "grey")
    ,plot.title = element_text(size = rel(1.5),color = "#474747")) 

  #Scale to 480px x 480px
  ggsave("plot6.png",width = 9.62, height = 9.62, units = "in", dpi = 50)

}