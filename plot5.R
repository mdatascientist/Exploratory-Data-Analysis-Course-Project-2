plot5 <- function(){
  
  5.
  # How have emissions from motor vehicle sources changed from 1999-2008 
  # in Baltimore City?
  
  library(plyr)
  library(ggplot2)
  
  #Set working directory for development
  pm25 <- readRDS("./data/summarySCC_PM25.rds",refhook = NULL)
  scs <- readRDS("./data/Source_Classification_Code.rds",refhook = NULL)
  
  #Return the Motor Vechicles SCC Identifiers used to filter pm25 by SCC Value
  scs_mv <- scs[scs$SCC %in% scs[grep("Onroad", scs$Data.Category), 1], ]
  #Filter pm25 by SCC
  pm25_scs <- pm25[which(pm25$fips == "24510"), ]
  
  #Merge the Two Data Frames
  pm25_scs <- merge(scs_mv,pm25_scs,by.x = "SCC", by.y = "SCC")

  names(pm25_scs)[names(pm25_scs)=="EI.Sector"] <- "Motor_Vehicle_Source" #Rename type to polType, type is a reserved word
  
  pm25_scs$Motor_Vehicle_Source <- substr(pm25_scs$Motor_Vehicle_Source,18,50)
  
  sumEm <- ddply(pm25_scs,.(Motor_Vehicle_Source, year), summarize, emissions = sum(Emissions)/ 1000)
  
  png('plot5.png',width=480,height=480)
  
  qplot(year
      ,emissions
      ,data = sumEm
      ,group = Motor_Vehicle_Source
      ,color = Motor_Vehicle_Source   #Line Color
      ,geom = c("line")
      ,ylab = "Emissions (thousands)" 
      ,xlab = "Year") +
    ggtitle(expression(atop("Emissions from motor vehicle sources", atop(italic("Baltimore City"), "")))) +
    scale_fill_discrete(name="Experimental\nCondition") +
    theme(axis.text = element_text(size = 10)
          ,axis.line = element_line(size = 0.5,color = "grey")
          ,legend.key = element_rect(colour = "grey")
          ,plot.title = element_text(size = rel(1.5),color = "#474747")) 
  
  
  #Finished plotting - cleanup
  dev.off()
}