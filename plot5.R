# Load Library
library(ggplot2)
library(dplyr)
# Importing Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Questions 5
# -------------------------------
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# After lookin variable in SCC data.frame I suppose that motor vehicle can be resumed as mobile vehicles in EI.Sector try it
par(mfrow = c(1, 1)) 

regExpQuery       <- "([Mm]obile.*[Vv]ehicles)"      # Search for Mobile Vehicles in EI.Sector
validItems        <- SCC$SCC[grep(regExpQuery,SCC$EI.Sector)]
subSetNEI         <- NEI[NEI$SCC %in% validItems, ]

Emissions <- 
      subSetNEI %>% 
      filter(fips == "24510") %>%
      group_by(year) %>%
      summarise(TotalEmission = sum(Emissions))

with(Emissions,
     barplot(TotalEmission,      
             main = expression("Total Mobile Vehicles emissions of PM "[2.5]*" in Baltymore" ), # set graph title
             names = year,                      # set Labels for bars
             xlab = "Years",                    # set label for x axis
             ylab = "Tons",         # set label for y axis               
             col = rgb(0.5 , 0.5, 0.5))            
)
# Save on png device
dev.copy(png, file="plot5.png", height=1024, width=1024)
dev.off()