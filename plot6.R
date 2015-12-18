# Load Library
library(ggplot2)
library(dplyr)
library(gridExtra)
# Importing Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Questions 6
# -------------------------------
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

# After lookin variable in SCC data.frame I suppose that motor vehicle can be resumed as mobile vehicles in EI.Sector try it

par(mfrow = c(1, 1)) 

regExpQuery       <- "([Mm]obile.*[Vv]ehicles)"      # Search for Mobile Vehicles in EI.Sector
validItems        <- SCC$SCC[grep(regExpQuery,SCC$EI.Sector)]
subSetNEI         <- NEI[NEI$SCC %in% validItems, ]

Emissions <- 
      subSetNEI %>% 
      filter(fips == "24510" | fips == "06037") %>%
      group_by(year,fips) %>%
      summarise(TotalEmission = sum(Emissions))

#  For each city I compute "change "as follow :
# 1. Absolute change : Emission over the years are referred to 1999 -> (Emissions - Emission1999)
# 2. Relative change : Emission over the year are in percentage respect to 1999 -> (Emissions - Emission2008)/Emission1999


offsetA = Emissions$TotalEmission[Emissions$year == 1999 & Emissions$fips == "06037"]
offsetB = Emissions$TotalEmission[Emissions$year == 1999 & Emissions$fips == "24510"]

Emissions$AbsChange[Emissions$fips == "06037"] <- Emissions$TotalEmission[Emissions$fips == "06037"] - offsetA
Emissions$AbsChange[Emissions$fips == "24510"] <- Emissions$TotalEmission[Emissions$fips == "24510"] - offsetB

Emissions$RelChange[Emissions$fips == "06037"] <- (Emissions$TotalEmission[Emissions$fips == "06037"] - offsetA)/offsetA
Emissions$RelChange[Emissions$fips == "24510"] <- (Emissions$TotalEmission[Emissions$fips == "24510"] - offsetB)/offsetB


p1 <- ggplot(data = Emissions, aes(x = year, y = AbsChange, colour = fips, group = fips)) + 
      geom_line(size = 1) + 
      geom_point(size = 5) +
      ylab(expression("PM"[25]*" Tons Variation")) + 
      scale_color_manual("City\n",labels = c("Los Angeles\nCounty", "Baltimore City"), values = c("blue", "red")) + 
      geom_text(data = NULL, x = 2004, y = -0.25, color = "blue", label = "Los Angeles County show the\nlargest variation in tons\nsince 1999") + 
      ggtitle("Absolute Variation of PM25 emission respect to 1999")
      

p2 <- ggplot(data = Emissions, aes(x = year, y = RelChange, colour = fips, group = fips)) + 
      geom_line(size = 1) + 
      geom_point(size = 5) +
      ylab(expression("PM"[25]*" % variation respect to 1999")) + 
      scale_color_manual("City\n",labels = c("Los Angeles\nCounty", "Baltimore City"), values = c("blue", "red")) + 
      geom_text(data = NULL, x = 2004, y = -0.25, label = "Baltimore City reduced\nby 75% the emissions\nfrom PM25 since 1999") + 
      ggtitle("Variation % of PM25 emission respect to 1999")


grid.arrange(p1,p2)

# Save on png device
dev.copy(png, file="plot6.png", height=1024, width=1024)
dev.off()