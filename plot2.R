# Load Library
library(ggplot2)
library(dplyr)
# Importing Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Questions 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# str(NEI)
# 'data.frame':      6497651 obs. of  6 variables:
# $ fips     : chr  "09001" "09001" "09001" "09001" ...
# $ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
# $ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
# $ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
# $ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
# $ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...


Emissions <- 
      NEI %>%
      filter(fips == "24510") %>%
      group_by(year) %>%
      summarise(TotalEmission = sum(Emissions)/1e3)

with(Emissions,
     barplot(TotalEmission,      
             main = expression("Total Emission from PM"[25]*" In Baltimore" ), # set graph title
             names = year,          # set Labels for bars
             xlab = "Years",                # set label for x axis
             ylab = "Kilo of Tons")     # set label for y axis      
)

# Save on png device
dev.copy(png, file="plot2.png", height=1024, width=1024)
dev.off()