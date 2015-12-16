# clean up workspace
rm(list = ls())
# Load Library
library(ggplot2)
library(dplyr)
# Importing Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Questions
# -------------------------------
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.

# str(NEI)
# 'data.frame':      6497651 obs. of  6 variables:
# $ fips     : chr  "09001" "09001" "09001" "09001" ...
# $ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
# $ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
# $ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
# $ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
# $ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...

# Use dplyr verb and chain to extract data

Emissions <- 
      NEI %>%      
      group_by(year) %>%
      summarise(TotalEmission = sum(Emissions)/1e6)

par(mfrow = c(1, 1)) # Reset any multiple plot

with(Emissions,
      barplot(TotalEmission,      
            main = expression("Total Emission from PM"[25] ), # set graph title
            names = year,          # set Labels for bars
            xlab = "Years",                # set label for x axis
            ylab = "Millions of Tons",
            col = rgb(0.7 , 0.7, 0.7, alpha = 0.2))     # set label for y axis      
      )


# Save on png device
dev.copy(png, file="plot1.png", height=1024, width=1024)
dev.off()