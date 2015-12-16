# Load Library
library(ggplot2)
library(dplyr)
# Importing Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Questions
# -------------------------------
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

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
      group_by(year,type) %>%
      summarise(TotalEmission = sum(Emissions))

str(Emissions)

p <- ggplot(Emissions, aes(x = factor(year), y = TotalEmission))
p +   geom_bar(stat = "identity") +      
      facet_grid( .~ type) +
      geom_smooth(method = "lm", se=F, color="#73C2FB", aes(group=1), size=0.5) + 
      ylab(expression(paste("PM"[2.5], " Emissions"))) + 
      xlab('Year') + 
      ggtitle('Emissions per Type in Baltimore')

# Save on png device
dev.copy(png, file="plot3.png", height=1024, width=1024)
dev.off()
