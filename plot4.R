# Load Library
library(ggplot2)
library(dplyr)
# Importing Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Questions 4
# -------------------------------
#Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?


# Take a look to SCC it's seems that Fuel Comb and Coal can be determined using EI.Sector Variable
# prepare regular expression for that

regExpQuery       <- "([Cc]oal.*[Cc]omb)|([Cc]omb.*[Cc]oal)"      # Search for any [Coal Comb] or [Comb Coal] pairs
validItems        <- SCC$SCC[grep(regExpQuery,SCC$EI.Sector)]
subSetNEI         <- NEI[NEI$SCC %in% validItems, ]

# Using dplyr to exctract subset
Emissions <- 
      subSetNEI %>%      
      group_by(year) %>%
      summarise(TotalEmission = sum(Emissions)/1e6)

par(mfrow = c(1, 1)) # Reset any multiple plot

with(Emissions,
     barplot(TotalEmission,      
             main = expression("Total coal combustion emissions from PM"[2.5]*" in U:S:A" ), # set graph title
             names = year,                      # set Labels for bars
             xlab = "Years",                    # set label for x axis
             ylab = "Millions of Tons",         # set label for y axis  
             ylim = c(0, 0.8),
             col = rgb(0.5 , 0.5, 0.5))            
)
model <- lm(Emissions$TotalEmission ~ c(1:4))
abline(model, lwd = 2, col = "black")
# Save on png device
dev.copy(png, file="plot4.png", height=1024, width=1024)
dev.off()