Emissions <- 
      SCC %>%
      filter(SCC.Level.One == "24510") %>%
      group_by(year,type) %>%
      summarise(TotalEmission = sum(Emissions))

str(Emissions)