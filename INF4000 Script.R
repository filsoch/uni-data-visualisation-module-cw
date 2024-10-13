install.packages("ggpubr")
install.packages("tidyverse")

library(WDI)
library(ggpubr)
library(tidyverse)
options(scipen=999)
new_wdi_cache <-WDIcache()

#creating UK greenhouse gas emissions chart

greenhouse_gas <- WDI(country=c("GB"),
                      indicator =c("EN.ATM.GHGT.KT.CE","EN.ATM.NOXE.KT.CE",
                                   "EN.ATM.METH.KT.CE","EN.ATM.CO2E.KT"),
                      start = 2014, end = 2020,
                      extra=TRUE,
                      cache = new_wdi_cache)

greenhouse_gas <- mutate(greenhouse_gas, Other = EN.ATM.GHGT.KT.CE -
                           (EN.ATM.NOXE.KT.CE + EN.ATM.METH.KT.CE
                            + EN.ATM.CO2E.KT))

greenhouse_gas <- gather(greenhouse_gas, key = measure, value = Emissions, 
             c("Other","EN.ATM.NOXE.KT.CE",
               "EN.ATM.METH.KT.CE","EN.ATM.CO2E.KT"))

#creating a prediction for years past 2020

total_emissions <- greenhouse_gas %>%
  group_by(year) %>%
  summarise(total_emissions = sum(Emissions)) %>% filter(year < 2020)            #data for 2020 was removed from prediction model because its an outlier        


trendline_data <- data.frame(
  year = c(2021:2030)
)

lm_total_emissions <- lm(total_emissions ~ year, data = total_emissions)

trendline_data$total_emissions <- predict(lm_total_emissions, newdata = trendline_data)
         

UK_Emissions <- ggplot(greenhouse_gas, aes(year, Emissions), fill = measure) + 
  geom_bar(stat ="identity", aes(fill=measure))+
  geom_bar(stat = "identity", data = trendline_data, aes(x = year, y = total_emissions), fill = "lightgreen", alpha=.6) +
  geom_hline(yintercept = (749506*0.32), color = "red") +
  annotate("text", x = 2022, y = (749506*0.32), label = "Target for 2030 - 68% reduction of emissions comapred with 1990", vjust = -0.7, color = "red") +
  scale_x_continuous(breaks=seq(2014,2030,1),
                     labels = ~ ifelse(.x >= 2021, paste0(.x, "*"), .x)) +
  scale_fill_viridis_d(labels=c('CO2 Emmisions (kt)',
                                         'Methane Emissions\n(kt of Co2 Equivalent)',
                                         "Nitrous Oxide\nEmissions (kt of Co2 Equivalent)",
                                         "Other Emissions"), direction = -1) +
  labs(title = "UK Greenhouse gas emissions", x = "Year",
       y = "Kilotons of CO2 equivalent", fill = "Type of Greenhouse Gas",
       caption = "*predicted emissions, Data: World Bank") +
  theme_minimal() +
  theme(legend.spacing.y = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5, face ="bold")) +
  guides(fill = guide_legend(byrow = TRUE))




# CO2 emissions per capita in g7 countries

Co2_per_capita <- WDI(country=c("CA","JP","US","FR","IT","DE","EU", "GB"),
                      indicator =c("EN.ATM.CO2E.PC"),
                      start = 1990, end = 2020,
                      extra=TRUE,
                      cache = new_wdi_cache)


CO2_graph <- ggplot(Co2_per_capita, aes(x = year, y= EN.ATM.CO2E.PC, color = country)) +
  geom_line(size = 0.8) + 
  geom_point(size = 1.1, shape = 15) +
  scale_colour_viridis_d(breaks=c('Canada', 'United States', 'Japan', 'Germany',
                                                  'European Union', 'Italy', 'United Kingdom',
                                                  'France'), option = "turbo") +
  labs(title = "CO2 emissions per capita in G7 countries",
       x = "Year",
       y = "CO2 emissions (metric tons per capita)",
       color = "G7 Member",
       caption = "Data: World Bank") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face ="bold"))

#co2 consumption per capita in Europe, on map

europe.countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Republic of Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany","Greece", "Hungary", "Ireland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
  "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "UK",
  "Norway", "Serbia", "switzerland", "Kosovo", "Montenegro", "Bosnia and Herzegovina",
  "Albania", "North Macedonia", "Turkey", "Ukraine", "Moldova", "Belarus", "Iceland"
)

europe.map <- map_data("world", region = europe.countries)

CO2Emissons<-WDI(indicator=c("EN.ATM.CO2E.PC"),
                 start=2019,
                 end=2019,
                 extra= TRUE,
                 cache=new_wdi_cache)

CO2Emissons <- CO2Emissons %>%
  mutate(country=recode(str_trim(country),"United Kingdom"="UK",
                        "Czechia" = "Czech Republic",
                        "Slovak Republic" = "Slovakia", "Turkiye" = "Turkey"))

CO2EmissonsMap <- left_join(europe.map ,CO2Emissons,by = c("region"="country"))

CO2_map <- ggplot(CO2EmissonsMap , aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=EN.ATM.CO2E.PC), colour="black")+
  scale_fill_viridis_c(option = "turbo")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, face ="bold")) +
  labs(title = "Map of CO2 emissions per capita\nin Europe, year 2019",
  fill = "CO2 emissions\n(metric tons per capita)",
  caption = "Data: World Bank")+
  coord_map()

#% of energy production from renewables

Renewables <- WDI(country=c("CA","JP","US","FR","IT","DE","EU", "GB"),
                      indicator =c("EG.ELC.RNWX.ZS"),
                      start = 1990, end = 2015,
                      extra=TRUE,
                      cache = new_wdi_cache)


Renewables_graph <- ggplot(Renewables, aes(x = year, y= EG.ELC.RNWX.ZS, color = country)) +
  geom_line(size = 0.8) + 
  geom_point(size = 1.1, shape = 15) +
  scale_colour_viridis_d(breaks=c('Germany', 'United Kingdom', 'Italy', 'European Union',
                                                  'Japan', 'United States', 'Canada',
                                                  'France'), option = "turbo") +
  labs(title = "Electricity production from renewable sources in G7 countries,\n
       excluding hydroelectric (% of total)",
       x = "Year",
       y = "Electricity production from renewable sources (% of total)",
       color = "G7 Member",
       caption = "Data: World Bank") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face ="bold"))

#creating composite visualisation using ggarrange

composite_plot <- ggarrange(UK_Emissions, CO2_map, CO2_graph, Renewables_graph,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,
          heights = c(1, 1.2),
          widths = c(1, 1.2))

#saving all the plots

ggsave("UK_Emissions_Plot.png", plot = UK_Emissions, width = 9, height = 5, units = "in", bg = "white")
ggsave("CO2_graph.png", plot = CO2_graph , width = 8, height = 5, units = "in", bg = "white")
ggsave("CO2_map.png", plot = CO2_map, width = 5, height = 5, units = "in", bg = "white")
ggsave("Renewables_graph.png", plot = Renewables_graph, width = 8, height = 5, units = "in", bg = "white")
ggsave("composite_visualisation.png", plot = composite_plot, width = 20, height = 12, units = "in", bg = "white")