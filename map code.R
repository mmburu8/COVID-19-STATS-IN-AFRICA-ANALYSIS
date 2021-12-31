library(tidyverse)
install.packages("sf")
library(sf)
install.packages("rnaturalearthdata")
install.packages("viridisLite")
library(viridisLite)
library(rnaturalearth)
library(rnaturalearthdata)
? scale_fill_viridis_c
install.packages("worldmap")
rld <-  ne_countries(scale = 'medium', type = 'map_units', returnclass = "sf")
head(rld[c("name", "continent")])
afrique <- rld[rld$continent == "Africa",]
ggplot() + geom_sf(data = afrique) + theme_bw()
hybrid <- read_csv("hybrid.csv")
afriking <- hybrid %>%
  filter(!COUNTRY_NAME == "Tanzania", year == 2021, mois == "Aug", day == 20) %>%
  mutate(ISO = fct_recode(ISO, "CAF" = "CAR")) %>%
  select(ISO, COUNTRY_NAME, population, cum_infected)
afrTogether <- afriking %>%
  left_join(afrique, c("ISO" = "iso_a3")) %>%
  select(ISO, COUNTRY_NAME, population, cum_infected, geometry)
ggplot(afrTogether)+
  geom_sf(aes(geometry = geometry, fill = cum_infected)) +

  labs(
    title = "CUMULATIVE NUMBER OF PEOPLE INFECTED WITH COVID-19 IN AFRICAN COUNTRIES")+
  scale_fill_viridis_c(option = "H")
getwd()
