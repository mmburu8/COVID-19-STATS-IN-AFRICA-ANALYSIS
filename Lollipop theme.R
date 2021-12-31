library(tidyverse)
library(datasets)
library(ggthemes)
# CLEANING AND PREPROCESSING.
hybrid <- read_csv("hybrid.csv")
arabPays <- c("Egypt", "Libya", "Morocco", "Algeria", "Sudan", "Somalia",
              "Djibouti", "Mauritania", "Comoros", "Eritrea", "Chad", "Tunisia")
dhanApril20 <- hybrid %>%
  filter(mois=="Apr", day>=24, year==2020)
dhanMay20 <- hybrid %>%
  filter(mois=="May", day<23, year==2020)
dhanApr21 <- hybrid %>%
  filter(mois=="Apr", day>=13, year==2021)
dhanMay21 <- hybrid %>%
  filter(mois=="May",day<12, year==2021)
rama20 <- as.data.frame(rbind(dhanApril20, dhanMay20))
rama21 <- as.data.frame(rbind(dhanApr21, dhanMay21))
ramadhan20 <- rama20%>%
  group_by(COUNTRY_NAME)%>%
  summarise(totInfected = sum(infected))%>%
  mutate(arabNchi = case_when(
    COUNTRY_NAME%in%arabPays ~ "Arab",
    !COUNTRY_NAME%in%arabPays ~ "Non Arab"
  ))
ramadhan21 <-  rama21 %>%
  group_by(COUNTRY_NAME)%>%
  summarise(totInfected =sum(infected))%>%
  mutate(arabNchi=case_when(
    COUNTRY_NAME%in%arabPays ~ "Arab",
    !COUNTRY_NAME%in%arabPays ~ "Non Arab"
  ))
 
# VISUALIZATION
ggplot(ramadhan21, aes(COUNTRY_NAME, totInfected, color=arabNchi))+
  geom_segment(aes(COUNTRY_NAME, xend=COUNTRY_NAME, yend= totInfected, y= 0),
               size= 1)+
  geom_point(size = 5)+
  scale_colour_manual(values = c("deeppink3","limegreen"))+
  coord_flip()+
  theme(
    plot.background=element_rect(fill="paleturquoise1", color="paleturquoise1"),
    panel.background = element_rect(fill="paleturquoise1", color="paleturquoise1"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    legend.background =element_rect(fill="paleturquoise1",color="paleturquoise1"),
    legend.key=element_rect(fill="paleturquoise1")
  )+
  labs(
    title= "NUMBER OF COVID-19 INFECTIONS RECORDED DURING RAMADHAN IN 2021" ,
    y = "Number of Infections",
    x = "COUNTRIES",
    col = "Arab Countries"
  )
?geom_segment
