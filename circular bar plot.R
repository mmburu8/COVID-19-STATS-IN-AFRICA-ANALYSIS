library(tidyverse)
hybrid <-  read_csv("hybrid.csv")
hybrid <- transform(hybrid, year = as.character(year))
hybrid$date <- paste(hybrid$mois, hybrid$year)
billie <- hybrid %>%
  filter(!date == "Jan 2020", !date == "Feb 2020", !COUNTRY_NAME == "Tanzania") %>%
  group_by(COUNTRY_NAME) %>%
  summarise(
    avginfect = mean(infected)
  )

billie$id <- seq(1, 53)
label_data <- billie
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle +180, angle)
p <- ggplot(billie, aes(x = as.factor(id), y = avginfect))+
  geom_bar(stat = "identity", fill= alpha("skyblue", 0.7))+
  ylim(-5000, 5000) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar(start = 0)+
  geom_text(data=label_data, aes(x=id, y=avginfect+10, label=COUNTRY_NAME, hjust=hjust),
            color="black", fontface="bold", alpha=0.6, size=2.5, angle=label_data$angle, 
            inherit.aes = FALSE)
p
