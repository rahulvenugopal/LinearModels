# Motivation for mixed effects models
# Groupings that affects data

# author @ Rahul Venugopal on 17th March 2024

# Load libraries
library(ggplot2)
library(palmerpenguins)
library(dplyr)
library(tidyr)
library(ggpubr)

# Load data
data <- penguins %>%
  select(species,flipper_length_mm,body_mass_g,sex, island) %>% 
  drop_na()

# Summary of the data set
str(data)

data$species <- as.factor(data$species)
data$island <- as.factor(data$island)

# Set up the theme
theme_update(
  axis.ticks = element_line(color = "grey92"),
  axis.ticks.length = unit(1, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 20),
  legend.text = element_text(color = "grey30"),
  axis.text.x = element_text(color = "grey20", size = 20),
  axis.text.y = element_text(color = "grey20", size = 20),
  plot.title = element_text(size = 18, face = "bold"),
  plot.subtitle = element_text(size = 12, color = "grey30"),
  plot.caption = element_text(size = 9, margin = margin(t = 15))
) + 
  theme(plot.margin = margin(t = 25, r = 25, b = 10, l = 25)) 

# Being in one island will influence your body weight!
# From a statistics angle, this has to be accounted when we want to understand
# the influence of flipper length and species
ggplot(data,
       aes(x = island, y = body_mass_g, color = island)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  geom_jitter(width = 0.1) +
  labs(x = "Island", y = "Body mass")