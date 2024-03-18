# Catch up with simple linear regression
# Continuous variable predicted by another cont variable
# Continuous variable predicted by a categorical variable
# Continuous variable predicted by a continuous and categorical with interaction

# Keywords

# author @ Rahul Venugopal on 17th March 2024

# Load libraries
library(ggplot2)
library(palmerpenguins)
library(dplyr)
library(tidyr)
library(ggpubr)

# Load data
data <- penguins %>%
  select(species,flipper_length_mm,body_mass_g,sex) %>% 
  drop_na()

# Summary of the data set
str(data)

# Does species predict body mass?
# Let us start with visualization
data$species <- as.factor(data$species)

# Body mass predicted by species ------------------------------------------

p1 <- ggplot(data,
             aes(x = species, y = body_mass_g, color = species)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  geom_jitter(width = 0.1) +
  labs(x = "Species", y = "Body mass") + 
  # Set up the theme
  theme(axis.ticks = element_line(color = "grey92"),
        axis.ticks.length = unit(1, "lines"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_blank(),
        legend.text = element_text(color = "grey30", size = 18),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey20", size = 10),
        plot.title = element_text(size = 30, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "grey30"),
        plot.caption = element_text(size = 9, margin = margin(t = 15)),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = margin(t = 25, r = 25, b = 10, l = 25)) 

bodymass_species <- lm(body_mass_g ~ species,
                       data = data)
summary(bodymass_species)

# Body mass predicted by flipper length -----------------------------------
bodymass_flipper <- lm(body_mass_g ~ flipper_length_mm,
                       data = data)
summary(bodymass_flipper)

# Body mass predicted by flipper length and species -----------------------
bodymass_flipper_species <- lm(body_mass_g ~ flipper_length_mm * species,
                       data = data)
summary(bodymass_flipper_species)

p2 <- ggplot(data,
             aes(x = species, y = flipper_length_mm, color = species)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  geom_jitter(width = 0.1) +
  labs(x = "Species", y = "Flipper length")

# Viz
(full_model <- ggplot(data, aes(x = flipper_length_mm, y = body_mass_g)) +
    geom_point(aes(colour = species)) +                                # scatter plot, coloured by sex
    labs(x = "Flipper length", y = "Body mass") +
    stat_smooth(method = "lm", aes(fill = species, colour = species)) +    # adding regression lines for each sex
    scale_colour_manual(values = c("#add8e6", "#98fb98", "#ffc0cb")) +
    scale_fill_manual(values = c("#add8e6", "#98fb98", "#ffc0cb")) +
    theme_pubr())