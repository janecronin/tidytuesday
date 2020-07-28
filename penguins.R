library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

str(penguins)

ggplot(penguins, aes(island, fill = species)) +
  geom_bar() +
  ggtitle("Where to Find Your Penguins") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(x ="Island", y = "Count", fill = "Species")
  
ggplot(penguins, aes(bill_length_mm, flipper_length_mm, color = species)) +
  geom_jitter(size = 3) +
  ggtitle("I like Big Flippers and I Cannot Lie") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) +
  scale_color_brewer(palette = "Blues", direction = -1, type = "qual") +
  labs(x = "Bill Length (mm)", y = "Flipper length (mm)", fill = "Species")

penguins_clean <- penguins %>% filter(sex != "NA")

ggplot(penguins_clean, aes(body_mass_g, sex)) +
  geom_boxplot(aes(fill = factor(species))) +
  ggtitle("The Mightiest Penguin") +
  theme_light() +
  theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Blues", direction = -1, type = "qual") +
  labs(x = "Weight (g)", y = "Sex", fill = "Species") +
  coord_flip()