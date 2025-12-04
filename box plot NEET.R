library(tidyverse)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/IDS Group Assignment/data sets")
continents <- read.csv("./continents-according-to-our-world-in-data.csv")
gdp <- read.csv("./gdp-per-capita-worldbank.csv")
youth <- read.csv("./youth-not-in-education-employment-training.csv")

#rename NEET column
youth_neet <- youth %>%
  select(
    Entity,
    Code,
    Year,
    neet_pct = "Share.of.youth.not.in.education..employment.or.training..total....of.youth.population."   # <- replace this
  )

continents_clean <- continents %>%
  select(Entity, Code, Continent)

# Join NEET with continent, remove Antarctica
neet_with_cont <- youth_neet %>%
  left_join(continents_clean, by = c("Entity", "Code")) %>%
  filter(!is.na(Continent), Continent != "Antarctica")

# Filter to SDG period (2015–2020)
neet_2015_2020 <- neet_with_cont %>%
  filter(Year >= 2015, Year <= 2020, !is.na(neet_pct))

# Optional: set a nice continent order
neet_2015_2020$Continent <- factor(
  neet_2015_2020$Continent,
  levels = c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")
)

# Boxplot: within-continent NEET variation (2015–2020 pooled)
p_neet_box <- ggplot(neet_2015_2020,
                     aes(x = Continent, y = neet_pct, fill = Continent)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  labs(
    title = "Within-Continent Variation in Youth NEET (2015–2020)",
    subtitle = "Distribution of NEET (% of youth 15–24) by continent",
    x = "Continent",
    y = "NEET (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11)
  )

p_neet_box_points <- ggplot(neet_2015_2020,
       aes(x = Continent,
           y = neet_pct,
           fill = Continent,
           color = Continent)) +
  
  # Boxplot (median, IQR, whiskers)
  geom_boxplot(alpha = 0.45, outlier.shape = NA) +
  
  # Points (jitter), using same continent colors
  geom_jitter(width = 0.2, size = 1.8, alpha = 0.8) +
  
  labs(
    title = "Within-Continent Variation in Youth NEET (2015–2020)",
    x = "Continent",
    y = "NEET (%)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

print(p_neet_box_points)

ggsave("neet_within_continent_boxplot_2015_2020.png",
       p_neet_box_points, width = 11, height = 6, dpi = 300)
