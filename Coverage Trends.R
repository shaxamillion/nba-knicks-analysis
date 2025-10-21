library(ggplot2)
library(dplyr)
library(tidyr)

# Create dataset (without Peel/Next)
coverage_data <- data.frame(
  Game = c("Game 1", "Game 2", "Game 3", "Game 4", "Game 5"),
  Drop = c(26, 3, 31, 27, 34),
  Level = c(39, 15, 24, 41, 17),
  `Soft Hedge` = c(10, 15, 14, 5, 7),
  `Hard Hedge` = c(0, 0, 0, 0, 4),
  Blitz = c(0, 0, 3, 5, 1),
  Switch = c(23, 62, 17, 18, 18),
  None = c(3, 3, 7, 5, 18),
  check.names = FALSE
)

# Convert to long format
coverage_long <- coverage_data %>%
  pivot_longer(cols = -Game, 
               names_to = "Coverage_Type", 
               values_to = "Usage") %>%
  mutate(Coverage_Type = factor(Coverage_Type, 
                                levels = c("Drop", "Level", "Switch", "Soft Hedge", "None", "Blitz", "Hard Hedge")))

# Create line chart
ggplot(coverage_long, aes(x = Game, y = Usage, 
                          color = Coverage_Type, 
                          group = Coverage_Type)) +
  geom_line(linewidth = 2.5, alpha = 0.85) +
  geom_point(size = 5, alpha = 0.95) +
  geom_text(data = subset(coverage_long, Usage > 0),
            aes(label = paste0(Usage, "%")), 
            vjust = -1.3, size = 3.8, fontface = "bold", 
            show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Drop" = "#FF6B6B",
      "Level" = "#4ECDC4", 
      "Switch" = "#FFD93D",
      "Soft Hedge" = "#A8E6CF",
      "None" = "#C7CEEA",
      "Blitz" = "#FFB6B9",
      "Hard Hedge" = "#E63946"
    ),
    breaks = c("Drop", "Level", "Switch", "Soft Hedge", "None", "Blitz", "Hard Hedge")
  ) +
  labs(
    title = "Coverage Trends",
    subtitle = "Preseason Games 1-5",
    x = NULL,
    y = "Usage",
    color = NULL
  ) +
  scale_y_continuous(
    limits = c(-2, 70), 
    breaks = seq(0, 70, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal(base_size = 15, base_family = "sans") +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      size = 24, 
      face = "bold", 
      color = "#1a1a1a",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, 
      size = 13, 
      color = "#666666", 
      margin = margin(b = 25)
    ),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "#F0F0F0", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    legend.key.width = unit(1.5, "cm"),
    legend.spacing.x = unit(0.3, "cm"),
    axis.text.x = element_text(size = 13, color = "#404040", face = "bold"),
    axis.text.y = element_text(size = 12, color = "#404040"),
    axis.title.y = element_text(size = 14, face = "bold", color = "#404040", margin = margin(r = 10)),
    axis.ticks = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )