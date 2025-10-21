library(ggplot2)
library(dplyr)
library(tidyr)

# Create the dataset
offense_data <- data.frame(
  Game = c("Game 1", "Game 2", "Game 3", "Game 4", "Game 5"),
  Freelance = c(1.01, 1.05, 1.03, 0.97, 0.79),
  Organized_Offense = c(0.95, 1.12, 0.95, 1.06, 1.06)
)

# Manual averages as provided
avg_freelance <- 0.93
avg_organized <- 1.03

# Reshape data for plotting
offense_long <- offense_data %>%
  pivot_longer(cols = c(Freelance, Organized_Offense),
               names_to = "Attack_Type",
               values_to = "ePPP") %>%
  mutate(Attack_Type = ifelse(Attack_Type == "Organized_Offense", "Organized Offense", Attack_Type))

# Create the plot
ggplot(offense_long, aes(x = Game, y = ePPP, color = Attack_Type, group = Attack_Type)) +
  # Add horizontal lines for averages (subtle and dotted)
  geom_hline(yintercept = avg_freelance, linetype = "dotted", color = "#FF6B6B", size = 0.8, alpha = 0.4) +
  geom_hline(yintercept = avg_organized, linetype = "dotted", color = "#4ECDC4", size = 0.8, alpha = 0.4) +
  # Add lines connecting points
  geom_line(size = 2.5, alpha = 0.9) +
  # Add points
  geom_point(size = 6, alpha = 1) +
  # Add value labels on points
  geom_text(aes(label = sprintf("%.2f", ePPP)), 
            vjust = -1.5, size = 4, fontface = "bold", 
            family = "sans", show.legend = FALSE) +
  # Color scheme
  scale_color_manual(values = c("Freelance" = "#FF6B6B", "Organized Offense" = "#4ECDC4")) +
  # Labels and title
  labs(
    title = "Freelance vs Organized Offense",
    subtitle = sprintf("Avg ePPP — Freelance: %.2f  |  Organized: %.2f", 
                       avg_freelance, avg_organized),
    x = "",
    y = "ePPP",
    color = ""
  ) +
  # Y-axis limits and breaks
  scale_y_continuous(limits = c(0.7, 1.2), breaks = seq(0.7, 1.2, 0.1)) +
  # Theme customization - clean and modern
  theme_minimal(base_size = 30, base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold", 
                              color = "#1a1a1a", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#666666", 
                                 margin = margin(b = 5)),
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.grid.major.y = element_line(color = "#E8E8E8", size = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 13, face = "bold"),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_blank(),
    legend.key.size = unit(1.2, "lines"),
    legend.margin = margin(b = 2),
    axis.text = element_text(size = 12, color = "#404040"),
    axis.title.y = element_text(size = 13, face = "bold", color = "#404040", 
                                margin = margin(r = 12)),
    axis.text.x = element_text(size = 12, face = "bold", color = "#404040"),
    axis.ticks = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )