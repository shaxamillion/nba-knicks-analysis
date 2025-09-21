library(tidyverse)
library(ggplot2)
library(ggimage)

# Create dataset
poa_data <- data.frame(
  Opponent = rep(c("HOU", "ATL", "MIL", "IND", "PHI", "CHI", "CHA", "DAL"), 5),
  Player = rep(c("Brunson", "Bridges", "OG", "Hart", "McBride"), each = 8),
  FREQ_Pct = c(
    # Brunson's data
    11.5, 6.5, 2.3, 1.6, 10.9, 16.4, 4.8, 23.7,
    # Bridges' data
    46.2, 43.5, 53.5, 53.2, 19.6, 43.3, 33.9, 39.5,
    # OG's data
    3.8, 8.1, 9.3, 1.6, 39.1, 6.0, 32.3, 2.6,
    # Hart's data
    15.4, 11.3, 2.3, 12.9, 2.2, 16.4, 16.1, 26.3,
    # McBride's data
    21.2, 29.0, 32.6, 30.6, 28.3, 17.9, 12.9, 7.9
  ),
  # Player headshot IDs
  NBA_ID = rep(c("1628973", "1628969", "1628384", "1628404", "1630540"), each = 8),
  stringsAsFactors = FALSE
)

# Opponent ordered by game sequence
poa_data$Opponent <- factor(
  poa_data$Opponent,
  levels = c("HOU", "ATL", "MIL", "IND", "PHI", "CHI", "CHA", "DAL")
)

# Add player colors
player_colors <- c(
  "Brunson" = "#FF6B35",    # Orange
  "Bridges" = "#004CFF",    # Blue  
  "OG" = "#00A86B",         # Green
  "Hart" = "#8B008B",       # Purple
  "McBride" = "#FF1744"     # Red
)

# Place headshot on last point
last_points <- poa_data %>%
  filter(Opponent == "DAL") %>%
  mutate(headshot_url = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", NBA_ID, ".png"))

# Create line plot
p <- ggplot(poa_data, aes(x = Opponent, y = FREQ_Pct, group = Player, color = Player)) +
  
  geom_line(size = 1.5, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  
  geom_image(
    data = last_points, 
    aes(x = "DAL", y = FREQ_Pct, image = headshot_url),
    size = 0.08, asp = 1.6, inherit.aes = FALSE,
    nudge_x = 0.3
  ) +
  
  scale_color_manual(values = player_colors) +
  
  # Y-axis:
  scale_y_continuous(
    name = "POA Defender Frequency %",
    breaks = seq(0, 60, 10),
    limits = c(0, 60),
    expand = c(0.02, 0.02),
    labels = function(x) paste0(x, "%")
  ) +
  
  # X-axis:
  scale_x_discrete(
    name = "Opponent Teams",
    expand = c(0.05, 0.15)
  ) +
  
  coord_cartesian(clip = "off") +
  
  #Styling
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#faf8f5", color = NA),
    panel.background = element_rect(fill = "#faf8f5", color = NA),
    panel.grid.major.y = element_line(color = "#e8e5e0", size = 0.4),
    panel.grid.minor.y = element_line(color = "#f0ede8", size = 0.2),
    panel.grid.major.x = element_line(color = "#e8e5e0", size = 0.3, alpha = 0.5),
    panel.grid.minor.x = element_blank(),
    text = element_text(family = "Arial", color = "#2c3e50"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, 
                              margin = margin(b = 10), color = "#2c3e50"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, 
                                 margin = margin(b = 25), color = "#7f8c8d"),
    plot.caption = element_text(size = 9, hjust = 0.5, 
                                margin = margin(t = 15), color = "#95a5a6"),
    axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
    axis.text = element_text(size = 10, color = "#2c3e50"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.line = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "#f5f2ed", color = "#e8e5e0"),
    legend.key = element_rect(fill = NA),
    legend.key.width = unit(1.5, "cm"),
    plot.margin = margin(25, 60, 25, 25)
  ) +
  
  # Title, subtitle, caption
  labs(
    title = "Game by Game POA Defender Usage",
    subtitle = "Hand-tracked data by @ShaxNBA",
    caption = "Across 8 games",
    color = "Player"
  )

# Display plot
print(p)

# Print data summary
cat("\nPOA Defender Usage Summary:\n")
for(player in unique(poa_data$Player)) {
  player_data <- poa_data[poa_data$Player == player, ]
  cat(player, "- Avg:", round(mean(player_data$FREQ_Pct), 1), "% | Range:", 
      round(min(player_data$FREQ_Pct), 1), "% -", round(max(player_data$FREQ_Pct), 1), "%\n")
}
