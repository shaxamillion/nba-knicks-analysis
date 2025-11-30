library(ggplot2)
library(dplyr)

# Create dataset from the screenshot data
three_data <- data.frame(
  Game = c("CLE", "BOS", "MIA", "MIL", "CHI1", "CHI2", "WSH", "MIN"),
  Game_Number = 1:8,
  Expected_PPS = c(1.05, 1.15, 1.19, 1.15, 1.08, 1.19, 1.17, 1.10),
  Shot_Frequency = c(51, 55, 58, 47, 49, 49, 45, 46)
)

# Create scatter plot
ggplot(three_data, aes(x = Shot_Frequency, y = Expected_PPS)) +
  # Add reference lines for average
  geom_hline(yintercept = mean(three_data$Expected_PPS), 
             linetype = "dashed", color = "#CCCCCC", linewidth = 0.8, alpha = 0.6) +
  geom_vline(xintercept = mean(three_data$Shot_Frequency), 
             linetype = "dashed", color = "#CCCCCC", linewidth = 0.8, alpha = 0.6) +
  
  # Add points with gradient color based on Expected PPS
  geom_point(aes(fill = Expected_PPS), 
             size = 12, shape = 21, color = "#2C3E50", stroke = 2, alpha = 0.9) +
  
  # Add game labels inside points
  geom_text(aes(label = Game), 
            size = 4, fontface = "bold", color = "#FFFFFF") +
  
  # Color gradient (green = high efficiency, yellow/red = lower)
  scale_fill_gradient2(
    low = "#FFD93D",
    mid = "#4ECDC4", 
    high = "#26de81",
    midpoint = 1.10,
    name = "Expected PPS"
  ) +
  
  # Labels
  labs(
    title = "Three-Point Shot Quality vs Volume",
    subtitle = "Expected Points Per Shot by Shot Frequency • Games 1-8",
    x = "Shot Frequency (%)",
    y = "Expected PPS"
  ) +
  
  # Axis formatting
  scale_x_continuous(
    breaks = seq(45, 70, 5),
    limits = c(44, 68)
  ) +
  scale_y_continuous(
    breaks = seq(0.95, 1.25, 0.05),
    limits = c(0.95, 1.22)
  ) +
  
  # Add quadrant labels
  annotate("text", x = 46, y = 1.20, label = "High Shot Quality\nLow Volume", 
           size = 3.5, color = "#888888", fontface = "italic", hjust = 0) +
  annotate("text", x = 66, y = 1.20, label = "High Shot Quality\nHigh Volume", 
           size = 3.5, color = "#888888", fontface = "italic", hjust = 1) +
  annotate("text", x = 46, y = 0.97, label = "Low Shot Quality\nLow Volume", 
           size = 3.5, color = "#888888", fontface = "italic", hjust = 0) +
  annotate("text", x = 66, y = 0.97, label = "Low Shot Quality\nHigh Volume", 
           size = 3.5, color = "#888888", fontface = "italic", hjust = 1) +
  
  # Theme
  theme_minimal(base_size = 15, base_family = "sans") +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      size = 22, 
      face = "bold", 
      color = "#1a1a1a",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, 
      size = 12, 
      color = "#666666", 
      margin = margin(b = 25)
    ),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 12, color = "#404040", face = "bold"),
    axis.title = element_text(size = 13, face = "bold", color = "#404040"),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.ticks = element_blank(),
    plot.margin = margin(25, 25, 25, 25)
  )

# Display the plot
print(last_plot())