library(ggplot2)
library(tidyr)

# Create the data frame
data <- data.frame(
  Game = 1:3,
  PPP = c(0.86, 1.17, 0.54),
  ePPP = c(1.01, 1.07, 0.99),
  Shot.Making = c(-0.15, 0.09, -0.45)
)

# Reshape data to long format for plotting
data_long <- pivot_longer(data, 
                          cols = c(PPP, ePPP, Shot.Making),
                          names_to = "Metric",
                          values_to = "Value")

# Rename Shot.Making to "Shot Making" for display
data_long$Metric[data_long$Metric == "Shot.Making"] <- "Shot Making"

# Create a more dynamic and visually appealing plot
ggplot(data_long, aes(x = Game, y = Value, color = Metric, fill = Metric)) +
  # Add shaded area under lines
  geom_area(alpha = 0.2, position = "identity") +
  # Add smooth curves instead of straight lines
  geom_smooth(method = "loess", se = FALSE, size = 2, span = 1.5) +
  # Add large points
  geom_point(size = 6, alpha = 0.8) +
  geom_point(size = 3, color = "white") +
  # Add reference line at y = 0 for Shot Making
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
  # Custom color palette with vibrant colors
  scale_color_manual(values = c("PPP" = "#E63946", 
                                "ePPP" = "#06FFA5", 
                                "Shot Making" = "#FFD60A")) +
  scale_fill_manual(values = c("PPP" = "#E63946", 
                               "ePPP" = "#06FFA5", 
                               "Shot Making" = "#FFD60A")) +
  scale_x_continuous(breaks = 1:3, labels = paste("Game", 1:3)) +
  # Labels and title with enhanced formatting
  labs(title = "Efficiency Metrics",
       subtitle = "Across Three Games",
       x = NULL,
       y = "Value") +
  # Dark theme with custom styling
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1a1a2e", color = NA),
    panel.background = element_rect(fill = "#16213e", color = NA),
    panel.grid.major = element_line(color = "#2c3e50", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold", 
                              color = "white", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, 
                                 color = "#b8b8b8", margin = margin(b = 20)),
    axis.text = element_text(color = "white", size = 12, face = "bold"),
    axis.title = element_text(color = "white", size = 14, face = "bold"),
    legend.position = "top",
    legend.background = element_rect(fill = "#16213e", color = "#2c3e50"),
    legend.text = element_text(color = "white", size = 12, face = "bold"),
    legend.title = element_text(color = "white", size = 13, face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )