library(tidyverse)
library(ggplot2)
library(ggimage)

# Create dataset
drop_data <- data.frame(
  Team = c("HOU", "ATL", "MIL", "IND", "PHI", "CHI", "CHA", "DAL"),
  Poss = c(52, 62, 43, 62, 53, 71, 66, 42),
  Drop_Freq_Pct = c(61.5, 51.5, 14.3, 27.9, 46.7, 28.6, 72.5, 63.2),
  Level_Freq_Pct = c(34.6, 36.4, 61.9, 18.6, 10.0, 24.5, 10.0, 10.5),
  Soft_Hedge_Freq_Pct = c(3.8, 3.0, 4.8, 7.0, 6.7, 8.2, 5.0, 21.1),
  Hard_Hedge_Freq_Pct = c(0.0, 0.0, 0.0, 4.7, 3.3, 8.2, 2.5, 0.0),
  Blitz_Freq_Pct = c(0.0, 3.0, 0.0, 9.3, 0.0, 6.1, 7.5, 5.3),
  Switch_Freq_Pct = c(0.0, 0.0, 4.8, 14.0, 0.0, 6.1, 2.5, 0.0),
  Peel_Next_Freq_Pct = c(0.0, 6.1, 4.8, 0.0, 0.0, 0.0, 0.0, 0.0),
  None_Freq_Pct = c(0.0, 0.0, 9.5, 18.6, 33.3, 18.4, 0.0, 0.0),
  Attack_Pct = c(50.0, 53.2, 48.8, 69.4, 56.6, 69.0, 60.6, 45.2),
  Attacks_75 = c(37.5, 39.9, 36.6, 52.0, 42.5, 51.8, 45.5, 33.9),
  Drop_PPA = c(0.85, 0.76, 0.67, 1.82, 0.91, 0.15, 0.95, 1.21),
  stringsAsFactors = FALSE
) %>%
  # Add team colors
  mutate(
    Team_Color = case_when(
      Team == "HOU" ~ "#CE1141",
      Team == "ATL" ~ "#E03A3E", 
      Team == "MIL" ~ "#00471B",
      Team == "IND" ~ "#002D62",
      Team == "PHI" ~ "#006BB6",
      Team == "CHI" ~ "#CE1141",
      Team == "CHA" ~ "#1D1160",
      Team == "DAL" ~ "#00538C",
      TRUE ~ "#2c3e50"
    )
  )

# Add KAT's player headshot
kat_data <- data.frame(
  x = 9.2,  # Position to the right of the teams
  y = 1.5,  # Middle-upper position
  headshot_url = "https://cdn.nba.com/headshots/nba/latest/260x190/1626157.png"  # Replace with KAT's actual ID
)

# Create bubble plot
p <- ggplot(drop_data, aes(x = Team, y = Drop_PPA)) +
  
  # Add bubbles with size based on Drop frequency
  geom_point(aes(size = Drop_Freq_Pct, fill = Team_Color), 
             shape = 21, color = "#2c3e50", stroke = 1.5, alpha = 0.8) +
  
  # Add KAT's headshot on the side
  geom_image(data = kat_data, aes(x = x, y = y, image = headshot_url), 
             size = 0.15, asp = 1.6, inherit.aes = FALSE) +
  
  # Add label for KAT
  annotate("text", x = 9.2, y = 1.15, label = "Karl-Anthony\nTowns", 
           hjust = 0.5, vjust = 1, size = 3.5, fontface = "bold", 
           color = "#2c3e50", family = "Arial") +
  
  # Add border around KAT's section
  annotate("rect", xmin = 8.7, xmax = 9.7, ymin = 1.05, ymax = 1.75, 
           fill = NA, color = "#bdc3c7", linetype = "dashed", alpha = 0.6) +
  
  # Custom size for bubbles
  scale_size_continuous(
    name = "Drop Frequency %",
    range = c(3, 20),  # Min and max bubble sizes
    breaks = c(20, 40, 60, 80),
    labels = c("20%", "40%", "60%", "80%"),
    guide = guide_legend(override.aes = list(stroke = 1, alpha = 0.8))
  ) +
  
  # Use team colors
  scale_fill_identity() +
  
  # Customize Y-axis
  scale_y_continuous(
    name = "Drop Coverage PPA",
    breaks = seq(0, 2.0, 0.25),
    limits = c(0, 2.0),
    expand = c(0.02, 0.02)
  ) +
  
  # Customize X-axis
  scale_x_discrete(
    name = "Teams",
    expand = c(0.1, 0.3)  # Extra space on the right for KAT
  ) +
  
  # Extend plot to show KAT's section
  coord_cartesian(xlim = c(0.5, 10), clip = "off") +
  
  # Background styling
  theme_minimal() +
  theme(
    # Background colors
    plot.background = element_rect(fill = "#faf8f5", color = NA),
    panel.background = element_rect(fill = "#faf8f5", color = NA),
    panel.grid.major.y = element_line(color = "#e8e5e0", size = 0.4),
    panel.grid.minor.y = element_line(color = "#f0ede8", size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # Fonts
    text = element_text(family = "Arial", color = "#2c3e50"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, 
                              margin = margin(b = 10), color = "#2c3e50"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, 
                                 margin = margin(b = 25), color = "#7f8c8d"),
    plot.caption = element_text(size = 9, hjust = 0.5, 
                                margin = margin(t = 15), color = "#95a5a6"),
    
    # Axis styling
    axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
    axis.text = element_text(size = 10, color = "#2c3e50"),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.line = element_blank(),
    
    # Legend styling
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "#f5f2ed", color = "#e8e5e0"),
    legend.key = element_rect(fill = NA),
    
    # Plot margins
    plot.margin = margin(25, 25, 25, 25)
  ) +
  
  # Add titles and caption
  labs(
    title = "KAT Drop Coverage by Team",
    subtitle = "Bubble size represents Drop coverage frequency",
    caption = "Data shows teams' Drop coverage PPA with frequency as bubble size\nLarger bubbles = higher Drop coverage frequency"
  )

# Display plot
print(p)

# Print summary statistics
cat("\nDrop Coverage Summary:\n")
cat("Highest PPA:", drop_data$Team[which.max(drop_data$Drop_PPA)], "- PPA:", max(drop_data$Drop_PPA), "\n")
cat("Lowest PPA:", drop_data$Team[which.min(drop_data$Drop_PPA)], "- PPA:", min(drop_data$Drop_PPA), "\n")
cat("Highest Frequency:", drop_data$Team[which.max(drop_data$Drop_Freq_Pct)], "- Freq:", max(drop_data$Drop_Freq_Pct), "%\n")
cat("Lowest Frequency:", drop_data$Team[which.min(drop_data$Drop_Freq_Pct)], "- Freq:", min(drop_data$Drop_Freq_Pct), "%\n")
