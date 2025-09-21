library(ggplot2)
library(dplyr)
library(tidyr)

# Create dataset
game_data <- data.frame(
  Game = c("HOU", "ATL", "MIL", "IND", "PHI", "CHI", "CHA", "DAL"),
  Game_Number = 1:8,
  Organized_Offense_PPP = c(0.81, 0.87, 1.02, 1.07, 1.11, 1.26, 0.93, 0.85),
  Organized_Offense_ePPP = c(0.97, 0.98, 1.15, 1.05, 1.14, 1.11, 1.06, 1.07)
)

# Create plot with area under curves
p1 <- ggplot(game_data, aes(x = Game_Number)) +
  # Add area under ePPP line
  geom_area(aes(y = Organized_Offense_ePPP), 
            fill = "#2E5A87", alpha = 0.2, color = NA) +
  
  # Add area under PPP line  
  geom_area(aes(y = Organized_Offense_PPP), 
            fill = "#C41E3A", alpha = 0.3, color = NA) +
  
  # Add the lines on top
  geom_line(aes(y = Organized_Offense_ePPP), 
            color = "#2E5A87", size = 2.5, alpha = 0.9) +
  geom_line(aes(y = Organized_Offense_PPP), 
            color = "#C41E3A", size = 2.5, alpha = 0.9) +
  
  # Add points
  geom_point(aes(y = Organized_Offense_PPP), 
             color = "#C41E3A", size = 4, alpha = 0.95) +
  geom_point(aes(y = Organized_Offense_ePPP), 
             color = "#2E5A87", size = 4, alpha = 0.95) +
  
  # Y and X-axis styling
  scale_x_continuous(breaks = 1:8, labels = game_data$Game, expand = c(0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 1.35), breaks = seq(0, 1.3, 0.2), expand = c(0.01, 0.01)) +
  
  # Title and subtitle
  labs(title = "ORGANIZED OFFENSE: Game by Game Performance",
       subtitle = "Area under curves shows cumulative performance vs expectations",
       x = "", y = "PPP / ePPP") +
  
  # Styling
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FDF6E3", color = NA),
    panel.background = element_rect(fill = "#FDF6E3", color = NA),
    plot.title = element_text(family = "Arial Black", size = 14, face = "bold", 
                              hjust = 0.5, margin = margin(t = 15, b = 5), color = "#2C3E50"),
    plot.subtitle = element_text(family = "Arial", size = 10, 
                                 hjust = 0.5, margin = margin(b = 20), color = "#34495E"),
    axis.text.x = element_text(family = "Arial", size = 11, face = "bold", color = "#34495E", 
                               margin = margin(t = 8)),
    axis.text.y = element_text(family = "Arial", size = 10, color = "#34495E"),
    axis.title.y = element_text(family = "Arial", size = 11, face = "bold", color = "#34495E", 
                                angle = 90, margin = margin(r = 15)),
    panel.grid.major.y = element_line(color = "#E8DCC6", size = 0.3),
    panel.grid.major.x = element_line(color = "#E8DCC6", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 25, 15, 25),
    axis.line.x = element_line(color = "#BDC3C7", size = 0.5),
    axis.line.y = element_line(color = "#BDC3C7", size = 0.5)
  ) +
  
  # Add legend
  annotate("point", x = 1.3, y = 1.25, color = "#C41E3A", size = 3) +
  annotate("text", x = 1.6, y = 1.25, label = "PPP (Actual)", 
           family = "Arial", size = 3.5, color = "#2C3E50", hjust = 0, fontface = "bold") +
  annotate("point", x = 3.8, y = 1.25, color = "#2E5A87", size = 3) +
  annotate("text", x = 4.1, y = 1.25, label = "ePPP (Expected)", 
           family = "Arial", size = 3.5, color = "#2C3E50", hjust = 0, fontface = "bold")

# Display plot
print(p1)