library(tidyverse)
library(ggplot2)
library(ggimage)

# Create dataset
pnr_data <- data.frame(
  Players = c("Brunson", "Bridges", "OG", "McBride", "Hart", "Payne"),
  Plays = c(26, 102, 33, 45, 28, 10),
  FREQ_pct = c(10.4, 40.8, 13.2, 18.0, 11.2, 4.0),
  PPA = c(0.82, 0.93, 0.94, 0.89, 1.15, 0.96),
  ePPA = c(0.93, 0.99, 0.85, 0.93, 1.05, 1.11),
  
  # Add NBA player IDs
  nba_player_id = c("1628973", "1628969", "1628384", "1630540", "1628404", "1626166"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    headshot_url = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", nba_player_id, ".png"),
    FREQ_decimal = FREQ_pct / 100  # Convert percentage to decimal for plotting
  )

# Create scatter plot
p <- ggplot(pnr_data, aes(x = PPA, y = FREQ_pct)) +
  
  # Add background color
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#faf8f5", color = NA),
    panel.background = element_rect(fill = "#faf8f5", color = NA),
    panel.grid.major = element_line(color = "#e8e5e0", size = 0.3),
    panel.grid.minor = element_line(color = "#f0ede8", size = 0.2),
    
    # Font
    text = element_text(family = "Arial", color = "#2c3e50"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, 
                              margin = margin(b = 20), color = "#2c3e50"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, 
                                 margin = margin(b = 30), color = "#7f8c8d"),
    axis.title = element_text(size = 12, face = "bold", color = "#34495e"),
    axis.text = element_text(size = 10, color = "#2c3e50"),
    
    # Remove axis lines
    axis.line = element_blank(),
    
    # Add subtle border
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Add player headshots as points
  geom_image(aes(image = headshot_url), size = 0.08, asp = 1.6) +
  
  # Customize axes
  scale_x_continuous(
    name = "Points Per Attempt (PPA)",
    breaks = seq(0.8, 1.2, 0.1),
    limits = c(0.75, 1.25),
    expand = c(0.02, 0.02)
  ) +
  
  scale_y_continuous(
    name = "Frequency %",
    breaks = seq(0, 45, 5),
    limits = c(0, 45),
    expand = c(0.02, 0.02),
    labels = function(x) paste0(x, "%")
  ) +
  
  # Add titles
  labs(
    title = "Pick & Roll POA Frequency vs Opponent Efficiency",
    subtitle = "In PnR Handler and Roll-Man playtypes"
  ) +
  
  # Add reference lines
  geom_hline(yintercept = mean(pnr_data$FREQ_pct), 
             color = "#bdc3c7", linetype = "dashed", alpha = 0.6, size = 0.5) +
  geom_vline(xintercept = mean(pnr_data$PPA), 
             color = "#bdc3c7", linetype = "dashed", alpha = 0.6, size = 0.5)

# Display plot
print(p)
