# Lineup Usage Chart with Player Names + Bar Chart

# libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)

# Lineup data with player names split
lineup_data <- data.frame(
  lineup_id = 1:7,
  lineup_name = c(
    "Starting Unit",
    "Sub Unit 1",
    "Sub Unit 2", 
    "2nd Unit 1",
    "2nd Unit 2",
    "2nd Unit 3",
    "2nd Unit 4"
  ),
  minutes = c(8, 3, 3, 3, 4, 1, 2),
  unit_type = c(
    "Starting Unit",
    "Sub Unit",
    "Sub Unit", 
    "2nd Unit",
    "2nd Unit",
    "2nd Unit",
    "2nd Unit"
  ),
  # Player combinations (The lineups are created vertically)
  
  player1 = c("Brunson", "Brunson", "Brunson", "Clarkson", "Clarkson", "Clarkson", "Brunson"),
  player2 = c("McBride", "McBride", "Clarkson", "Hart", "Bridges", "McBride", "McBride"),
  player3 = c("Bridges", "Hart", "Hart", "Bridges", "OG", "Hart", "Hart"),
  player4 = c("OG", "OG", "Yabusele", "KAT", "KAT", "Yabusele", "Yabusele"),
  player5 = c("KAT", "Robinson", "Robinson", "Yabusele", "Robinson", "Robinson", "Robinson")
)

# Convert to long format

lineup_long <- lineup_data %>%
  pivot_longer(
    cols = starts_with("player"), 
    names_to = "position", 
    values_to = "player"
  ) %>%
  mutate(
    position_num = as.numeric(gsub("player", "", position))
  )

# Create main chart showing all lineups with player names
lineup_viz <- ggplot(lineup_long, aes(x = position_num, y = factor(lineup_id, levels = 6:1))) +
  
  # Background tiles on each lineup
  geom_tile(aes(fill = unit_type), 
            width = 0.9, height = 0.8, 
            alpha = 0.3, color = "white", size = 1) +
  
  # Player names on tiles
  geom_text(aes(label = player), 
            color = "#2F4F4F", 
            size = 3.5, 
            fontface = "bold") +
  
  scale_fill_manual(
    values = c(
      "Starting Unit" = "#2E8B57",   # Dark sea green
      "Sub Unit" = "#9ACD32",        # Yellow green  
      "2nd Unit" = "#DAA520"         # Golden rod
    ),
    name = "Unit Type"
  ) +
  
  scale_x_continuous(
    breaks = 1:5,
    labels = c("PG", "SG", "SF", "PF", "C"),
    expand = c(0.1, 0.1)
  ) +
  
  scale_y_discrete(
    labels = paste0(lineup_data$lineup_name[6:1], " - ", lineup_data$minutes[6:1], " min")
  ) +
  
  labs(
    title = "LINEUP COMBINATIONS",
    subtitle = "Player rotations by unit type and minutes played",
    x = "Positions",
    y = "Lineups"
  ) +
  
  theme_minimal() +
  theme(
    # Cream background
    
    plot.background = element_rect(fill = "#FDFCF7", color = NA),
    panel.background = element_rect(fill = "#FDFCF7", color = NA),
    panel.grid = element_blank(),
    
    # Title styling
    plot.title = element_text(
      size = 24, 
      face = "bold", 
      hjust = 0.5, 
      color = "#2F4F4F",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0.5, 
      color = "#696969",
      margin = margin(b = 20)
    ),
    
    # Y-axis (lineup names)
    axis.text.y = element_text(
      size = 11, 
      face = "bold", 
      color = "#2F4F4F",
      hjust = 1,
      margin = margin(r = 15)
    ),
    axis.title.y = element_text(
      size = 12,
      face = "bold",
      color = "#2F4F4F",
      margin = margin(r = 20)
    ),
    
    # X-axis styling
    axis.text.x = element_text(
      size = 12, 
      color = "#2F4F4F",
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 12,
      face = "bold",
      color = "#2F4F4F",
      margin = margin(t = 15)
    ),
    
    # Legend styling
    legend.position = "top",
    legend.title = element_text(
      size = 12,
      face = "bold",
      color = "#2F4F4F"
    ),
    legend.text = element_text(
      size = 11,
      color = "#2F4F4F"
    ),
    legend.background = element_rect(fill = "#FDFCF7", color = NA),
    legend.margin = margin(b = 15),
    
    # Margins
    plot.margin = margin(30, 30, 30, 60)
  )

print(lineup_viz)

# Bar chart with lineup details

lineup_bars <- lineup_data %>%
  mutate(
    full_lineup = paste(player1, player2, player3, player4, player5, sep = " - "),
    display_name = paste0(lineup_name, "\n", full_lineup)
  )

bar_chart <- ggplot(lineup_bars, aes(x = minutes, y = fct_reorder(display_name, minutes), fill = unit_type)) +
  geom_col(width = 0.5, alpha = 0.9, color = "white", size = 1.5) +
  
  # Add minute labels
  geom_text(aes(label = paste0(minutes, " min")), 
            hjust = -0.1, 
            color = "#2F4F4F", 
            size = 4.5, 
            fontface = "bold") +
  
  scale_fill_manual(
    values = c(
      "Starting Unit" = "#2E8B57",
      "Sub Unit" = "#9ACD32",       
      "2nd Unit" = "#DAA520"        
    ),
    name = "Unit Type"
  ) +
  
  scale_x_continuous(
    limits = c(0, 12),
    breaks = c(0, 3, 6, 9, 12),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "LINEUP USAGE BREAKDOWN",
    subtitle = "Minutes played by each 5-man combination",
    x = "Minutes Played",
    y = "Lineups"
  ) +
  
  theme_minimal() +
  theme(
    # Same cream styling
    plot.background = element_rect(fill = "#FDFCF7", color = NA),
    panel.background = element_rect(fill = "#FDFCF7", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#F0E68C", size = 0.3, linetype = "dashed"),
    
    plot.title = element_text(
      size = 24, 
      face = "bold", 
      hjust = 0.5, 
      color = "#2F4F4F",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0.5, 
      color = "#696969",
      margin = margin(b = 20)
    ),
    
    axis.text.y = element_text(
      size = 10, 
      face = "bold", 
      color = "#2F4F4F",
      hjust = 1,
      lineheight = 1.3,
      margin = margin(r = 20)
    ),
    axis.title.y = element_text(
      size = 12,
      face = "bold",
      color = "#2F4F4F",
      margin = margin(r = 25)
    ),
    
    axis.text.x = element_text(
      size = 11, 
      color = "#2F4F4F",
      face = "bold"
    ),
    axis.title.x = element_text(
      size = 12,
      face = "bold",
      color = "#2F4F4F",
      margin = margin(t = 15)
    ),
    
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold", color = "#2F4F4F"),
    legend.text = element_text(size = 11, color = "#2F4F4F"),
    legend.background = element_rect(fill = "#FDFCF7", color = NA),
    legend.margin = margin(b = 15),
    
    plot.margin = margin(30, 80, 30, 120),
    
    # Adding more space between bars
    panel.spacing = unit(2, "lines")
  )

print(bar_chart)