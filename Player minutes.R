# Player Minutes Chart + Lollipop chart

# libraries
library(ggplot2)
library(dplyr)
library(forcats)

# Player Minutes Data
player_minutes <- data.frame(
  player = c("Brunson", "McBride", "Bridges", "OG", "KAT", "Hart", "Clarkson", "Robinson", "Yabusele"),
  minutes = c(16, 14, 15, 15, 15, 12, 11, 13, 9),
  # Add role categories based on minutes
  role = c(
    rep("Starter", 5),  
    rep("Key Reserve", 2),
    rep("Front-court depth", 2)
  )
)

# Horizontal bar chart
minutes_chart <- ggplot(player_minutes, aes(x = minutes, y = fct_reorder(player, minutes), fill = role)) +
  geom_col(width = 0.6, alpha = 0.9, color = "white", size = 1.5) +
  
  # Add minute labels on bars
  geom_text(aes(label = paste0(minutes, " min")), 
            hjust = -0.1, 
            color = "#2F4F4F", 
            size = 4.5, 
            fontface = "bold") +
  
  scale_fill_manual(
    values = c(
      "Starter" = "#2E8B57",        # Dark sea green
      "Key Reserve" = "#9ACD32",    # Yellow green
      "Front-court depth" = "#DAA520"     # Golden rod
    ),
    name = "Player Role"
  ) +
  
  scale_x_continuous(
    limits = c(0, 18),
    breaks = c(0, 5, 10, 15, 18),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "PLAYER MINUTES DISTRIBUTION",
    subtitle = "Total playing time for each player",
    x = "Minutes Played",
    y = "Players"
  ) +
  
  theme_minimal() +
  theme(
    # Cream background
    plot.background = element_rect(fill = "#FDFCF7", color = NA),
    panel.background = element_rect(fill = "#FDFCF7", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#F0E68C", size = 0.3, linetype = "dashed"),
    
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
    
    # Y-axis (player names)
    axis.text.y = element_text(
      size = 12, 
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
    plot.margin = margin(30, 80, 30, 60)
  )

print(minutes_chart)

# Lollipop chart

lollipop_chart <- ggplot(player_minutes, aes(x = minutes, y = fct_reorder(player, minutes))) +
  
  # Add segments (sticks)
  geom_segment(aes(x = 0, xend = minutes, y = player, yend = player, color = role), 
               size = 2, alpha = 0.8) +
  
  # Add points (lollipops)
  geom_point(aes(color = role, size = minutes), alpha = 0.9) +
  
  # Add minute labels
  geom_text(aes(label = paste0(minutes, " min")), 
            hjust = -0.5, 
            color = "#2F4F4F", 
            size = 4, 
            fontface = "bold") +
  
  scale_color_manual(
    values = c(
      "Starter" = "#2E8B57",
      "Key Reserve" = "#9ACD32",
      "Front-court depth" = "#DAA520"
    ),
    name = "Player Role"
  ) +
  
  scale_size_continuous(range = c(6, 10), guide = "none") +
  
  scale_x_continuous(
    limits = c(0, 18),
    breaks = c(0, 5, 10, 15),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "PLAYER USAGE CHART",
    subtitle = "Minutes distribution across the roster",
    x = "Minutes Played",
    y = "Players"
  ) +
  
  theme_minimal() +
  theme(
    # cream styling
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
      size = 12, 
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
    
    plot.margin = margin(30, 80, 30, 60)
  )

print(lollipop_chart)

# Summary stats table
summary_stats <- player_minutes %>%
  group_by(role) %>%
  summarise(
    players = n(),
    total_minutes = sum(minutes),
    avg_minutes = round(mean(minutes), 1),
    .groups = "drop"
  )

print("Summary Statistics:")
print(summary_stats)