# Basketball Rotation Charts (there's three examples)

# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Minutes Data for Time Intervals
rotation_data <- data.frame(
  player = c("Brunson", "McBride", "Bridges", "OG", "KAT", "Robinson", "Clarkson", "Hart", "Yabusele"),
  total = c(16, 14, 15, 15, 15, 13, 11, 12, 9),
  `0-5` = c(5, 5, 5, 5, 5, 0, 0, 0, 0),
  `5-8` = c(3, 3, 0, 3, 0, 3, 0, 3, 0),
  `8-11` = c(3, 0, 0, 0, 0, 3, 3, 3, 3),
  `11-14` = c(0, 0, 3, 0, 3, 0, 3, 3, 3),
  `14-18` = c(0, 0, 4, 4, 4, 4, 4, 0, 0),
  `18-19` = c(0, 1, 0, 0, 0, 1, 1, 1, 1),
  `19-21` = c(2, 2, 0, 0, 0, 2, 0, 2, 2),
  `21-24` = c(3, 3, 3, 3, 3, 0, 0, 0, 0),
  check.names = FALSE
)

# Convert to long format
rotation_long <- rotation_data %>%
  pivot_longer(cols = -c(player, total), names_to = "interval", values_to = "minutes") %>%
  mutate(
    status = ifelse(minutes > 0, "On Court", "Off Court"),
    interval = factor(interval, levels = c("0-5", "5-8", "8-11", "11-14", "14-18", "18-19", "19-21", "21-24")),
    player = factor(player, levels = rev(c("Brunson", "McBride", "Bridges", "OG", "KAT", "Robinson", "Clarkson", "Hart", "Yabusele")))
  )

# Create the chart
chart <- ggplot(rotation_long, aes(x = interval, y = player, fill = status)) +
  geom_tile(color = "white", size = 1.2, alpha = 0.9) +
  
  # Color scheme
  scale_fill_manual(
    values = c("On Court" = "#00D4AA", "Off Court" = "#2D3748"),
    name = "",
    guide = guide_legend(
      override.aes = list(size = 1),
      keywidth = 1.5,
      keyheight = 1.5
    )
  ) +
  
  # Labels and titles
  labs(
    title = "PLAYER ROTATION CHART",
    subtitle = "Court Time Distribution Across Game Intervals",
    x = "Game Time Intervals (Minutes)",
    y = "",
    caption = "■ On Court    ■ Off Court"
  ) +
  
  # theme change
  theme_minimal() +
  theme(
    # Background and panel
    plot.background = element_rect(fill = "#F8FAFC", color = NA),
    panel.background = element_rect(fill = "#F8FAFC", color = NA),
    panel.grid = element_blank(),
    
    # Title styling
    plot.title = element_text(
      size = 24, 
      face = "bold", 
      hjust = 0.5, 
      color = "#1A202C",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 14, 
      hjust = 0.5, 
      color = "#4A5568",
      margin = margin(b = 20)
    ),
    
    # Axis styling
    axis.title.x = element_text(
      size = 12, 
      face = "bold", 
      color = "#2D3748",
      margin = margin(t = 15)
    ),
    axis.text.x = element_text(
      size = 11, 
      color = "#4A5568",
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 12, 
      face = "bold", 
      color = "#2D3748"
    ),
    
    # Legend styling
    legend.position = "none",  # We'll use the caption instead
    
    # Caption styling
    plot.caption = element_text(
      size = 12,
      hjust = 0.5,
      color = "#4A5568",
      margin = margin(t = 15)
    ),
    
    # Margins
    plot.margin = margin(30, 30, 30, 30)
  )

print(chart)

# Version with gradient borders

stylish_chart <- ggplot(rotation_long, aes(x = interval, y = player)) +
  # Shadow
  geom_tile(aes(fill = status), 
            color = "#E2E8F0", 
            size = 0.8,
            alpha = 0.95) +
  
  # Gradient color scheme
  scale_fill_manual(
    values = c("On Court" = "#10B981", "Off Court" = "#374151"),
    name = ""
  ) +
  
  # Minutes text change (only when > 2 to avoid clutter)
  geom_text(
    data = rotation_long %>% filter(minutes > 2),
    aes(label = minutes),
    color = "white",
    size = 3,
    fontface = "bold",
    alpha = 0.8
  ) +
  
  labs(
    title = "🏀 PLAYER ROTATION MATRIX",
    subtitle = "Real-time court presence throughout the game",
    x = "Game Timeline (Minutes)",
    y = "Players",
    caption = "🟢 On Court  ⚫ Off Court  |  Numbers show minutes played in interval"
  ) +
  
  theme_void() +
  theme(
    # Background
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    
    # Title styling
    plot.title = element_text(
      size = 22, 
      face = "bold", 
      hjust = 0.5, 
      color = "#111827",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0.5, 
      color = "#6B7280",
      margin = margin(b = 25),
      face = "italic"
    ),
    
    # Axis text
    axis.text.x = element_text(
      size = 11, 
      color = "#374151",
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 12, 
      face = "bold", 
      color = "#1F2937",
      margin = margin(r = 15)
    ),
    
    # Caption
    plot.caption = element_text(
      size = 11,
      hjust = 0.5,
      color = "#6B7280",
      margin = margin(t = 20)
    ),
    
    # Margins
    plot.margin = margin(40, 40, 40, 40),
    
    # Remove legend
    legend.position = "none"
  )

print(stylish_chart)

# Cream theme version

cream_chart <- ggplot(rotation_long, aes(x = interval, y = player, fill = status)) +
  geom_tile(color = "white", size = 2, alpha = 0.9) +
  
  scale_fill_manual(
    values = c("On Court" = "#2E8B57", "Off Court" = "#F4F1E8"),
    name = ""
  ) +
  
  labs(
    title = "ROTATION DASHBOARD",
    subtitle = "Player Court Time Analysis",
    x = "Time Intervals",
    y = "Players",
    caption = "■ Active    □ Inactive"
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
    
    # Y-axis (player names)
    axis.text.y = element_text(
      size = 12, 
      face = "bold", 
      color = "#2F4F4F",
      hjust = 1,  # Right align the text
      margin = margin(r = 10)  # Space between names and chart
    ),
    axis.title.y = element_text(
      size = 12,
      face = "bold",
      color = "#2F4F4F",
      margin = margin(r = 15)
    ),
    
    # X-axis styling
    axis.text.x = element_text(
      size = 11, 
      color = "#2F4F4F",
      face = "bold",
      margin = margin(t = 10)
    ),
    axis.title.x = element_text(
      size = 12,
      face = "bold",
      color = "#2F4F4F",
      margin = margin(t = 15)
    ),
    
    # Caption
    plot.caption = element_text(
      size = 11,
      hjust = 0.5,
      color = "#696969",
      margin = margin(t = 15)
    ),
    
    # Margins - more space on left for player names
    plot.margin = margin(30, 30, 30, 50),
    legend.position = "none"
  )

print(cream_chart)