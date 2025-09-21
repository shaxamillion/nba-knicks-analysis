library(ggplot2)
library(dplyr)
library(tidyr)

# Create dataset
shot_data <- data.frame(
  Attack_Type = c("Organized Offense", "Freelance"),
  Rim = c(28.1, 25.9),
  Short_Midrange = c(14.2, 25.9),
  Long_Midrange = c(12.4, 13.2),
  Pull_Up_3 = c(8.8, 9.4),
  Catch_Shoot_3 = c(36.5, 25.5),
  PPP = c(0.99, 0.95),
  ePPP = c(1.06, 0.96)
)


# Convert to long format and add coordinates
radar_data <- shot_data %>%
  select(-PPP, -ePPP) %>%
  pivot_longer(cols = c(Rim, Short_Midrange, Long_Midrange, Pull_Up_3, Catch_Shoot_3),
               names_to = "Shot_Type",
               values_to = "Frequency") %>%
  mutate(
    Shot_Type = factor(Shot_Type, levels = c("Rim", "Short_Midrange", "Long_Midrange", "Pull_Up_3", "Catch_Shoot_3")),
    
    # Calculate angles for 5 points around circle
    angle = (as.numeric(Shot_Type) - 1) * 2 * pi / 5,
    
    # Convert to x,y coordinates
    x = Frequency * cos(angle),
    y = Frequency * sin(angle),
    
    # Add labels
    Shot_Label = case_when(
      Shot_Type == "Rim" ~ "Rim",
      Shot_Type == "Short_Midrange" ~ "Short Mid",
      Shot_Type == "Long_Midrange" ~ "Long Mid", 
      Shot_Type == "Pull_Up_3" ~ "Pull-Up 3",
      Shot_Type == "Catch_Shoot_3" ~ "C&S 3"
    )
  )

# Create axis labels with coordinates
axis_labels <- data.frame(
  Shot_Type = c("Rim", "Short_Midrange", "Long_Midrange", "Pull_Up_3", "Catch_Shoot_3"),
  Shot_Label = c("Rim", "Short Mid", "Long Mid", "Pull-Up 3", "C&S 3"),
  angle = c(0, 1, 2, 3, 4) * 2 * pi / 5,
  label_x = 42 * cos(c(0, 1, 2, 3, 4) * 2 * pi / 5),
  label_y = 42 * sin(c(0, 1, 2, 3, 4) * 2 * pi / 5)
)

# Create concentric circles for grid
circles <- data.frame(
  radius = c(10, 20, 30, 40),
  label = c("10%", "20%", "30%", "40%")
)

# Create radar chart
p1 <- ggplot() +
  # Add concentric circles
  lapply(circles$radius, function(r) {
    theta <- seq(0, 2*pi, length.out = 100)
    data.frame(x = r * cos(theta), y = r * sin(theta), radius = r)
  }) %>%
  bind_rows() %>%
  {geom_path(data = ., aes(x = x, y = y, group = radius), 
             color = "#E8DCC6", size = 0.5, alpha = 0.7)} +
  
  # Add axis lines
  geom_segment(data = axis_labels, 
               aes(x = 0, y = 0, xend = label_x, yend = label_y),
               color = "#BDC3C7", size = 0.5, alpha = 0.7) +
  
  # Add color in polygons for each attack type
  geom_polygon(data = radar_data %>% filter(Attack_Type == "Organized Offense"), 
               aes(x = x, y = y), fill = "#FF0040", alpha = 0.4, color = "#FF0040", size = 2.5) +
  geom_polygon(data = radar_data %>% filter(Attack_Type == "Freelance"), 
               aes(x = x, y = y), fill = "#0080FF", alpha = 0.4, color = "#0080FF", size = 2.5) +
  
  # Add points
  geom_point(data = radar_data, aes(x = x, y = y, color = Attack_Type), size = 4, alpha = 0.95) +
  
  
  # Add axis labels
  geom_text(data = axis_labels, aes(x = label_x, y = label_y, label = Shot_Label),
            family = "Arial", size = 4, fontface = "bold", color = "#2C3E50") +
  
  # Scaling + title and subtitle
  scale_color_manual(values = c("Organized Offense" = "#FF0040", "Freelance" = "#0080FF")) +
  coord_fixed(ratio = 1) +
  xlim(-50, 50) + ylim(-50, 50) +
  labs(title = "SHOT PROFILE IN ATTACK TYPE",
       subtitle = "Attack type comparison by shot location frequency",
       color = "Attack Type") +
  
  # Styling
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FDF6E3", color = NA),
    panel.background = element_rect(fill = "#FDF6E3", color = NA),
    plot.title = element_text(family = "Arial Black", size = 16, face = "bold", 
                              hjust = 0.5, margin = margin(t = 15, b = 5), color = "#2C3E50"),
    plot.subtitle = element_text(family = "Arial", size = 12, 
                                 hjust = 0.5, margin = margin(b = 20), color = "#34495E"),
    plot.margin = margin(20, 25, 15, 25),
    legend.title = element_text(family = "Arial", size = 12, face = "bold", color = "#2C3E50"),
    legend.text = element_text(family = "Arial", size = 11, color = "#34495E"),
    legend.position = "bottom"
  ) +
  
  # Add efficiency summary
  annotate("text", x = 0, y = -45, 
           label = "Organized: 1.06 ePPP | Freelance: 0.96 ePPP", 
           family = "Arial", size = 3.5, color = "#2C3E50", fontface = "bold")

# Display chart
print(p1)