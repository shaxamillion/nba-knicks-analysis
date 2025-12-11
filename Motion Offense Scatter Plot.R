library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

# Create the dataset
motion_data <- data.frame(
  Game = c("P1", "P2", "P3", "P4", "P5", 1:23),
  Q1 = c(15, 9, 10, 11, 7, 6, 6, 11, 8, 9, 4, 7, 5, 5, 12, 4, 12, 7, 5, 6, 7, 10, 10, 5, 2, 4, 2, 4),
  Q2 = c(8, 8, 9, 13, 14, 6, 10, 10, 11, 8, 7, 11, 8, 7, 9, 10, 5, 11, 7, 5, 4, 3, 2, 8, 8, 6, 5, 6),
  Q3 = c(10, 8, 8, 0, 12, 6, 7, 16, 11, 7, 9, 5, 0, 6, 11, 7, 5, 12, 4, 1, 5, 6, 4, 7, 6, 4, 2, 6),
  Q4 = c(0, 0, 0, 0, 4, 6, 7, 6, 6, 11, 7, 4, 8, 3, 7, 4, 4, 3, 3, 2, 1, 3, 4, 6, 1, 2, 2, 1),
  stringsAsFactors = FALSE
)

# Add opponent names
opponents <- c(
  "PHI", "PHI", "MIN", "WAS", "CHA",  # Preseason
  "CLE", "BOS", "MIA", "MIL", "CHI", "CHI", "WSH", "MIN", 
  "BKN", "MEM", "ORL", "MIA", "MIA", "DAL", "ORL", "BKN", 
  "CHA", "MIL", "TOR", "BOS", "CHA", "UTA", "ORL"  # Regular season
)

motion_data$Opponent <- opponents
motion_data$Game_Num <- 1:nrow(motion_data)

# Reshape data to long format
plot_data <- motion_data %>%
  pivot_longer(cols = c(Q1, Q2, Q3, Q4), 
               names_to = "Quarter", 
               values_to = "Motion_Plays") %>%
  mutate(Quarter_Num = as.numeric(gsub("Q", "", Quarter)),
         X_Position = Game_Num + (Quarter_Num - 2.5) * 0.15,
         Game_Label = ifelse(Game_Num <= 5, Game, Game_Num),
         Label = paste0(Opponent, "\n", Game_Label)) %>%
  # Remove Q4 from preseason games (games 1-5)
  filter(!(Game_Num <= 5 & Quarter == "Q4"))

# Create modern, visually appealing plot
ggplot(plot_data, aes(x = X_Position, y = Motion_Plays)) +
  geom_smooth(aes(x = Game_Num), method = "loess", se = TRUE, 
              color = "#E74C3C", fill = "#E74C3C", alpha = 0.15, 
              size = 1.2, span = 0.5) +
  geom_point(aes(color = Quarter), size = 3, alpha = 0.75) +
  geom_text_repel(data = plot_data %>% filter(Quarter == "Q1"),
                  aes(label = Label, x = Game_Num), 
                  size = 2.8, 
                  family = "sans",
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 30,
                  color = "#2C3E50",
                  segment.color = "#BDC3C7",
                  segment.size = 0.3) +
  scale_color_manual(values = c("Q1" = "#3498DB", "Q2" = "#F39C12", 
                                "Q3" = "#2ECC71", "Q4" = "#9B59B6")) +
  scale_x_continuous(breaks = seq(0, 28, 2), limits = c(0, 29)) +
  scale_y_continuous(breaks = seq(0, 16, 2), limits = c(-1, 17)) +
  labs(
    title = "Saying Goodbye to the Motion Offense",
    subtitle = "Starting possessions with motion offense is declining",
    x = "Game Number",
    y = "Motion Offense Plays",
    color = "Quarter",
    caption = "Tracked by: ShaxNBA | Data: 2025-26 NBA Season | Each point represents one quarter"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "#F8F9FA", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA),
    panel.grid.major = element_line(color = "#E0E0E0", size = 0.3),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, 
                              color = "#2C3E50", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, 
                                 color = "#7F8C8D", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#95A5A6", hjust = 1,
                                margin = margin(t = 15)),
    
    axis.title = element_text(size = 12, face = "bold", color = "#34495E"),
    axis.text = element_text(size = 10, color = "#7F8C8D"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold", color = "#34495E"),
    legend.text = element_text(size = 10, color = "#7F8C8D"),
    legend.background = element_rect(fill = "#F8F9FA", color = NA),
    legend.key = element_rect(fill = "#F8F9FA", color = NA),
    
    plot.margin = margin(20, 20, 20, 20)
  )