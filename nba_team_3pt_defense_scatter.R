library(hoopR)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggimage)

# Get NBA team defensive 3PT stats
defense_3pt <- nba_leaguedashptteamdefend(
  league_id = '00',
  season = '2025-26',
  per_mode = 'Totals',
  defense_category = '3 Pointers'
)

# Convert to data frame and clean
defense_3pt_df <- as.data.frame(defense_3pt)

# Clean column names and select relevant columns
defense_3pt_clean <- defense_3pt_df %>%
  rename_with(~str_remove(., 'LeagueDashPtTeamDefend.')) %>%
  mutate(
    FREQ = as.numeric(FREQ),
    FG3_PCT = as.numeric(FG3_PCT) * 100,  # Convert to percentage
    logo_url = paste0("https://cdn.nba.com/logos/nba/", TEAM_ID, "/primary/L/logo.svg")
  ) %>%
  select(TEAM_ABBREVIATION, TEAM_NAME, TEAM_ID, FREQ, FG3_PCT, GP, logo_url)

# View the data
head(defense_3pt_clean)

# Create scatter plot
ggplot(defense_3pt_clean, aes(x = FG3_PCT, y = FREQ)) +
  # Add quadrant reference lines
  geom_hline(yintercept = mean(defense_3pt_clean$FREQ), 
             linetype = "dashed", color = "#CCCCCC", linewidth = 0.8, alpha = 0.6) +
  geom_vline(xintercept = mean(defense_3pt_clean$FG3_PCT), 
             linetype = "dashed", color = "#CCCCCC", linewidth = 0.8, alpha = 0.6) +
  
  # Add team logos
  geom_image(aes(image = logo_url), size = 0.05, asp = 1.5) +
  
  # Labels
  labs(
    title = "NBA Team 3-Point Defense: Opponent Shooting % vs Frequency Faced",
    subtitle = "2025-26 Season • How well teams defend vs how often they face 3PT shots",
    x = "Opponent 3-Point FG% Allowed",
    y = "3-Point Shot Frequency Faced (%)"
  ) +
  
  # Axis formatting
  scale_y_continuous(
    breaks = seq(0.35, 0.50, 0.02),
    labels = function(x) paste0(round(x * 100, 1), "%")
  ) +
  scale_x_continuous(
    breaks = seq(30, 42, 2),
    labels = function(x) paste0(x, "%")
  ) +
  
  # Add quadrant labels
  annotate("text", x = min(defense_3pt_clean$FG3_PCT) + 1, 
           y = max(defense_3pt_clean$FREQ) - 0.01, 
           label = "High Frequency\nLow Opp%", 
           size = 3, color = "#888888", fontface = "italic", hjust = 0) +
  annotate("text", x = max(defense_3pt_clean$FG3_PCT) - 1, 
           y = max(defense_3pt_clean$FREQ) - 0.01, 
           label = "High Frequency\nHigh Opp%", 
           size = 3, color = "#888888", fontface = "italic", hjust = 1) +
  annotate("text", x = min(defense_3pt_clean$FG3_PCT) + 1, 
           y = min(defense_3pt_clean$FREQ) + 0.01, 
           label = "Low Frequency\nLow Opp%", 
           size = 3, color = "#888888", fontface = "italic", hjust = 0) +
  annotate("text", x = max(defense_3pt_clean$FG3_PCT) - 1, 
           y = min(defense_3pt_clean$FREQ) + 0.01, 
           label = "Low Frequency\nHigh Opp%", 
           size = 3, color = "#888888", fontface = "italic", hjust = 1) +
  
  # Theme
  theme_minimal(base_size = 14, base_family = "sans") +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      size = 20, 
      face = "bold", 
      color = "#1a1a1a",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, 
      size = 11, 
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
    axis.text = element_text(size = 11, color = "#404040", face = "bold"),
    axis.title = element_text(size = 12, face = "bold", color = "#404040"),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.ticks = element_blank(),
    plot.margin = margin(25, 25, 25, 25)
  )

# Display the plot
print(last_plot())
