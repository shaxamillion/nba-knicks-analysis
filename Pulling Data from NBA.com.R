# Load required libraries
library(hoopR)
library(stringr)
library(dplyr)

# Get NBA league-wide player stats
general <- nba_leaguedashplayerstats(
  league_id = '00', 
  season = '2025-26',
  measure_type = "Base",
  per_mode = "PerGame"
)

# Convert to data frame
general <- as.data.frame(general)

# Clean and select relevant columns
general <- general %>%
  rename_with(~str_remove(., 'LeagueDashPlayerStats.')) %>%
  mutate_at(c('FG3A', 'FG3_PCT', 'MIN', 'GP'), as.numeric) %>%
  filter(FG3A >= 1.5, MIN >= 15, GP >= 15) %>%
  reframe(PLAYER_ID, PLAYER_NAME, FG3A, FG3_PCT, MIN, GP)

# View the data
head(general)

# Define players to highlight with headshots
highlight_players <- data.frame(
  PLAYER_NAME = c("Paolo Banchero", "Anthony Black", "Desmond Bane", 
                  "Franz Wagner", "Wendell Carter Jr.", "Tyus Jones", 
                  "Jalen Suggs", "Tristan da Silva",
                  "Jalen Brunson", "OG Anunoby", "Karl-Anthony Towns",
                  "Miles McBride", "Mikal Bridges", "Landry Shamet",
                  "Josh Hart", "Jordan Clarkson"),
  nba_player_id = c("1631094", "1641710", "1630217", 
                    "1630532", "1628976", "1626145", 
                    "1630591", "1641783",
                    "1628973", "1628384", "1626157",
                    "1630540", "1628969", "1629013",
                    "1628404", "203903"),
  stringsAsFactors = FALSE
)

# Add headshot URLs to highlight players
highlight_players <- highlight_players %>%
  mutate(headshot_url = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", nba_player_id, ".png"))

# Join with general data to get stats for highlighted players
highlight_data <- general %>%
  inner_join(highlight_players, by = "PLAYER_NAME")

# Create scatter plot
library(ggimage)

ggplot(general, aes(x = FG3_PCT, y = FG3A)) +
  geom_point(size = 3, color = "#2C3E50", alpha = 0.4) +
  geom_image(data = highlight_data, aes(image = headshot_url), size = 0.15) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "NBA 3-Point Shooting: Attempts vs. Accuracy",
       subtitle = "2025-26 Season • Min 1.5 3PA, 15 MIN, 15 GP • Magic & Knicks players highlighted",
       x = "3-Point Percentage",
       y = "3-Point Attempts per Game") +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#FAF9F6", color = NA),
    panel.background = element_rect(fill = "#FAF9F6", color = NA),
    panel.grid.major = element_line(color = "#E0E0E0", size = 0.3),
    panel.grid.minor = element_line(color = "#F0F0F0", size = 0.2),
    plot.title = element_text(face = "bold", size = 16, 
                              color = "#2C3E50", hjust = 0),
    plot.subtitle = element_text(size = 10, color = "#7F8C8D", hjust = 0),
    axis.title = element_text(face = "bold", color = "#2C3E50"),
    axis.text = element_text(color = "#2C3E50"),
    plot.margin = margin(20, 20, 20, 20)
  )