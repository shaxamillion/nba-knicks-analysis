library(ggplot2)
library(ggalluvial)
library(dplyr)
library(tidyr)

# Create dataset
create_alluvial_data <- function() {
  usage_data <- data.frame(
    Defended_Player = c("KAT", "Sims", "OG", "Hart"),
    Possessions = c(350, 117, 17, 100),
    Brunson = c(53.0, 88.4, 54.0, 55.0),
    Bridges = c(25.3, 22.9, 29.4, 29.0),
    OG = c(19.8, 28.4, 48.1, 11.0),
    KAT = c(53.1, 0.0, 41.7, 45.0),
    McBride = c(15.4, 25.7, 5.9, 15.0),
    Hart = c(24.4, 14.4, 20.9, 34.0)
  )
  
  # Convert to long format
  alluvial_data <- usage_data %>%
    pivot_longer(cols = c(Brunson, Bridges, OG, KAT, McBride, Hart),
                 names_to = "Player",
                 values_to = "Usage_Pct") %>%
    filter(Usage_Pct > 5) %>%  # Filter out very small usage
    mutate(
      # Emphasize differences / Format
      freq = round(Usage_Pct^1.5 / 3),  # Exponential scaling to amplify differences
      freq = pmax(freq, 1),  # Ensure minimum of 1
      Scenario = paste0(Defended_Player, "\n(", Possessions, " poss.)")
    ) %>%
    
    # Expand rows based on frequency
    uncount(freq)
  
  return(alluvial_data)
}

alluvial_data <- create_alluvial_data()

# Add color for players
player_colors <- c(
  "Brunson" = "#FF0040",
  "Bridges" = "#0080FF",
  "OG" = "#00CC66",
  "KAT" = "#FF8000",
  "McBride" = "#8000FF",
  "Hart" = "#00CCCC"
)

# Create alluvial plot
p1 <- ggplot(alluvial_data,
             aes(axis1 = Scenario, axis2 = Player, fill = Player)) +
  geom_alluvium(aes(fill = Player), alpha = 0.7, decreasing = FALSE) +
  geom_stratum(alpha = 0.8, decreasing = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), 
            size = 3, color = "white", fontface = "bold", decreasing = FALSE) +
  
  # Scaling + labels
  scale_fill_manual(values = player_colors) +
  scale_x_discrete(limits = c("Scenario", "Player"),
                   labels = c("Player Defended\nby Center", "Usage Goes To")) +
  
  # Title, subtitle and caption
  labs(title = "TRUE USAGE FLOW",
       subtitle = "Player true usage% when a player is defended by the opposing center",
       caption = "True Usage% = Player Passing Usage% + Player Scoring Usage%",
       fill = "Player") +
  
  # Styling
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FDF6E3", color = NA),
    panel.background = element_rect(fill = "#FDF6E3", color = NA),
    plot.title = element_text(family = "Arial Black", size = 16, face = "bold", 
                              hjust = 0.5, margin = margin(t = 15, b = 5), color = "#2C3E50"),
    plot.subtitle = element_text(family = "Arial", size = 12, 
                                 hjust = 0.5, margin = margin(b = 25), color = "#34495E"),
    axis.text.x = element_text(family = "Arial", size = 12, face = "bold", color = "#2C3E50"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(20, 25, 15, 25),
    legend.title = element_text(family = "Arial", size = 12, face = "bold", color = "#2C3E50"),
    legend.text = element_text(family = "Arial", size = 11, color = "#34495E"),
    legend.position = "right"
  )

# Display visual
print(p1)