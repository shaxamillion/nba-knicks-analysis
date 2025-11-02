library(tidyverse)

# Create data for the last 5 games
data <- tibble(
  Game = rep(1:5, each = 2),
  Category = rep(c("Playcall", "Motion Offense"), 5),
  Percentage = c(5, 42, 3, 51, 7, 53, 3, 49, 26, 44)
) %>%
  group_by(Game) %>%
  mutate(
    Other = 100 - sum(Percentage),
    Game_Label = paste("Game", Game)
  ) %>%
  ungroup()

# Add "Other" category to each game
data_complete <- data %>%
  select(Game, Category, Percentage, Game_Label) %>%
  bind_rows(
    data %>%
      select(Game, Other, Game_Label) %>%
      distinct() %>%
      mutate(Category = "Other") %>%
      rename(Percentage = Other)
  ) %>%
  mutate(
    Category = factor(Category, levels = c("Playcall", "Motion Offense", "Other"))
  )

# Create pie charts
ggplot(data_complete, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 2) +
  coord_polar("y", start = 0) +
  facet_wrap(~ Game_Label, nrow = 1) +
  scale_fill_manual(
    values = c("Playcall" = "#EF4444", 
               "Motion Offense" = "#3B82F6", 
               "Other" = "#E5E7EB")
  ) +
  geom_text(
    aes(label = ifelse(Percentage > 3, paste0(Percentage, "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 3.5
  ) +
  labs(
    title = "Playcall% and Motion Offense% Distribution",
    subtitle = "Breakdown across last 5 games",
    fill = NULL
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "#64748B", size = 12, hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )