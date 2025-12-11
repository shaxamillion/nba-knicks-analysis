library(gt)
library(gtExtras)
library(dplyr)

# Create the dataset
ball_handler_data <- data.frame(
  Player = c("Desmond Bane", "Franz Wagner", "Paolo Banchero", "Jalen Suggs", "Anthony Black"),
  Plays = c(34, 30, 10, 23, 21),
  PPA = c(0.90, 0.90, 0.28, 0.61, 0.55),
  Direct_PPA = c(1.17, 1.43, 0.46, 0.67, 0.68),
  NP_Poss = c(12, 11, 1, 13, 11),
  NP_PPA = c(1.18, 1.27, 0.00, 0.86, 0.32),
  nba_player_id = c("1630217", "1630532", "1631094", "1630591", "1641710"),  # Fill in actual IDs
  stringsAsFactors = FALSE
)

# Add headshot URLs
ball_handler_data <- ball_handler_data %>%
  mutate(headshot = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", nba_player_id, ".png"))

# Create beautiful table
ball_handler_data %>%
  select(headshot, Player, Plays, PPA, Direct_PPA, NP_Poss, NP_PPA) %>%
  gt() %>%
  # Add headshots
  gt_img_rows(columns = headshot, height = 50) %>%
  
  # Column labels
  cols_label(
    headshot = "",
    Player = "Ball Handler",
    Plays = "Plays",
    PPA = "PPA",
    Direct_PPA = "Direct PPA",
    NP_Poss = "NP Poss.",
    NP_PPA = "NP PPA"
  ) %>%
  
  # Format numeric columns
  fmt_number(
    columns = c(PPA, Direct_PPA, NP_PPA),
    decimals = 2
  ) %>%
  
  # Add color scales
  data_color(
    columns = c(Plays),
    colors = scales::col_numeric(
      palette = c("#FFF3CD", "#FFE5A1", "#FFD76E"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(Direct_PPA),
    colors = scales::col_numeric(
      palette = c("#F8D7DA", "#FFFFFF", "#D1F2EB"),
      domain = c(0, 2)
    )
  ) %>%
  data_color(
    columns = c(NP_PPA),
    colors = scales::col_numeric(
      palette = c("#F8D7DA", "#FFFFFF", "#D1F2EB"),
      domain = c(0, 2)
    )
  ) %>%
  
  # Styling
  tab_header(
    title = md("**Orlando Magic Ball Handler Performance**"),
    subtitle = "Direct action efficiency vs. overall play-type efficiency"
  ) %>%
  
  tab_source_note(
    source_note = "Data: 2025-26 NBA Season | ORL Three-Game Analysis"
  ) %>%
  
  # Table styling with modern font
  tab_style(
    style = cell_text(
      font = "Inter, Helvetica Neue, Arial, sans-serif",
      size = px(14),
      weight = 600,
      color = "#2C3E50"
    ),
    locations = cells_column_labels()
  ) %>%
  
  tab_style(
    style = cell_text(
      font = "Inter, Helvetica Neue, Arial, sans-serif",
      size = px(13),
      color = "#34495E"
    ),
    locations = cells_body()
  ) %>%
  
  tab_style(
    style = cell_text(
      font = "Inter, Helvetica Neue, Arial, sans-serif",
      size = px(18),
      weight = 700,
      color = "#2C3E50"
    ),
    locations = cells_title(groups = "title")
  ) %>%
  
  tab_style(
    style = cell_text(
      font = "Inter, Helvetica Neue, Arial, sans-serif",
      size = px(12),
      color = "#7F8C8D"
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  tab_style(
    style = cell_text(
      font = "Inter, Helvetica Neue, Arial, sans-serif",
      size = px(10),
      color = "#95A5A6"
    ),
    locations = cells_source_notes()
  ) %>%
  
  # Background colors and alignment
  tab_options(
    table.background.color = "#F8F9FA",
    heading.background.color = "#F8F9FA",
    column_labels.background.color = "#E9ECEF",
    table.border.top.color = "#DEE2E6",
    table.border.bottom.color = "#DEE2E6",
    heading.border.bottom.color = "#DEE2E6",
    column_labels.border.top.color = "#DEE2E6",
    column_labels.border.bottom.color = "#ADB5BD",
    table.font.size = px(13),
    heading.align = "center",
    source_notes.font.size = px(10)
  ) %>%
  
  # Column alignment
  cols_align(
    align = "center",
    columns = c(Plays, PPA, Direct_PPA, NP_Poss, NP_PPA)
  ) %>%
  
  cols_align(
    align = "left",
    columns = Player
  )