library(tidyverse)
library(gt)
library(gtExtras)

# Create dataset
basketball_stats <- data.frame(
  Player_ID = c("001", "002", "003", "004", "005", "006", "007", "008", "009", "010", "011"),
  Ball_Handlers = c("Jalen Green", "FVV", "Trae Young", "Damian Lillard", "Tyrese Haliburton", 
                    "Paul George", "Jared McCain", "Coby White", "Zach LaVine", 
                    "Brandon Miller", "Vasilije Micić"),
  Team = c("HOU", "HOU", "ATL", "MIL", "IND", "PHI", "PHI", "CHI", "CHI", "CHA", "CHA"),
  Plays = c(15, 29, 41, 33, 38, 21, 15, 16, 32, 30, 18),
  PPA = c(0.70, 0.85, 0.73, 0.65, 1.09, 1.12, 0.57, 0.54, 0.84, 0.45, 0.80),
  ePPA = c(0.77, 0.83, 0.75, 0.97, 0.88, 0.76, 0.88, 0.68, 0.72, 0.77, 0.68),
  Reset_Pct = c(26.7, 17.2, 29.3, 21.2, 23.7, 19.0, 13.3, 31.3, 28.1, 13.3, 38.9),
  Pass_Pct = c(27.3, 60.9, 51.7, 69.2, 44.8, 41.2, 38.5, 27.3, 65.2, 26.9, 45.5),
  NP_Plays = c(8, 9, 14, 8, 16, 10, 8, 8, 8, 19, 6),
  NP_PPA = c(1.07, 0.67, 0.71, 0.99, 1.37, 1.40, 1.07, 0.45, 0.75, 0.45, 1.10),
  NP_ePPA = c(1.06, 0.81, 0.91, 1.22, 1.10, 0.88, 1.03, 0.99, 0.65, 0.82, 1.18),
  nba_player_id = c("1630224", "1627832", "1629027", "203081", "1630169", 
                    "202331", "1642272", "1629632", "203897", "1641706", "203995"),
  stringsAsFactors = FALSE
)
# Add player headshot URLs
basketball_stats <- basketball_stats %>%
  mutate(headshot_url = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", nba_player_id, ".png")) %>%
  select(-Player_ID, -nba_player_id) %>%
  select(headshot_url, everything())

# Create gt table
basketball_stats %>%
  gt() %>%
  
  # Embed headshots
  gt_img_rows(columns = headshot_url, height = 35) %>%
  
  # Table header
  tab_header(
    title = "PnR Ball Handlers",
    subtitle = "Hand-tracked data by @ShaxNBA"
  ) %>%
  
  # Column labels
  cols_label(
    Ball_Handlers = "Ball Handlers",
    Team = "Team",
    Plays = "Plays",
    PPA = "PPA",
    ePPA = "ePPA",
    Reset_Pct = "Reset%",
    Pass_Pct = "Pass%",
    NP_Plays = "NP Plays",
    NP_PPA = "NP PPA",
    NP_ePPA = "NP ePPA",
    headshot_url = ""
  ) %>%
  
  # Format percentages with % signs
  fmt_percent(
    columns = c(Reset_Pct, Pass_Pct),
    decimals = 1,
    scale_values = FALSE
  ) %>%
  
  # Format numeric columns
  fmt_number(
    columns = c(PPA, ePPA, NP_PPA, NP_ePPA),
    decimals = 2
  ) %>%
  
  # Align columns
  cols_align(
    align = "center",
    columns = c(Team, Plays, PPA, ePPA, Reset_Pct, Pass_Pct, NP_Plays, NP_PPA, NP_ePPA)
  ) %>%
  
  cols_align(
    align = "left",
    columns = Ball_Handlers
  ) %>%
  
  # Styling
  tab_style(
    style = list(
      cell_fill(color = "#f8f5f0"),
      cell_text(
        font = "Arial",
        weight = "normal",
        color = "#2c3e50",
        size = px(11)
      )
    ),
    locations = cells_body()
  ) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#34495e"),
      cell_text(
        font = "Arial",
        weight = "bold",
        color = "white",
        size = px(12)
      )
    ),
    locations = cells_column_labels()
  ) %>%
  
  tab_style(
    style = list(
      cell_text(
        font = "Arial",
        weight = "bold",
        size = px(18),
        color = "#2c3e50"
      )
    ),
    locations = cells_title(groups = "title")
  ) %>%
  
  tab_style(
    style = list(
      cell_text(
        font = "Arial",
        weight = "normal",
        size = px(13),
        color = "#7f8c8d"
      )
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  # Highlight high performing numbers - individual column conditions
  tab_style(
    style = list(
      cell_fill(color = "#d5f4e6"),
      cell_text(weight = "bold", color = "#27ae60")
    ),
    locations = cells_body(
      columns = PPA,
      rows = PPA >= 1.0
    )
  ) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#d5f4e6"),
      cell_text(weight = "bold", color = "#27ae60")
    ),
    locations = cells_body(
      columns = ePPA,
      rows = ePPA >= 1.0
    )
  ) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#d5f4e6"),
      cell_text(weight = "bold", color = "#27ae60")
    ),
    locations = cells_body(
      columns = NP_PPA,
      rows = NP_PPA >= 1.0
    )
  ) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#d5f4e6"),
      cell_text(weight = "bold", color = "#27ae60")
    ),
    locations = cells_body(
      columns = NP_ePPA,
      rows = NP_ePPA >= 1.0
    )
  ) %>%
  
  # Style team names
  tab_style(
    style = list(
      cell_text(
        weight = "bold",
        color = "#555"
      )
    ),
    locations = cells_body(columns = Team)
  ) %>%
  
  # Footnote for NP
  tab_footnote(
    footnote = "NP = No Pass",
    locations = cells_column_labels(columns = c(NP_Plays, NP_ePPA, NP_PPA))
  ) %>%
  
  # Table options for fonts, title fonts, background, column labels, tabl size etc.
  tab_options(
    table.font.names = "Arial",
    table.font.size = px(11),
    table.background.color = "#faf8f5",
    table.width = pct(85),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(13),
    heading.background.color = "#faf8f5",
    column_labels.background.color = "#34495e",
    column_labels.font.weight = "bold",
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f5f2ed",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#2c3e50",
    data_row.padding = px(8),
    source_notes.font.size = px(10),
    footnotes.font.size = px(10)
  )