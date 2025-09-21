library(tidyverse)
library(gt)
library(gtExtras)

# Create data - add player headshot IDs
coverage_data <- tibble(
  nba_player_id = c("1630224", "1627832", "1629027", "203081", "1630169", 
                    "202331", "1642272", "1629632", "203897", "1641706", "203995"),
  Opp = c("HOU", "HOU", "ATL", "MIL", "IND", "PHI", "PHI", "CHI", "CHI", "CHA", "CHA"),
  Ball_Handlers = c("Jalen Green", "FVV", "Trae Young", "Damian Lillard", "Tyrese Haliburton", 
                    "Paul George", "Jared McCain", "Coby White", "Zach LaVine", 
                    "Brandon Miller", "Vasilije Micić"),
  Drop = c(33.3, 48.3, 24.4, 12.1, 21.1, 28.6, 46.7, 25.0, 12.5, 36.7, 55.6),
  Level = c(40.0, 20.7, 43.9, 45.5, 23.7, 28.6, 6.7, 18.8, 31.3, 13.3, 5.6),
  Soft_Hedge = c(0.0, 17.2, 12.2, 9.1, 5.3, 19.0, 0.0, 0.0, 28.1, 16.7, 16.7),
  Hard_Hedge = c(13.3, 3.4, 0.0, 3.0, 2.6, 14.3, 0.0, 6.3, 6.3, 3.3, 0.0),
  Blitz = c(6.7, 3.4, 9.8, 9.1, 26.3, 4.8, 6.7, 18.8, 6.3, 6.7, 22.2),
  Switch = c(6.7, 3.4, 9.8, 9.1, 26.3, 4.8, 6.7, 18.8, 6.3, 6.7, 22.2),
  Peel_Next = c(0.0, 0.0, 4.9, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
  None = c(0.0, 0.0, 0.0, 12.1, 18.4, 4.8, 40.0, 18.8, 9.4, 13.3, 0.0)
) %>%
  mutate(headshot_url = paste0("https://cdn.nba.com/headshots/nba/latest/260x190/", nba_player_id, ".png")) %>%
  select(-nba_player_id) %>%
  select(headshot_url, everything())

# Create gt table
coverage_table <- coverage_data %>%
  gt() %>%
  
  # Embed headshots
  gt_img_rows(columns = headshot_url, height = 35) %>%
  
  # Add title and subtitle
  tab_header(
    title = "PnR Coverage Analysis",
    subtitle = "Hand-tracked data by @ShaxNBA"
  ) %>%
  
  # Format column labels
  cols_label(
    headshot_url = "",
    Opp = "Team",
    Ball_Handlers = "Ball Handler",
    Drop = "Drop",
    Level = "Level", 
    Soft_Hedge = "Soft Hedge",
    Hard_Hedge = "Hard Hedge",
    Blitz = "Blitz",
    Switch = "Switch",
    Peel_Next = "Peel/Next",
    None = "None"
  ) %>%
  
  # Format percentage columns
  fmt_percent(
    columns = c(Drop, Level, Soft_Hedge, Hard_Hedge, Blitz, Switch, Peel_Next, None),
    decimals = 1,
    scale_values = FALSE
  ) %>%
  
  # Align columns
  cols_align(
    align = "center",
    columns = c(Opp, Drop, Level, Soft_Hedge, Hard_Hedge, Blitz, Switch, Peel_Next, None)
  ) %>%
  
  cols_align(
    align = "left",
    columns = Ball_Handlers
  ) %>%
  
  # Styling
  tab_style(
    style = list(
      cell_text(
        font = "Arial",
        weight = "normal",
        color = "#2c3e50",
        size = px(11)
      )
    ),
    locations = cells_body()
  ) %>%
  
  # Create gradients for each row
  # Create a background for all coverage cells
  tab_style(
    style = list(cell_fill(color = "#fafafa")),
    locations = cells_body(columns = c(Drop, Level, Soft_Hedge, Hard_Hedge, Blitz, Switch, Peel_Next, None))
  ) %>%
  
  # Jalen Green - Red gradient
  tab_style(
    style = list(cell_fill(color = "#ffebeb"), cell_text(color = "#333")),
    locations = cells_body(columns = Drop, rows = 1) # 33.3
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ff9999"), cell_text(color = "#000", weight = "bold")),
    locations = cells_body(columns = Level, rows = 1) # 40.0 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ffcccc"), cell_text(color = "#333")),
    locations = cells_body(columns = Hard_Hedge, rows = 1) # 13.3
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ffe0e0"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Blitz, Switch), rows = 1) # 6.7
  ) %>%
  
  # FVV - Blue gradient
  tab_style(
    style = list(cell_fill(color = "#6699ff"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = Drop, rows = 2) # 48.3 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ccddff"), cell_text(color = "#333")),
    locations = cells_body(columns = Level, rows = 2) # 20.7
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#b3ccff"), cell_text(color = "#333")),
    locations = cells_body(columns = Soft_Hedge, rows = 2) # 17.2
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#e6f0ff"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Hard_Hedge, Blitz, Switch), rows = 2) # 3.4
  ) %>%
  
  # Trae Young - Green gradient
  tab_style(
    style = list(cell_fill(color = "#66cc66"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = Level, rows = 3) # 43.9 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#b3e0b3"), cell_text(color = "#333")),
    locations = cells_body(columns = Drop, rows = 3) # 24.4
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ccebcc"), cell_text(color = "#333")),
    locations = cells_body(columns = Soft_Hedge, rows = 3) # 12.2
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#d9f2d9"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Blitz, Switch), rows = 3) # 9.8
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#e6f7e6"), cell_text(color = "#333")),
    locations = cells_body(columns = Peel_Next, rows = 3) # 4.9
  ) %>%
  
  # Damian Lillard - Purple gradient
  tab_style(
    style = list(cell_fill(color = "#9966cc"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = Level, rows = 4) # 45.5 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#d9cceb"), cell_text(color = "#333")),
    locations = cells_body(columns = Drop, rows = 4) # 12.1
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#d9cceb"), cell_text(color = "#333")),
    locations = cells_body(columns = None, rows = 4) # 12.1
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#e0d6f0"), cell_text(color = "#333")),
    locations = cells_body(columns = Soft_Hedge, rows = 4) # 9.1
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#e0d6f0"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Blitz, Switch), rows = 4) # 9.1
  ) %>%
  
  # Tyrese Haliburton - Orange gradient
  tab_style(
    style = list(cell_fill(color = "#ff8533"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = c(Blitz, Switch), rows = 5) # 26.3 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ffb366"), cell_text(color = "#333")),
    locations = cells_body(columns = Level, rows = 5) # 23.7
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ffcc99"), cell_text(color = "#333")),
    locations = cells_body(columns = Drop, rows = 5) # 21.1
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ffd6b3"), cell_text(color = "#333")),
    locations = cells_body(columns = None, rows = 5) # 18.4
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ffe6d6"), cell_text(color = "#333")),
    locations = cells_body(columns = Soft_Hedge, rows = 5) # 5.3
  ) %>%
  
  # Paul George - Teal gradient
  tab_style(
    style = list(cell_fill(color = "#4da6a6"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = c(Drop, Level), rows = 6) # 28.6 - tied highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#99cccc"), cell_text(color = "#333")),
    locations = cells_body(columns = Soft_Hedge, rows = 6) # 19.0
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#b3d9d9"), cell_text(color = "#333")),
    locations = cells_body(columns = Hard_Hedge, rows = 6) # 14.3
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#e0f2f2"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Blitz, Switch, None), rows = 6) # 4.8
  ) %>%
  
  # Jared McCain - Yellow gradient
  tab_style(
    style = list(cell_fill(color = "#ffb833"), cell_text(color = "#000", weight = "bold")),
    locations = cells_body(columns = Drop, rows = 7) # 46.7 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#cc7700"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = None, rows = 7) # 40.0 - second highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ffe6b3"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Level, Blitz, Switch), rows = 7) # 6.7
  ) %>%
  
  # Coby White - Light Green gradient
  tab_style(
    style = list(cell_fill(color = "#80cc80"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = Drop, rows = 8) # 25.0 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#b3e0b3"), cell_text(color = "#333")),
    locations = cells_body(columns = Level, rows = 8) # 18.8
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#b3e0b3"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Blitz, Switch, None), rows = 8) # 18.8
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#d9f2d9"), cell_text(color = "#333")),
    locations = cells_body(columns = Hard_Hedge, rows = 8) # 6.3
  ) %>%
  
  # Zach LaVine - Indigo gradient
  tab_style(
    style = list(cell_fill(color = "#6666cc"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = Level, rows = 9) # 31.3 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#9999dd"), cell_text(color = "#333")),
    locations = cells_body(columns = Soft_Hedge, rows = 9) # 28.1
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#ccccee"), cell_text(color = "#333")),
    locations = cells_body(columns = Drop, rows = 9) # 12.5
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#d9d9f2"), cell_text(color = "#333")),
    locations = cells_body(columns = None, rows = 9) # 9.4
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#e6e6f7"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Hard_Hedge, Blitz, Switch), rows = 9) # 6.3
  ) %>%
  
  # Brandon Miller - Pink gradient
  tab_style(
    style = list(cell_fill(color = "#e6559a"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = Drop, rows = 10) # 36.7 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#f299c2"), cell_text(color = "#333")),
    locations = cells_body(columns = Soft_Hedge, rows = 10) # 16.7
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#f5b3d1"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Level, None), rows = 10) # 13.3
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#f9ccdd"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Blitz, Switch), rows = 10) # 6.7
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#fce6f0"), cell_text(color = "#333")),
    locations = cells_body(columns = Hard_Hedge, rows = 10) # 3.3
  ) %>%
  
  # Vasilije Micić - Deep Purple gradient
  tab_style(
    style = list(cell_fill(color = "#7733cc"), cell_text(color = "#fff", weight = "bold")),
    locations = cells_body(columns = Drop, rows = 11) # 55.6 - highest
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#b366e0"), cell_text(color = "#333")),
    locations = cells_body(columns = c(Blitz, Switch), rows = 11) # 22.2
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#cc99eb"), cell_text(color = "#333")),
    locations = cells_body(columns = Soft_Hedge, rows = 11) # 16.7
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#e6ccf5"), cell_text(color = "#333")),
    locations = cells_body(columns = Level, rows = 11) # 5.6
  ) %>%
  
  # Cell formatting
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
  
  # Style team names
  tab_style(
    style = list(
      cell_text(
        weight = "bold",
        color = "#555"
      )
    ),
    locations = cells_body(columns = Opp)
  ) %>%
  
  # Add spanner for coverage types
  tab_spanner(
    label = "Coverage Types (%)",
    columns = c(Drop, Level, Soft_Hedge, Hard_Hedge, Blitz, Switch, Peel_Next, None)
  ) %>%
  
  # Table options for fonts, title fonts, background, column labels, tabl size etc.
  tab_options(
    table.font.names = "Arial",
    table.font.size = px(11),
    table.background.color = "#faf8f5",
    table.width = pct(90),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(13),
    heading.background.color = "#faf8f5",
    column_labels.background.color = "#34495e",
    column_labels.font.weight = "bold",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    column_labels.border.top.style = "none",
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#2c3e50",
    data_row.padding = px(8),
    source_notes.font.size = px(10),
    footnotes.font.size = px(10)
  )

# Display table
coverage_table