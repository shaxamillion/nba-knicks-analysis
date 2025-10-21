library(gt)
library(gtExtras)
library(dplyr)

# Combined lineup dataset from Games 1, 2, & 3 - Top 5
lineup_data <- data.frame(
  lineup = c(
    "Brunson-Bridges-OG-KAT-Robinson",
    "Brunson-McBride-Bridges-OG-KAT",
    "Brunson-McBride-Bridges-Yabu-KAT",
    "Brunson-Bridges-Pacome-KAT-Robinson",
    "Brunson-Shamet-Bridges-KAT-Robinson"
  ),
  minutes = c("12:03", "6:38", "6:57", "6:09", "4:40"),
  eppp = c(0.98, 1.21, 0.86, 1.16, 1.21),
  total_seconds = c(723, 398, 417, 369, 280),
  # Player IDs for each position (PG, SG, SF, PF, C)
  p1_id = c("1628973", "1628973", "1628973", "1628973", "1628973"),
  p2_id = c("1628969", "1630540", "1630540", "1628969", "1629013"),
  p3_id = c("1628384", "1628969", "1628969", "1642359", "1628969"),
  p4_id = c("1626157", "1628384", "1627824", "1626157", "1626157"),
  p5_id = c("1629011", "1626157", "1626157", "1629011", "1629011")
)

# Create gt table
lineup_table <- lineup_data %>%
  gt() %>%
  # Hide ID columns and total_seconds
  cols_hide(c(total_seconds, p1_id, p2_id, p3_id, p4_id, p5_id)) %>%
  # Add title and subtitle
  tab_header(
    title = "Top 5 Lineup Combinations",
    subtitle = "Preseason Games 1, 2 & 3"
  ) %>%
  # Format column labels
  cols_label(
    lineup = "LINEUP",
    minutes = "MINUTES",
    eppp = "ePPP"
  ) %>%
  # Add player headshots
  text_transform(
    locations = cells_body(columns = lineup),
    fn = function(x) {
      row_nums <- seq_along(x)
      
      # Create headshots for all 5 players
      p1_imgs <- paste0('<img src="https://cdn.nba.com/headshots/nba/latest/1040x760/', 
                        lineup_data$p1_id[row_nums], '.png" height="42" style="border-radius: 50%; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">')
      p2_imgs <- paste0('<img src="https://cdn.nba.com/headshots/nba/latest/1040x760/', 
                        lineup_data$p2_id[row_nums], '.png" height="42" style="border-radius: 50%; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">')
      p3_imgs <- paste0('<img src="https://cdn.nba.com/headshots/nba/latest/1040x760/', 
                        lineup_data$p3_id[row_nums], '.png" height="42" style="border-radius: 50%; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">')
      p4_imgs <- paste0('<img src="https://cdn.nba.com/headshots/nba/latest/1040x760/', 
                        lineup_data$p4_id[row_nums], '.png" height="42" style="border-radius: 50%; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">')
      p5_imgs <- paste0('<img src="https://cdn.nba.com/headshots/nba/latest/1040x760/', 
                        lineup_data$p5_id[row_nums], '.png" height="42" style="border-radius: 50%; box-shadow: 0 2px 4px rgba(0,0,0,0.1);">')
      
      paste0('<div style="display: flex; align-items: center; gap: 10px; justify-content: center;">',
             p1_imgs, p2_imgs, p3_imgs, p4_imgs, p5_imgs,
             '</div>')
    }
  ) %>%
  # Add subtle gradient background
  data_color(
    columns = total_seconds,
    colors = scales::col_numeric(
      palette = c("#78A8CE", "#78A8CE"),
      domain = c(280, 723)
    ),
    apply_to = "fill"
  ) %>%
  # Add color coding for ePPP (green = good, red = bad, 1.00 is league average)
  data_color(
    columns = eppp,
    colors = scales::col_numeric(
      palette = c("#FF6B6B", "#FFE5E5", "#FFFFFF", "#D4EDDA", "#90EE90"),
      domain = c(0.80, 1.25)
    ),
    apply_to = "fill"
  ) %>%
  # Format ePPP to 2 decimals
  fmt_number(
    columns = eppp,
    decimals = 2
  ) %>%
  # Center all columns
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  # Style column headers
  tab_style(
    style = list(
      cell_fill(color = "#78A8CE"),
      cell_text(
        weight = 800,
        size = px(11),
        font = google_font("Inter"),
        color = "#4A4A4A",
        transform = "uppercase"
      ),
      cell_borders(sides = "bottom", color = "#78A8CE", weight = px(1))
    ),
    locations = cells_column_labels()
  ) %>%
  # Style title
  tab_style(
    style = list(
      cell_fill(color = "#78A8CE"),
      cell_text(
        weight = 700,
        size = px(20),
        font = google_font("Inter"),
        color = "#2B2B2B"
      )
    ),
    locations = cells_title(groups = "title")
  ) %>%
  # Style subtitle
  tab_style(
    style = list(
      cell_fill(color = "#78A8CE"),
      cell_text(
        size = px(13),
        font = google_font("Inter"),
        color = "#2B2B2B",
        weight = 700
      )
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  # Style minutes text
  tab_style(
    style = list(
      cell_text(
        size = px(15),
        weight = 700,
        font = google_font("Inter"),
        color = "#4A4A4A"
      )
    ),
    locations = cells_body(columns = c(minutes, eppp))
  ) %>%
  # Table styling options
  tab_options(
    table.font.size = 14,
    table.background.color = "#FFF",
    heading.align = "center",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    column_labels.border.bottom.width = px(1),
    column_labels.border.top.style = "none",
    data_row.padding = px(12),
    table.width = px(600),
    heading.padding = px(5)
  ) %>%
  # Add footnote explaining ePPP
  tab_footnote(
    footnote = "ePPP = Expected Points Per Possession (League avg ~1.00). Green = above average, Red = below average",
    locations = cells_column_labels(columns = eppp)
  )

# Display table
lineup_table