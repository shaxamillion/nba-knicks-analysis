
library(gt)
library(gtExtras)
library(dplyr)

# Create dataset
pnr_data <- data.frame(
  combination = c("Hart-KAT", "Brunson-KAT", "Bridges-KAT", "McBride-Sims", 
                  "Bridges-Sims", "McBride-KAT", "Bridges-Brunson", "OG-KAT"),
  plays = c(18, 18, 60, 12, 18, 24, 12, 21),
  ppa = c(1.29, 0.93, 0.97, 1.00, 0.65, 0.96, 0.67, 0.86),
  eppa = c(1.09, 0.96, 0.98, 0.83, 0.96, 1.10, 0.85, 0.91),
  shot_making = c(0.20, -0.03, -0.01, 0.18, -0.31, -0.14, -0.18, -0.05),  # Numeric values
  shot_making_display = c("+0.20", "-0.03", "-0.01", "+0.18", "-0.31", "-0.14", "-0.18", "-0.05"),  # Display values
  to_rate = c("5.6%", "16.7%", "11.7%", "0.0%", "16.7%", "8.3%", "8.3%", "9.5%"),
  efg_pct = c("66.7%", "53.3%", "52.0%", "50.0%", "29.2%", "44.4%", "36.4%", "47.4%"),
  poa_id = c("1628404", "1628973", "1628969", "1630540", "1628969", "1630540", "1628969", "1628384"),  # POA defender player IDs
  screener_id = c("1626157", "1626157", "1626157", "1630579", "1630579", "1626157", "1628973", "1626157")   # Screener's defender player IDs
)

# Create gt table
pnr_table <- pnr_data %>%
  gt() %>%
  # Hide the ID columns and numeric shot_making since they're just for reference
  cols_hide(c(shot_making, poa_id, screener_id)) %>%
  # Add title and subtitle
  tab_header(
    title = "Defensive PnR Combination Analysis",
    subtitle = "vs PnR Handler + PnR Roll-Man Playtypes"
  ) %>%
  # Add and Format column labels
  cols_label(
    combination = "Combinations",
    plays = html("Plays <span style='color: #228B22;'>🏀</span>"),
    ppa = "PPA",
    eppa = "ePPA",
    shot_making_display = "Shot Making",
    to_rate = "TO Rate",
    efg_pct = "eFG%"
  ) %>%
  # Add player headshots using NBA headshot URLs
  text_transform(
    locations = cells_body(columns = combination),
    fn = function(x) {
      # Match with player IDs
      row_nums <- seq_along(x)
      
      # Create player headshots in HTML
      poa_imgs <- paste0('<img src="https://cdn.nba.com/headshots/nba/latest/1040x760/', 
                         pnr_data$poa_id[row_nums], '.png" height="30" style="border-radius: 50%;">')
      screener_imgs <- paste0('<img src="https://cdn.nba.com/headshots/nba/latest/1040x760/', 
                              pnr_data$screener_id[row_nums], '.png" height="30" style="border-radius: 50%;">')
      
      paste0('<div style="display: flex; align-items: center; gap: 8px;">',
             poa_imgs, ' + ', screener_imgs, ' ', x, '</div>')
    }
  ) %>%
  
  # Format numeric columns
  fmt_number(
    columns = c(ppa, eppa),
    decimals = 2
  ) %>%
  
  # Color code PPA and ePPA columns (1.00 is the breakpoint - red above, green below)
  data_color(
    columns = c(ppa, eppa),
    colors = scales::col_numeric(
      palette = c("#00CC00", "#90EE90", "#FFFFFF", "#FFB3B3", "#FF0000"),  # Green to white to red
      domain = c(0.6, 1.3),  # Set domain to capture your data range with 1.0 in the middle
      na.color = "#FFFFFF"
    ),
    apply_to = "fill"
  ) %>%
  
  # Center data in columns
  cols_align(
    align = "center",
    columns = c(plays, shot_making_display)
  ) %>%
  # Style the table
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#F8F9FA"),
      cell_text(weight = "bold")
    ),
    locations = cells_title()
  ) %>%
  # Add footnote
  tab_footnote(
    footnote = "PPA = Points Per Attempt, ePPA = Expected Points Per Attempt, Shot Making = PPA - ePPA",
    locations = cells_column_labels(columns = c(ppa))
  ) %>%
  # More Table styling
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 14,
    column_labels.font.weight = "bold",
    table.border.top.color = "#000000",
    table.border.bottom.color = "#000000",
    column_labels.border.bottom.color = "#000000",
    column_labels.border.bottom.width = px(2)
  )

# Display table
pnr_table