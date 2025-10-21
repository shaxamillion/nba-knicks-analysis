library(gt)
library(gtExtras)
library(dplyr)

# Create dataset
set_plays_data <- data.frame(
  Set_Play = c("Weave Step", "Weave Keep", "Get 45", "Get 45 Zoom"),
  Uses = c(23, 20, 23, 11),
  PPP = c(1.16, 0.87, 0.84, 1.09),
  ePPP = c(1.06, 1.12, 1.08, 1.01),
  Set_to_Chance = c(52, 80, 74, 45),
  Set_to_Chance_display = c("52%", "80%", "74%", "45%")
)

# Create gt table
set_plays_table <- set_plays_data %>%
  gt() %>%
  # Hide numeric Set_to_Chance
  cols_hide(Set_to_Chance) %>%
  # Add title
  tab_header(
    title = "Motion Offense Analysis",
    subtitle = "Preseason Games"
  ) %>%
  # Format column labels
  cols_label(
    Set_Play = "SET PLAY",
    Uses = "USES",
    PPP = "PPP",
    ePPP = "ePPP",
    Set_to_Chance_display = "SET TO CHANCE"
  ) %>%
  # Format numeric columns
  fmt_number(
    columns = c(PPP, ePPP),
    decimals = 2
  ) %>%
  # Color code PPP (red to green gradient)
  data_color(
    columns = PPP,
    colors = scales::col_numeric(
      palette = c("#FF6B6B", "#FFE5E5", "#FFFFFF", "#D4EDDA", "#90EE90"),
      domain = c(0.84, 1.16)
    ),
    apply_to = "fill"
  ) %>%
  # Color code ePPP (red to green gradient)
  data_color(
    columns = ePPP,
    colors = scales::col_numeric(
      palette = c("#FF6B6B", "#FFE5E5", "#FFFFFF", "#D4EDDA", "#90EE90"),
      domain = c(1.01, 1.12)
    ),
    apply_to = "fill"
  ) %>%
  # Yellow gradient for Set to Chance
  data_color(
    columns = Set_to_Chance,
    colors = scales::col_numeric(
      palette = c("#FFE5E5", "#FFF9C4", "#D4EDDA"),
      domain = c(45, 80)
    ),
    apply_to = "fill"
  ) %>%
  # Center columns
  cols_align(
    align = "center",
    columns = c(Uses, PPP, ePPP, Set_to_Chance_display)
  ) %>%
  # Left align play names
  cols_align(
    align = "left",
    columns = Set_Play
  ) %>%
  # Style column headers
  tab_style(
    style = list(
      cell_fill(color = "#2C3E50"),
      cell_text(
        weight = 700,
        size = px(11),
        color = "#FFFFFF",
        transform = "uppercase"
      ),
      cell_borders(sides = "bottom", color = "#34495E", weight = px(2))
    ),
    locations = cells_column_labels()
  ) %>%
  # Style title
  tab_style(
    style = list(
      cell_fill(color = "#34495E"),
      cell_text(
        weight = 700,
        size = px(20),
        color = "#FFFFFF"
      )
    ),
    locations = cells_title(groups = "title")
  ) %>%
  # Style subtitle
  tab_style(
    style = list(
      cell_fill(color = "#34495E"),
      cell_text(
        size = px(12),
        color = "#ECF0F1"
      )
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  # Style play names
  tab_style(
    style = list(
      cell_fill(color = "#F8F9FA"),
      cell_text(
        size = px(14),
        weight = 600,
        color = "#2C3E50"
      )
    ),
    locations = cells_body(columns = Set_Play)
  ) %>%
  # Style Uses column
  tab_style(
    style = list(
      cell_fill(color = "#E8EAF6"),
      cell_text(
        size = px(14),
        weight = 600,
        color = "#2C3E50"
      )
    ),
    locations = cells_body(columns = Uses)
  ) %>%
  # Style PPP and ePPP text (dark on colored background)
  tab_style(
    style = list(
      cell_text(
        size = px(14),
        weight = 700,
        color = "#2C3E50"
      )
    ),
    locations = cells_body(columns = c(PPP, ePPP))
  ) %>%
  # Style Set to Chance text
  tab_style(
    style = list(
      cell_text(
        size = px(14),
        weight = 600,
        color = "#2C3E50"
      )
    ),
    locations = cells_body(columns = Set_to_Chance_display)
  ) %>%
  # Table styling options
  tab_options(
    table.font.size = 14,
    table.background.color = "#FFFFFF",
    heading.align = "center",
    table.border.top.color = "#34495E",
    table.border.top.width = px(3),
    table.border.bottom.color = "#34495E",
    table.border.bottom.width = px(3),
    column_labels.border.bottom.width = px(2),
    data_row.padding = px(12),
    table.width = px(750),
    heading.padding = px(14)
  )

# Display table
set_plays_table