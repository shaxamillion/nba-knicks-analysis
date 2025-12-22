library(ggplot2)
library(dplyr)
library(gridExtra)

# Create the data (CLE is row 1: 92, 27, 59, 46; BOS is row 2: 3, 100, 0, 19; etc.)
eppp_data <- data.frame(
  Game = 1:16,
  Opponent = c("CLE", "BOS", "MIA", "MIL", "CHI", "CHI", "WSH", "MIN",
               "BKN", "MEM", "ORL", "MIA", "MIA", "DAL", "ORL", "BKN"),
  Q1 = c(92, 3, 85, 94, 89, 44, 28, 8, 90, 95, 6, 56, 9, 41, 82, 35),
  Q2 = c(27, 100, 31, 53, 17, 51, 33, 62, 50, 49, 76, 71, 60, 97, 78, 99),
  Q3 = c(59, 0, 37, 12, 86, 83, 96, 58, 1, 32, 54, 87, 64, 14, 73, 80),
  Q4 = c(46, 19, 42, 13, 47, 69, 30, 91, 63, 18, 65, 55, 24, 67, 74, 72)
)

# Calculate summary stats for sorting
eppp_data <- eppp_data %>%
  mutate(
    Mean = round((Q1 + Q2 + Q3 + Q4) / 4, 1),
    StdDev = round(sqrt(((Q1-Mean)^2 + (Q2-Mean)^2 + (Q3-Mean)^2 + (Q4-Mean)^2) / 4), 1),
    Best_Q = apply(cbind(Q1, Q2, Q3, Q4), 1, max),
    Worst_Q = apply(cbind(Q1, Q2, Q3, Q4), 1, min)
  ) %>%
  arrange(desc(Mean)) # Sort by best performance

# Function to create an enhanced radar chart
create_radar <- function(game_num, q1, q2, q3, q4, opponent, mean_val, stddev, best_q, worst_q) {
  # Quarter colors
  quarter_colors <- c("Q1" = "#4A90E2", "Q2" = "#50C878", "Q3" = "#F5A623", "Q4" = "#E74C3C")
  
  # Create data for this game
  quarters <- c(q1, q2, q3, q4)
  df <- data.frame(
    Quarter = c("Q1", "Q2", "Q3", "Q4", "Q1"), # Repeat Q1 to close the polygon
    Value = c(q1, q2, q3, q4, q1)
  )
  
  # Convert to radians for plotting
  df$angle <- seq(0, 2*pi, length.out = 5)
  df$x <- df$Value * cos(df$angle)
  df$y <- df$Value * sin(df$angle)
  
  # Create reference circles
  circles <- data.frame(
    radius = c(25, 50, 75, 100),
    is_fifty = c(FALSE, TRUE, FALSE, FALSE)
  )
  
  # Average circle
  avg_circle <- data.frame(
    x = mean_val * cos(seq(0, 2*pi, length.out = 100)),
    y = mean_val * sin(seq(0, 2*pi, length.out = 100))
  )
  
  # Create axis lines
  axis_lines <- data.frame(
    x_start = rep(0, 4),
    y_start = rep(0, 4),
    x_end = 100 * cos(seq(0, 2*pi, length.out = 5)[1:4]),
    y_end = 100 * sin(seq(0, 2*pi, length.out = 5)[1:4])
  )
  
  # Quarter labels positions
  label_df <- data.frame(
    Quarter = c("Q1", "Q2", "Q3", "Q4"),
    angle = seq(0, 2*pi, length.out = 5)[1:4],
    x = 115 * cos(seq(0, 2*pi, length.out = 5)[1:4]),
    y = 115 * sin(seq(0, 2*pi, length.out = 5)[1:4]),
    color = quarter_colors
  )
  
  # Identify best and worst quarters
  df_points <- df[1:4,]
  df_points$is_best <- df_points$Value == best_q
  df_points$is_worst <- df_points$Value == worst_q
  df_points$Quarter_name <- c("Q1", "Q2", "Q3", "Q4")
  df_points$color <- quarter_colors[df_points$Quarter_name]
  
  # Create the plot
  p <- ggplot() +
    # Reference circles
    lapply(1:nrow(circles), function(i) {
      r <- circles$radius[i]
      line_weight <- if(circles$is_fifty[i]) 0.6 else 0.3
      line_type <- if(circles$is_fifty[i]) "solid" else "dashed"
      geom_path(data = data.frame(
        x = r * cos(seq(0, 2*pi, length.out = 100)),
        y = r * sin(seq(0, 2*pi, length.out = 100))
      ), aes(x = x, y = y), color = "gray70", linetype = line_type, size = line_weight)
    }) +
    # Average circle (dashed)
    geom_path(data = avg_circle, aes(x = x, y = y), 
              color = "blue", linetype = "dotted", size = 0.8, alpha = 0.7) +
    # Axis lines
    geom_segment(data = axis_lines, 
                 aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
                 color = "gray50", size = 0.3) +
    # Colored segments for each quarter
    geom_polygon(data = data.frame(
      x = c(0, df$x[1], df$x[2]),
      y = c(0, df$y[1], df$y[2])
    ), aes(x = x, y = y), fill = quarter_colors["Q1"], alpha = 0.3) +
    geom_polygon(data = data.frame(
      x = c(0, df$x[2], df$x[3]),
      y = c(0, df$y[2], df$y[3])
    ), aes(x = x, y = y), fill = quarter_colors["Q2"], alpha = 0.3) +
    geom_polygon(data = data.frame(
      x = c(0, df$x[3], df$x[4]),
      y = c(0, df$y[3], df$y[4])
    ), aes(x = x, y = y), fill = quarter_colors["Q3"], alpha = 0.3) +
    geom_polygon(data = data.frame(
      x = c(0, df$x[4], df$x[5]),
      y = c(0, df$y[4], df$y[5])
    ), aes(x = x, y = y), fill = quarter_colors["Q4"], alpha = 0.3) +
    # Main polygon outline
    geom_path(data = df, aes(x = x, y = y), 
              color = "black", size = 1.2) +
    # Data points - all as dots
    geom_point(data = df_points, aes(x = x, y = y), 
               size = 4, color = "black", fill = df_points$color, 
               shape = 21, stroke = 1.5) +
    # Quarter labels with colors
    geom_text(data = label_df, aes(x = x, y = y, label = Quarter),
              fontface = "bold", size = 3.5, color = label_df$color) +
    # Percentile values
    geom_text(data = df[1:4,], aes(x = x * 0.65, y = y * 0.65, 
                                   label = paste0(Value, "%")),
              size = 2.8, fontface = "bold", color = "black") +
    coord_fixed() +
    labs(title = paste0("Game ", game_num, " vs ", opponent),
         subtitle = paste0("Avg: ", mean_val, "% | SD: ", stddev)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 8, color = "gray30"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(8, 8, 8, 8)
    ) +
    xlim(-130, 130) + ylim(-130, 130)
  
  return(p)
}

# Create all 16 radar charts (now sorted by performance)
plots <- lapply(1:16, function(i) {
  create_radar(
    eppp_data$Game[i],
    eppp_data$Q1[i],
    eppp_data$Q2[i],
    eppp_data$Q3[i],
    eppp_data$Q4[i],
    eppp_data$Opponent[i],
    eppp_data$Mean[i],
    eppp_data$StdDev[i],
    eppp_data$Best_Q[i],
    eppp_data$Worst_Q[i]
  )
})

# Arrange in a 4x4 grid
grid.arrange(grobs = plots, ncol = 4, nrow = 4)

# Print enhanced summary
cat("\n=== Performance Summary (Sorted Best to Worst) ===\n")
eppp_data %>%
  select(Game, Opponent, Mean, StdDev, Q1, Q2, Q3, Q4) %>%
  print()

cat("\n=== Most Balanced Games (Lowest Standard Deviation) ===\n")
eppp_data %>%
  arrange(StdDev) %>%
  select(Game, Opponent, Mean, StdDev) %>%
  head(5) %>%
  print()

cat("\n=== Most Lopsided Games (Highest Standard Deviation) ===\n")
eppp_data %>%
  arrange(desc(StdDev)) %>%
  select(Game, Opponent, Mean, StdDev) %>%
  head(5) %>%
  print()