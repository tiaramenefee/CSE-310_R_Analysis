# Load Libraries
library(DBI)
library(RSQLite)
library(ggplot2)

# Data Connections
connect_and_fetch <- function(db_path) {
  con <- dbConnect(RSQLite::SQLite(), db_path, extended_types = TRUE)
  
  query <- "
    SELECT
        p.name,
        g.week,
        g.opponent,
        ds.attempts,
        ds.yards AS passing_yards
    FROM DakStats ds
    JOIN Player p ON ds.player_id = p.player_id
    JOIN Game g ON ds.game_id = g.game_id
    WHERE p.name = 'Dak Prescott';
  "
  
  player_data <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  
  return(player_data)
}

# Analyze Data/Make Plot
analyze_performance <- function(data, player_name) {
  cat("--- Performing Linear Regression for", player_name, "---\n\n")
  
  model <- lm(passing_yards ~ attempts, data = data)
  
  print(summary(model))
  
  p <- ggplot(data, aes(x = attempts, y = passing_yards)) +
    geom_point(color = "blue", size = 4) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = paste("Passing Yards vs. Attempts for", player_name),
      subtitle = "2025 Season",
      x = "Pass Attempts",
      y = "Passing Yards"
    ) +
    theme_minimal()
  
  print(p)
  
  return(model)
}

# Export Results to CSV
export_to_csv <- function(df, filename) {
  write.csv(df, filename, row.names = FALSE)
  cat("\n--- Data successfully exported to", filename, "---\n")
}

# Run Program
db_file <- "cowboys_stats.db"

dak_stats_df <- connect_and_fetch(db_file)

cat("\n--- Fetched Player Data ---\n")
print(dak_stats_df)
cat("\nDataframe columns:\n")
for (col_name in colnames(dak_stats_df)) {
  cat("- ", col_name, "\n")
}

dak_model <- analyze_performance(dak_stats_df, "Dak Prescott")

export_to_csv(dak_stats_df, "dak_prescott_2025_stats.csv")

dak_jersey <- as.integer(4)
cat("\nAnalysis complete for player #", dak_jersey, "\n")