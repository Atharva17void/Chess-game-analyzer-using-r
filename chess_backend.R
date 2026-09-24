# ==============================================================================
# CHESS PERFORMANCE ANALYZER - BACKEND ANALYTICS ENGINE (Base R)
# ==============================================================================
# This script contains the pure data structures, statistical functions, and 
# business logic for analyzing chess games. No external libraries or ML are used.
#
# Sections:
#   1. Data Setup (Vectors -> Data Frame)
#   2. Adding New Games (rbind helper)
#   3. Basic Derived Stats (Total Games, W/L/D, Win %, Avg Moves)
#   4. Opening Analytics (Most Played, Best Opening with min games threshold)
#   5. Rating Trend Data (Chronological sequence & change)
#   6. Performance by Color (White vs Black breakdown)
#   7. Form Classification (if-else logic on recent games)
#   8. Leaderboard & Badge Color Mapping (Green Win / Red Loss / Gray Draw)
#   9. Match Report Summary Card Generator
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: DATA SETUP
# ------------------------------------------------------------------------------
# A data frame is constructed by combining vectors of equal length.
# Each row represents a single game; each column represents a game attribute.

create_initial_games_df <- function() {
  # 1. Define individual Base R vectors for initial sample games
  opponents <- c(
    "GrandmasterBot",
    "Alex_Chess99",
    "Elena_Rook",
    "CasualPawn",
    "SpeedDemon_Blitz",
    "TacticalTitan",
    "KnightRider"
  )
  
  dates <- as.Date(c(
    "2026-03-01",
    "2026-03-03",
    "2026-03-05",
    "2026-03-08",
    "2026-03-12",
    "2026-03-15",
    "2026-03-18"
  ))
  
  results <- c(
    "Win",
    "Loss",
    "Win",
    "Draw",
    "Win",
    "Win",
    "Loss"
  )
  
  colors <- c(
    "White",
    "Black",
    "White",
    "White",
    "Black",
    "White",
    "Black"
  )
  
  openings <- c(
    "Italian Game",
    "Sicilian Defense",
    "Italian Game",
    "Queen's Gambit",
    "Sicilian Defense",
    "Italian Game",
    "French Defense"
  )
  
  ratings <- c(1420, 1412, 1422, 1422, 1431, 1442, 1433)
  
  moves <- c(34, 48, 29, 52, 41, 36, 44)
  
  # 2. Combine all vectors into a Base R data.frame
  df <- data.frame(
    Opponent = opponents,
    Date     = dates,
    Result   = results,
    Color    = colors,
    Opening  = openings,
    Rating   = ratings,
    Moves    = moves,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

# ------------------------------------------------------------------------------
# SECTION 2: ADDING NEW GAMES
# ------------------------------------------------------------------------------
# Appends a newly played game to an existing data frame using rbind().

add_game <- function(df, opponent, date, result, color, opening, rating, moves) {
  # Validate inputs
  if (is.null(opponent) || opponent == "") opponent <- "Anonymous"
  if (is.null(date) || is.na(date)) date <- Sys.Date()
  if (!result %in% c("Win", "Loss", "Draw")) stop("Result must be Win, Loss, or Draw")
  if (!color %in% c("White", "Black")) stop("Color must be White or Black")
  if (is.null(opening) || opening == "") opening <- "Custom Opening"
  
  new_row <- data.frame(
    Opponent = as.character(opponent),
    Date     = as.Date(date),
    Result   = as.character(result),
    Color    = as.character(color),
    Opening  = as.character(opening),
    Rating   = as.numeric(rating),
    Moves    = as.numeric(moves),
    stringsAsFactors = FALSE
  )
  
  updated_df <- rbind(df, new_row)
  rownames(updated_df) <- NULL # Reset row numbers cleanly
  return(updated_df)
}

# ------------------------------------------------------------------------------
# SECTION 3: BASIC DERIVED STATS (Features 2, 3, 4)
# ------------------------------------------------------------------------------

# Total games and Win / Loss / Draw counts
calc_game_counts <- function(df) {
  total_games <- nrow(df)
  if (total_games == 0) {
    return(list(total = 0, wins = 0, losses = 0, draws = 0))
  }
  
  wins   <- sum(df$Result == "Win")
  losses <- sum(df$Result == "Loss")
  draws  <- sum(df$Result == "Draw")
  
  return(list(
    total  = total_games,
    wins   = wins,
    losses = losses,
    draws  = draws
  ))
}

# Win percentage: (wins / total_games) * 100
calc_win_rate <- function(df) {
  if (nrow(df) == 0) return(0.0)
  wins <- sum(df$Result == "Win")
  rate <- (wins / nrow(df)) * 100
  return(round(rate, 1))
}

# Average game length (mean moves)
calc_avg_moves <- function(df) {
  if (nrow(df) == 0) return(0.0)
  return(round(mean(df$Moves), 1))
}

# ------------------------------------------------------------------------------
# SECTION 4: OPENING ANALYTICS (Features 5, 6)
# ------------------------------------------------------------------------------

# Feature 5: Find most-played opening
find_most_played_opening <- function(df) {
  if (nrow(df) == 0) return(list(name = "None", count = 0))
  
  opening_counts <- table(df$Opening)
  max_idx <- which.max(opening_counts)
  
  return(list(
    name  = names(opening_counts)[max_idx],
    count = as.integer(opening_counts[max_idx])
  ))
}

# Feature 6: Find best opening (highest win rate, min 2 games played with it)
find_best_opening <- function(df, min_games = 2) {
  if (nrow(df) == 0) {
    return(list(name = "None", win_rate = 0, games = 0, status = "No games played"))
  }
  
  unique_openings <- unique(df$Opening)
  best_name <- "None"
  best_rate <- -1
  best_count <- 0
  
  for (op in unique_openings) {
    op_subset <- df[df$Opening == op, ]
    op_total  <- nrow(op_subset)
    
    # Enforce minimum games played requirement
    if (op_total >= min_games) {
      op_wins <- sum(op_subset$Result == "Win")
      op_rate <- (op_wins / op_total) * 100
      
      # Tie-break: higher win rate, or if equal win rate, more games played
      if (op_rate > best_rate || (op_rate == best_rate && op_total > best_count)) {
        best_rate  <- op_rate
        best_name  <- op_name <- op
        best_count <- op_total
      }
    }
  }
  
  if (best_rate == -1) {
    return(list(
      name     = "Need more games",
      win_rate = 0,
      games    = 0,
      status   = paste0("Play at least ", min_games, " games with the same opening to unlock")
    ))
  }
  
  return(list(
    name     = best_name,
    win_rate = round(best_rate, 1),
    games    = best_count,
    status   = paste0(round(best_rate, 1), "% win rate across ", best_count, " games")
  ))
}

# Frequency table for all openings (for bar charts & tables)
get_openings_summary <- function(df) {
  if (nrow(df) == 0) {
    return(data.frame(Opening = character(), Games = integer(), Wins = integer(), WinRate = numeric()))
  }
  
  unique_ops <- unique(df$Opening)
  summary_list <- lapply(unique_ops, function(op) {
    sub <- df[df$Opening == op, ]
    tot <- nrow(sub)
    wns <- sum(sub$Result == "Win")
    data.frame(
      Opening = op,
      Games   = tot,
      Wins    = wns,
      WinRate = round((wns / tot) * 100, 1),
      stringsAsFactors = FALSE
    )
  })
  
  res <- do.call(rbind, summary_list)
  # Sort by total games descending
  res <- res[order(-res$Games, -res$WinRate), ]
  rownames(res) <- NULL
  return(res)
}

# ------------------------------------------------------------------------------
# SECTION 5: RATING TREND DATA (Feature 7)
# ------------------------------------------------------------------------------
# Orders games chronologically and outputs game index, date, rating, and rating change

get_rating_trend <- function(df) {
  if (nrow(df) == 0) {
    return(data.frame(Game = integer(), Date = as.Date(character()), Rating = numeric()))
  }
  
  # Ensure chronological ordering by date, preserving original row order for ties
  ordered_df <- df[order(df$Date), ]
  
  trend <- data.frame(
    Game   = seq_len(nrow(ordered_df)),
    Date   = ordered_df$Date,
    Rating = ordered_df$Rating,
    Change = c(0, diff(ordered_df$Rating))
  )
  
  return(trend)
}

# ------------------------------------------------------------------------------
# SECTION 6: PERFORMANCE BY COLOR (Feature 8)
# ------------------------------------------------------------------------------
# White vs Black performance breakdown

calc_color_performance <- function(df) {
  compute_stats_for_color <- function(col_name) {
    sub <- df[df$Color == col_name, ]
    tot <- nrow(sub)
    if (tot == 0) {
      return(list(total = 0, wins = 0, losses = 0, draws = 0, win_rate = 0.0))
    }
    w <- sum(sub$Result == "Win")
    l <- sum(sub$Result == "Loss")
    d <- sum(sub$Result == "Draw")
    return(list(
      total    = tot,
      wins     = w,
      losses   = l,
      draws    = d,
      win_rate = round((w / tot) * 100, 1)
    ))
  }
  
  return(list(
    White = compute_stats_for_color("White"),
    Black = compute_stats_for_color("Black")
  ))
}

# ------------------------------------------------------------------------------
# SECTION 7: FORM CLASSIFICATION (Feature 9)
# ------------------------------------------------------------------------------
# Classifies current form using if-else logic based on the last 5 results.
# Criteria:
#   - 4-5 Wins        -> "On Fire 🔥"
#   - 3 Wins          -> "Solid Form ⚡"
#   - 3 or more Losses-> "Rough Patch ⚠️"
#   - Otherwise       -> "Steady ⚖️"

classify_current_form <- function(df, n_recent = 5) {
  total_games <- nrow(df)
  if (total_games == 0) {
    return(list(
      label       = "No Data",
      description = "Play games to generate form analysis",
      recent      = character(),
      badge_color = "#9CA3AF"
    ))
  }
  
  # Take the most recent n_recent games (last rows of data frame)
  recent_window <- min(n_recent, total_games)
  recent_results <- tail(df$Result, recent_window)
  
  recent_wins   <- sum(recent_results == "Win")
  recent_losses <- sum(recent_results == "Loss")
  recent_draws  <- sum(recent_results == "Draw")
  
  # If-Else classification logic
  if (recent_wins >= 4) {
    form_label  <- "On Fire 🔥"
    form_desc   <- paste0(recent_wins, " wins in last ", recent_window, " games. Peak momentum!")
    badge_color <- "#F5B942" # Gold accent
  } else if (recent_losses >= 3) {
    form_label  <- "Rough Patch ⚠️"
    form_desc   <- paste0(recent_losses, " losses in last ", recent_window, " games. Time to review tactics.")
    badge_color <- "#E74C3C" # Crimson red
  } else if (recent_wins == 3) {
    form_label  <- "Solid Form ⚡"
    form_desc   <- paste0(recent_wins, " wins in last ", recent_window, " games. Positive trajectory.")
    badge_color <- "#2ECC71" # Emerald green
  } else {
    form_label  <- "Steady ⚖️"
    form_desc   <- paste0("Balanced results (", recent_wins, "W / ", recent_losses, "L / ", recent_draws, "D).")
    badge_color <- "#9CA3AF" # Cool gray
  }
  
  return(list(
    label          = form_label,
    description    = form_desc,
    recent_results = recent_results,
    wins           = recent_wins,
    losses         = recent_losses,
    draws          = recent_draws,
    badge_color    = badge_color
  ))
}

# ------------------------------------------------------------------------------
# SECTION 8: LEADERBOARD & COLOR BADGING (Feature 10)
# ------------------------------------------------------------------------------
# Returns a clean data frame with result badge color hex codes for UI rendering.

prepare_game_log <- function(df) {
  if (nrow(df) == 0) return(df)
  
  # Map Result to UI colors: Win = green, Loss = red, Draw = gray
  badge_colors <- ifelse(
    df$Result == "Win",  "#2ECC71",
    ifelse(df$Result == "Loss", "#E74C3C", "#9CA3AF")
  )
  
  log_df <- df
  log_df$BadgeColor <- badge_colors
  log_df$GameNumber <- seq_len(nrow(df))
  
  # Reorder columns with GameNumber first
  cols <- c("GameNumber", "Date", "Opponent", "Color", "Opening", "Result", "Rating", "Moves", "BadgeColor")
  return(log_df[, cols])
}

# ------------------------------------------------------------------------------
# SECTION 9: MATCH REPORT SUMMARY CARD (Feature 11)
# ------------------------------------------------------------------------------
# Aggregates key metrics into a single structured list for the Match Report card.

generate_match_report <- function(df) {
  total_games <- nrow(df)
  
  if (total_games == 0) {
    return(list(
      total_games   = 0,
      win_rate      = 0.0,
      avg_moves     = 0.0,
      best_opening  = "None",
      current_form  = "No Data",
      rating_change = 0,
      start_rating  = 0,
      current_rating= 0
    ))
  }
  
  counts       <- calc_game_counts(df)
  win_rate     <- calc_win_rate(df)
  avg_moves    <- calc_avg_moves(df)
  best_op_info <- find_best_opening(df, min_games = 2)
  form_info    <- classify_current_form(df, n_recent = 5)
  
  # Rating change from first game to latest game
  ordered_df     <- df[order(df$Date), ]
  start_rating   <- ordered_df$Rating[1]
  current_rating <- ordered_df$Rating[nrow(ordered_df)]
  rating_change  <- current_rating - start_rating
  
  return(list(
    total_games    = counts$total,
    wins           = counts$wins,
    losses         = counts$losses,
    draws          = counts$draws,
    win_rate       = win_rate,
    avg_moves      = avg_moves,
    best_opening   = best_op_info$name,
    best_op_stat   = best_op_info$status,
    current_form   = form_info$label,
    form_desc      = form_info$description,
    form_color     = form_info$badge_color,
    start_rating   = start_rating,
    current_rating = current_rating,
    rating_change  = rating_change
  ))
}
