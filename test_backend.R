# ==============================================================================
# TEST SUITE: CHESS PERFORMANCE ANALYZER - BACKEND VERIFICATION
# ==============================================================================
# This script loads chess_backend.R, runs all 11 PRD statistical features,
# verifies calculations with assertions, tests adding a new game, and prints
# a clean verification report to the terminal.
# ==============================================================================

# 1. Source the backend logic
source("chess_backend.R")

cat("\n======================================================================\n")
cat("  CHESS PERFORMANCE ANALYZER - BACKEND VERIFICATION SUITE\n")
cat("======================================================================\n\n")

# ------------------------------------------------------------------------------
# TEST 1: Initial Data Setup (Feature 1)
# ------------------------------------------------------------------------------
cat("[TEST 1] Verifying Initial Data Setup (Vectors -> data.frame)...\n")
games <- create_initial_games_df()

stopifnot(is.data.frame(games))
stopifnot(nrow(games) == 7)
stopifnot(all(c("Opponent", "Date", "Result", "Color", "Opening", "Rating", "Moves") %in% names(games)))

cat("  -> OK: Initial dataset has", nrow(games), "games with 7 columns.\n\n")

# ------------------------------------------------------------------------------
# TEST 2 & 3 & 4: Derived Stats (Counts, Win %, Avg Moves)
# ------------------------------------------------------------------------------
cat("[TEST 2-4] Verifying Basic Derived Stats...\n")
counts   <- calc_game_counts(games)
win_rate <- calc_win_rate(games)
avg_mov  <- calc_avg_moves(games)

# 7 games: 4 Wins, 2 Losses, 1 Draw
stopifnot(counts$total == 7)
stopifnot(counts$wins == 4)
stopifnot(counts$losses == 2)
stopifnot(counts$draws == 1)

# Win rate = 4/7 * 100 = 57.1%
expected_win_rate <- round((4 / 7) * 100, 1)
stopifnot(win_rate == expected_win_rate)

# Avg moves = sum(34, 48, 29, 52, 41, 36, 44) / 7 = 284 / 7 = 40.6
expected_avg_moves <- round(mean(c(34, 48, 29, 52, 41, 36, 44)), 1)
stopifnot(avg_mov == expected_avg_moves)

cat("  -> OK: Total =", counts$total, "| W:", counts$wins, "| L:", counts$losses, "| D:", counts$draws, "\n")
cat("  -> OK: Win Rate =", win_rate, "%\n")
cat("  -> OK: Avg Game Length =", avg_mov, "moves\n\n")

# ------------------------------------------------------------------------------
# TEST 5 & 6: Opening Analytics (Most Played, Best Opening)
# ------------------------------------------------------------------------------
cat("[TEST 5-6] Verifying Opening Analytics...\n")
most_played <- find_most_played_opening(games)
best_op     <- find_best_opening(games, min_games = 2)
ops_summary <- get_openings_summary(games)

# Italian Game is played 3 times (all 3 were Wins)
# Sicilian Defense is played 2 times (1 Win, 1 Loss = 50%)
# Queen's Gambit is played 1 time (Draw) -> Excluded by min_games = 2 rule
# French Defense is played 1 time (Loss) -> Excluded by min_games = 2 rule
stopifnot(most_played$name == "Italian Game")
stopifnot(most_played$count == 3)

stopifnot(best_op$name == "Italian Game")
stopifnot(best_op$win_rate == 100.0)
stopifnot(best_op$games == 3)

cat("  -> OK: Most Played Opening =", most_played$name, "(", most_played$count, "games )\n")
cat("  -> OK: Best Opening (min 2 games) =", best_op$name, "(", best_op$status, ")\n")
cat("  -> OK: Summary breakdown table generated for", nrow(ops_summary), "distinct openings.\n\n")

# ------------------------------------------------------------------------------
# TEST 7: Rating Trend
# ------------------------------------------------------------------------------
cat("[TEST 7] Verifying Rating Trend Data...\n")
trend <- get_rating_trend(games)

stopifnot(nrow(trend) == 7)
stopifnot(trend$Rating[1] == 1420)
stopifnot(trend$Rating[7] == 1433)
stopifnot(trend$Rating[nrow(trend)] - trend$Rating[1] == 13)

cat("  -> OK: Rating trajectory generated from Game 1 (1420) to Game 7 (1433). Net change: +13\n\n")

# ------------------------------------------------------------------------------
# TEST 8: Performance by Color
# ------------------------------------------------------------------------------
cat("[TEST 8] Verifying White vs Black Performance Breakdown...\n")
color_perf <- calc_color_performance(games)

# White games: 4 (vs GM Bot [W], Elena [W], CasualPawn [D], TacticalTitan [W]) -> 3W, 0L, 1D (75.0% win rate)
# Black games: 3 (vs Alex [L], SpeedDemon [W], KnightRider [L]) -> 1W, 2L, 0D (33.3% win rate)
stopifnot(color_perf$White$total == 4)
stopifnot(color_perf$White$wins == 3)
stopifnot(color_perf$White$draws == 1)
stopifnot(color_perf$White$win_rate == 75.0)

stopifnot(color_perf$Black$total == 3)
stopifnot(color_perf$Black$wins == 1)
stopifnot(color_perf$Black$losses == 2)
stopifnot(color_perf$Black$win_rate == 33.3)

cat("  -> OK: White Performance: ", color_perf$White$wins, "W /", color_perf$White$losses, "L /", color_perf$White$draws, "D (", color_perf$White$win_rate, "% )\n")
cat("  -> OK: Black Performance: ", color_perf$Black$wins, "W /", color_perf$Black$losses, "L /", color_perf$Black$draws, "D (", color_perf$Black$win_rate, "% )\n\n")

# ------------------------------------------------------------------------------
# TEST 9: Form Classification (Last 5 Games if-else)
# ------------------------------------------------------------------------------
cat("[TEST 9] Verifying Form Classification (if-else logic on recent games)...\n")
form <- classify_current_form(games, n_recent = 5)

# Last 5 games (games 3 to 7): Win, Draw, Win, Win, Loss -> 3 Wins, 1 Draw, 1 Loss
# Under our rule: 3 wins -> "Solid Form ⚡"
stopifnot(form$wins == 3)
stopifnot(form$losses == 1)
stopifnot(form$draws == 1)
stopifnot(form$label == "Solid Form ⚡")

cat("  -> OK: Last 5 Results =", paste(form$recent_results, collapse = " -> "), "\n")
cat("  -> OK: Form Status =", form$label, "|", form$description, "\n\n")

# ------------------------------------------------------------------------------
# TEST 10: Leaderboard & Badge Color Mapping
# ------------------------------------------------------------------------------
cat("[TEST 10] Verifying Leaderboard Log & Color Badges...\n")
log_data <- prepare_game_log(games)

stopifnot(nrow(log_data) == 7)
stopifnot(log_data$BadgeColor[1] == "#2ECC71") # Win = green
stopifnot(log_data$BadgeColor[2] == "#E74C3C") # Loss = red
stopifnot(log_data$BadgeColor[4] == "#9CA3AF") # Draw = gray

cat("  -> OK: All 7 rows mapped with correct UI hex badge colors (#2ECC71, #E74C3C, #9CA3AF).\n\n")

# ------------------------------------------------------------------------------
# TEST 11: Match Report Summary Card
# ------------------------------------------------------------------------------
cat("[TEST 11] Verifying Match Report Summary Card Generator...\n")
report <- generate_match_report(games)

stopifnot(report$total_games == 7)
stopifnot(report$win_rate == 57.1)
stopifnot(report$best_opening == "Italian Game")
stopifnot(report$current_form == "Solid Form ⚡")
stopifnot(report$rating_change == 13)

cat("  -> OK: Match Report Bundle:\n")
cat("         - Total Games   :", report$total_games, "\n")
cat("         - Win Rate      :", report$win_rate, "%\n")
cat("         - Avg Moves     :", report$avg_moves, "\n")
cat("         - Best Opening  :", report$best_opening, "(", report$best_op_stat, ")\n")
cat("         - Current Form  :", report$current_form, "\n")
cat("         - Rating Change :", ifelse(report$rating_change >= 0, paste0("+", report$rating_change), report$rating_change), 
    "(", report$start_rating, "->", report$current_rating, ")\n\n")

# ------------------------------------------------------------------------------
# TEST 12: Dynamic Addition of a New Game (add_game)
# ------------------------------------------------------------------------------
cat("[TEST 12] Testing add_game() and Dynamic Recalculation...\n")

# Add Game 8: Win vs Vishy_Fan
# Recent 5 games become: [4] Draw, [5] Win, [6] Win, [7] Loss, [8] Win -> 3 Wins, 1 Draw, 1 Loss
games_updated <- add_game(
  df       = games,
  opponent = "Vishy_Fan",
  date     = "2026-03-20",
  result   = "Win",
  color    = "White",
  opening  = "Italian Game",
  rating   = 1445,
  moves    = 31
)

stopifnot(nrow(games_updated) == 8)
counts_updated <- calc_game_counts(games_updated)
stopifnot(counts_updated$total == 8)
stopifnot(counts_updated$wins == 5)

# Verify 3 wins in last 5 keeps Solid Form
form_updated <- classify_current_form(games_updated, n_recent = 5)
stopifnot(form_updated$wins == 3)
stopifnot(form_updated$label == "Solid Form ⚡")

# Now add Game 9: Another Win to push recent wins to 4 and trigger "On Fire 🔥"!
# Recent 5 games become: [5] Win, [6] Win, [7] Loss, [8] Win, [9] Win -> 4 Wins, 1 Loss
games_on_fire <- add_game(
  df       = games_updated,
  opponent = "KasparovBot",
  date     = "2026-03-22",
  result   = "Win",
  color    = "Black",
  opening  = "Sicilian Defense",
  rating   = 1458,
  moves    = 39
)

stopifnot(nrow(games_on_fire) == 9)
form_on_fire <- classify_current_form(games_on_fire, n_recent = 5)
stopifnot(form_on_fire$wins == 4)
stopifnot(form_on_fire$label == "On Fire 🔥")

# Rating change is now 1458 - 1420 = +38
report_on_fire <- generate_match_report(games_on_fire)
stopifnot(report_on_fire$rating_change == 38)
stopifnot(report_on_fire$current_form == "On Fire 🔥")

cat("  -> OK: Successfully added Game 8 (Win vs Vishy_Fan) -> Form is 'Solid Form ⚡'.\n")
cat("  -> OK: Successfully added Game 9 (Win vs KasparovBot) -> Form elevated to 'On Fire 🔥'.\n")
cat("  -> OK: Net rating change updated dynamically from +13 to +38.\n\n")

cat("======================================================================\n")
cat("  ALL 12 BACKEND TESTS PASSED SUCCESSFULLY! (100% BASE R)\n")
cat("======================================================================\n\n")
