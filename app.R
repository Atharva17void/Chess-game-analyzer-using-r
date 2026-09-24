# ==============================================================================
# PROJECT: CHESS PERFORMANCE ANALYZER (R + Shiny)
# ==============================================================================
# Single-file Shiny web application built entirely with Base R and Shiny.
# No dplyr, no tidyverse, no ML, and no external frameworks.
#
# Sections:
#   # 1. Data Setup
#   # 2. Derived Stats (win %, avg moves, best opening)
#   # 3. Rating Trend Data
#   # 4. Form Classification (if-else)
#   # 5. UI Layout (sidebar form + tabs)
#   # 6. Server Logic (reactive data, outputs)
#   # 7. Visualizations (rating line chart, win/loss bar chart)
#   # 8. Summary Report
# ==============================================================================

library(shiny)

# ------------------------------------------------------------------------------
# 1. DATA SETUP
# ------------------------------------------------------------------------------
# Combines Base R vectors into an initial data frame representing game history.

create_initial_games_df <- function() {
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
  
  results <- c("Win", "Loss", "Win", "Draw", "Win", "Win", "Loss")
  colors  <- c("White", "Black", "White", "White", "Black", "White", "Black")
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
  moves   <- c(34, 48, 29, 52, 41, 36, 44)
  
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

# Helper to append a new game to the reactive data frame
add_new_game <- function(df, opponent, date, result, color, opening, rating, moves) {
  if (is.null(opponent) || opponent == "") opponent <- "Anonymous"
  if (is.null(date) || is.na(date)) date <- Sys.Date()
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
  rownames(updated_df) <- NULL
  return(updated_df)
}

# ------------------------------------------------------------------------------
# 2. DERIVED STATS (win %, avg moves, best opening)
# ------------------------------------------------------------------------------

calc_basic_stats <- function(df) {
  total_games <- nrow(df)
  if (total_games == 0) {
    return(list(total = 0, wins = 0, losses = 0, draws = 0, win_rate = 0, avg_moves = 0))
  }
  
  wins   <- sum(df$Result == "Win")
  losses <- sum(df$Result == "Loss")
  draws  <- sum(df$Result == "Draw")
  rate   <- round((wins / total_games) * 100, 1)
  avg_m  <- round(mean(df$Moves), 1)
  
  return(list(
    total     = total_games,
    wins      = wins,
    losses    = losses,
    draws     = draws,
    win_rate  = rate,
    avg_moves = avg_m
  ))
}

find_opening_stats <- function(df, min_games = 2) {
  if (nrow(df) == 0) {
    return(list(
      most_played = "None",
      most_played_count = 0,
      best_opening = "None",
      best_win_rate = 0,
      best_count = 0,
      best_status = "No games recorded"
    ))
  }
  
  # Most played opening
  op_counts <- table(df$Opening)
  max_idx   <- which.max(op_counts)
  most_op   <- names(op_counts)[max_idx]
  most_cnt  <- as.integer(op_counts[max_idx])
  
  # Best opening (minimum games filter)
  unique_ops <- unique(df$Opening)
  best_name  <- "Need more games"
  best_rate  <- -1
  best_cnt   <- 0
  
  for (op in unique_ops) {
    sub <- df[df$Opening == op, ]
    n_played <- nrow(sub)
    if (n_played >= min_games) {
      rate <- (sum(sub$Result == "Win") / n_played) * 100
      if (rate > best_rate || (rate == best_rate && n_played > best_cnt)) {
        best_rate <- rate
        best_name <- op
        best_cnt  <- n_played
      }
    }
  }
  
  status_msg <- if (best_rate >= 0) {
    paste0(round(best_rate, 1), "% win rate (", best_cnt, " games)")
  } else {
    paste0("Play min ", min_games, " games with the same opening to qualify")
  }
  
  return(list(
    most_played       = most_op,
    most_played_count = most_cnt,
    best_opening      = best_name,
    best_win_rate     = ifelse(best_rate >= 0, round(best_rate, 1), 0),
    best_count        = best_cnt,
    best_status       = status_msg
  ))
}

calc_color_breakdown <- function(df) {
  compute_color <- function(col_name) {
    sub <- df[df$Color == col_name, ]
    tot <- nrow(sub)
    if (tot == 0) return(list(total = 0, wins = 0, losses = 0, draws = 0, win_rate = 0))
    w <- sum(sub$Result == "Win")
    l <- sum(sub$Result == "Loss")
    d <- sum(sub$Result == "Draw")
    return(list(total = tot, wins = w, losses = l, draws = d, win_rate = round((w / tot) * 100, 1)))
  }
  return(list(White = compute_color("White"), Black = compute_color("Black")))
}

# ------------------------------------------------------------------------------
# 3. RATING TREND DATA
# ------------------------------------------------------------------------------

get_chronological_trend <- function(df) {
  if (nrow(df) == 0) return(data.frame(Game = integer(), Rating = numeric(), Date = as.Date(character())))
  ordered <- df[order(df$Date), ]
  return(data.frame(
    Game   = seq_len(nrow(ordered)),
    Rating = ordered$Rating,
    Date   = ordered$Date,
    Opponent = ordered$Opponent,
    Result = ordered$Result
  ))
}

# ------------------------------------------------------------------------------
# 4. FORM CLASSIFICATION (if-else)
# ------------------------------------------------------------------------------

classify_form <- function(df, n_recent = 5) {
  tot <- nrow(df)
  if (tot == 0) {
    return(list(label = "No Data", desc = "Play games to analyze form", badge_class = "badge-gray"))
  }
  
  recent_window  <- min(n_recent, tot)
  recent_results <- tail(df$Result, recent_window)
  w <- sum(recent_results == "Win")
  l <- sum(recent_results == "Loss")
  d <- sum(recent_results == "Draw")
  
  if (w >= 4) {
    label <- "On Fire 🔥"
    desc  <- paste0(w, " wins in last ", recent_window, " games! Peak momentum.")
    badge <- "badge-gold"
  } else if (l >= 3) {
    label <- "Rough Patch ⚠️"
    desc  <- paste0(l, " losses in last ", recent_window, " games. Review your blunders.")
    badge <- "badge-red"
  } else if (w == 3) {
    label <- "Solid Form ⚡"
    desc  <- paste0(w, " wins in last ", recent_window, " games. Strong play.")
    badge <- "badge-green"
  } else {
    label <- "Steady ⚖️"
    desc  <- paste0("Balanced split: ", w, "W / ", l, "L / ", d, "D.")
    badge <- "badge-gray"
  }
  
  return(list(label = label, desc = desc, badge_class = badge, wins = w, losses = l, draws = d, recent = recent_results))
}

# ------------------------------------------------------------------------------
# 5. UI LAYOUT (sidebar form + tabs)
# ------------------------------------------------------------------------------

ui <- fluidPage(
  # Import Google Fonts: Poppins (Headings) and Inter (Body)
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Poppins:wght@600;700;800&display=swap"
    ),
    
    # Custom CSS: Dark Chess Theme (#0F1115 bg, #1A1D23 cards, #2E7D32 accent)
    tags$style(HTML("
      /* --- GLOBAL BASE STYLES --- */
      body {
        background-color: #0F1115 !important;
        color: #E5E7EB !important;
        font-family: 'Inter', -apple-system, sans-serif !important;
        margin: 0;
        padding-bottom: 30px;
      }
      
      h1, h2, h3, h4, .title-text {
        font-family: 'Poppins', sans-serif !important;
        letter-spacing: 0.5px;
        color: #FFFFFF;
      }
      
      /* --- HEADER BRANDING --- */
      .app-header {
        background: linear-gradient(135deg, #16191E 0%, #1A1D23 100%);
        border-bottom: 1px solid #2A2E37;
        padding: 24px 30px;
        margin-bottom: 24px;
        border-radius: 0 0 16px 16px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.4);
      }
      .app-title {
        font-size: 28px;
        font-weight: 700;
        margin: 0;
        display: flex;
        align-items: center;
        gap: 12px;
      }
      .app-subtitle {
        color: #9CA3AF;
        font-size: 14px;
        margin-top: 4px;
        margin-bottom: 0;
      }
      
      /* --- SIDEBAR PANEL STYLING --- */
      .sidebar-panel {
        background-color: #16191E !important;
        border: 1px solid #262A33 !important;
        border-radius: 12px !important;
        padding: 24px !important;
        box-shadow: 0 8px 24px rgba(0, 0, 0, 0.3) !important;
      }
      .sidebar-title {
        font-size: 18px;
        font-weight: 600;
        color: #FFFFFF;
        border-bottom: 1px solid #262A33;
        padding-bottom: 12px;
        margin-bottom: 18px;
      }
      
      /* Form Controls */
      .form-control, .selectize-input {
        background-color: #0F1115 !important;
        border: 1px solid #333842 !important;
        color: #E5E7EB !important;
        border-radius: 8px !important;
        padding: 10px 14px !important;
        font-size: 14px !important;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: #2E7D32 !important;
        box-shadow: 0 0 0 2px rgba(46, 125, 50, 0.25) !important;
        outline: none !important;
      }
      label {
        color: #9CA3AF !important;
        font-size: 13px !important;
        font-weight: 500 !important;
        margin-bottom: 6px !important;
      }
      
      /* Add Game Button */
      .btn-add-game {
        background: linear-gradient(135deg, #2E7D32 0%, #1B5E20 100%) !important;
        border: none !important;
        color: #FFFFFF !important;
        font-family: 'Poppins', sans-serif !important;
        font-weight: 600 !important;
        font-size: 15px !important;
        border-radius: 8px !important;
        padding: 12px 20px !important;
        width: 100% !important;
        transition: all 0.2s ease-in-out !important;
        box-shadow: 0 4px 12px rgba(46, 125, 50, 0.35) !important;
        cursor: pointer;
      }
      .btn-add-game:hover {
        background: linear-gradient(135deg, #388E3C 0%, #2E7D32 100%) !important;
        transform: translateY(-2px);
        box-shadow: 0 6px 16px rgba(46, 125, 50, 0.5) !important;
      }
      
      /* --- TABS & NAVIGATION --- */
      .nav-tabs {
        border-bottom: 1px solid #2A2E37 !important;
        margin-bottom: 20px !important;
      }
      .nav-tabs > li > a {
        color: #9CA3AF !important;
        background-color: transparent !important;
        border: none !important;
        font-family: 'Poppins', sans-serif !important;
        font-weight: 600 !important;
        font-size: 15px !important;
        padding: 12px 20px !important;
        border-bottom: 3px solid transparent !important;
        transition: all 0.2s ease !important;
      }
      .nav-tabs > li > a:hover {
        color: #E5E7EB !important;
        border-bottom: 3px solid #4B5563 !important;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        color: #2ECC71 !important;
        border-bottom: 3px solid #2ECC71 !important;
        background-color: transparent !important;
      }
      
      /* --- CONTENT CARDS --- */
      .chess-card {
        background-color: #1A1D23;
        border: 1px solid #262A33;
        border-radius: 12px;
        padding: 22px;
        margin-bottom: 20px;
        box-shadow: 0 8px 24px rgba(0, 0, 0, 0.3);
      }
      .chess-card-title {
        font-size: 18px;
        font-weight: 600;
        margin-top: 0;
        margin-bottom: 16px;
        color: #FFFFFF;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      
      /* --- STAT CARDS (Insights Tab) --- */
      .stat-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
        gap: 16px;
        margin-bottom: 24px;
      }
      .stat-box {
        background: #16191E;
        border: 1px solid #262A33;
        border-radius: 12px;
        padding: 18px 20px;
        position: relative;
        overflow: hidden;
      }
      .stat-label {
        font-size: 12px;
        color: #9CA3AF;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        font-weight: 600;
        margin-bottom: 6px;
      }
      .stat-value {
        font-family: 'Poppins', sans-serif;
        font-size: 26px;
        font-weight: 700;
        color: #FFFFFF;
        margin-bottom: 4px;
      }
      .stat-subtext {
        font-size: 12px;
        color: #9CA3AF;
      }
      
      /* --- BADGES --- */
      .badge-win   { background: rgba(46, 204, 113, 0.15); color: #2ECC71; border: 1px solid #2ECC71; }
      .badge-loss  { background: rgba(231, 76, 60, 0.15); color: #E74C3C; border: 1px solid #E74C3C; }
      .badge-draw  { background: rgba(156, 163, 175, 0.15); color: #9CA3AF; border: 1px solid #9CA3AF; }
      .badge-gold  { background: rgba(245, 185, 66, 0.15); color: #F5B942; border: 1px solid #F5B942; }
      .badge-green { background: rgba(46, 204, 113, 0.15); color: #2ECC71; border: 1px solid #2ECC71; }
      .badge-red   { background: rgba(231, 76, 60, 0.15); color: #E74C3C; border: 1px solid #E74C3C; }
      .badge-gray  { background: rgba(156, 163, 175, 0.15); color: #9CA3AF; border: 1px solid #9CA3AF; }
      
      .pill {
        display: inline-block;
        padding: 4px 12px;
        border-radius: 999px;
        font-size: 12px;
        font-weight: 600;
        letter-spacing: 0.3px;
      }
      
      /* --- GAME LOG TABLE --- */
      .custom-table {
        width: 100%;
        border-collapse: separate;
        border-spacing: 0;
        color: #E5E7EB;
        font-size: 14px;
      }
      .custom-table th {
        background-color: #16191E;
        color: #9CA3AF;
        font-weight: 600;
        text-transform: uppercase;
        font-size: 12px;
        letter-spacing: 0.5px;
        padding: 12px 16px;
        border-bottom: 2px solid #262A33;
        text-align: left;
      }
      .custom-table td {
        padding: 12px 16px;
        border-bottom: 1px solid #262A33;
      }
      .custom-table tbody tr:nth-child(even) {
        background-color: rgba(255, 255, 255, 0.02);
      }
      .custom-table tbody tr:hover {
        background-color: rgba(46, 125, 50, 0.08);
      }
    "))
  ),
  
  # Page Header Banner
  div(
    class = "app-header",
    div(
      class = "app-title",
      span("♟", style = "color: #2ECC71; font-size: 32px;"),
      "Chess Performance Analyzer"
    ),
    p(
      class = "app-subtitle",
      "Interactive Game Log, Statistical Trends & Opening Intelligence (Base R + Shiny)"
    )
  ),
  
  # Main Layout: Sidebar (Game Entry) + Main Panel (Tabs)
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      width = 4,
      
      div(class = "sidebar-title", "⚔️ Record New Game"),
      
      textInput(
        inputId = "opponent",
        label   = "Opponent Name / Bot:",
        value   = "CasualPlayer",
        placeholder = "e.g. Magnus, Bot_Easy"
      ),
      
      dateInput(
        inputId = "date",
        label   = "Date Played:",
        value   = Sys.Date(),
        format  = "yyyy-mm-dd"
      ),
      
      selectInput(
        inputId = "result",
        label   = "Result:",
        choices = c("Win", "Loss", "Draw"),
        selected = "Win"
      ),
      
      selectInput(
        inputId = "color",
        label   = "My Piece Color:",
        choices = c("White", "Black"),
        selected = "White"
      ),
      
      selectInput(
        inputId = "opening",
        label   = "Opening Played:",
        choices = c(
          "Italian Game",
          "Sicilian Defense",
          "Queen's Gambit",
          "French Defense",
          "Caro-Kann Defense",
          "Ruy Lopez",
          "King's Indian Defense",
          "English Opening",
          "Other Opening"
        ),
        selected = "Italian Game"
      ),
      
      numericInput(
        inputId = "rating",
        label   = "My Rating at the Time:",
        value   = 1435,
        min     = 100,
        max     = 3500,
        step    = 5
      ),
      
      numericInput(
        inputId = "moves",
        label   = "Number of Moves in Game:",
        value   = 35,
        min     = 1,
        max     = 300,
        step    = 1
      ),
      
      tags$br(),
      actionButton(
        inputId = "add_game",
        label   = "♟ Add Game to Log",
        class   = "btn-add-game"
      )
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        
        # TAB 1: GAME LOG
        tabPanel(
          title = "📋 Game Log",
          tags$br(),
          div(
            class = "chess-card",
            div(class = "chess-card-title", "📜 Complete Game History"),
            uiOutput("game_log_table")
          )
        ),
        
        # TAB 2: CHARTS
        tabPanel(
          title = "📊 Analytics Charts",
          tags$br(),
          div(
            class = "chess-card",
            div(class = "chess-card-title", "📈 Rating Progression (Oldest to Newest)"),
            plotOutput("plot_rating_trend", height = "260px")
          ),
          fluidRow(
            column(
              width = 6,
              div(
                class = "chess-card",
                div(class = "chess-card-title", "🎯 Results Breakdown"),
                plotOutput("plot_results_bar", height = "220px")
              )
            ),
            column(
              width = 6,
              div(
                class = "chess-card",
                div(class = "chess-card-title", "📖 Openings Frequency"),
                plotOutput("plot_openings_bar", height = "220px")
              )
            )
          )
        ),
        
        # TAB 3: INSIGHTS & MATCH REPORT
        tabPanel(
          title = "♛ Insights & Report",
          tags$br(),
          uiOutput("insights_dashboard")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 6. SERVER LOGIC (reactive data, outputs)
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive state: In-memory game log
  # Initialized with sample games; updates whenever a new game is submitted.
  games_state <- reactiveVal(create_initial_games_df())
  
  # Observer: When "Add Game" button is clicked
  observeEvent(input$add_game, {
    current_data <- games_state()
    
    updated_data <- add_new_game(
      df       = current_data,
      opponent = input$opponent,
      date     = input$date,
      result   = input$result,
      color    = input$color,
      opening  = input$opening,
      rating   = input$rating,
      moves    = input$moves
    )
    
    # Update reactive state
    games_state(updated_data)
  })
  
  # ----------------------------------------------------------------------------
  # RENDER: Game Log Table (Tab 1)
  # ----------------------------------------------------------------------------
  output$game_log_table <- renderUI({
    df <- games_state()
    if (nrow(df) == 0) {
      return(p("No games recorded yet. Use the sidebar to add your first game!"))
    }
    
    # Sort with newest game at the top for comfortable log viewing
    df$GameIndex <- seq_len(nrow(df))
    display_df <- df[order(-df$GameIndex), ]
    
    rows <- lapply(seq_len(nrow(display_df)), function(i) {
      row <- display_df[i, ]
      
      badge_class <- if (row$Result == "Win") {
        "badge-win"
      } else if (row$Result == "Loss") {
        "badge-loss"
      } else {
        "badge-draw"
      }
      
      color_icon <- if (row$Color == "White") "⚪ White" else "⚫ Black"
      
      tags$tr(
        tags$td(paste0("#", row$GameIndex)),
        tags$td(as.character(row$Date)),
        tags$td(tags$strong(row$Opponent)),
        tags$td(color_icon),
        tags$td(row$Opening),
        tags$td(span(class = paste("pill", badge_class), row$Result)),
        tags$td(tags$strong(row$Rating)),
        tags$td(paste(row$Moves, "moves"))
      )
    })
    
    tags$div(
      style = "overflow-x: auto;",
      tags$table(
        class = "custom-table",
        tags$thead(
          tags$tr(
            tags$th("#"),
            tags$th("Date"),
            tags$th("Opponent"),
            tags$th("Color"),
            tags$th("Opening"),
            tags$th("Result"),
            tags$th("Rating"),
            tags$th("Length")
          )
        ),
        tags$tbody(rows)
      )
    )
  })
  
  # ----------------------------------------------------------------------------
  # 7. VISUALIZATIONS (rating line chart, win/loss bar chart, openings)
  # ----------------------------------------------------------------------------
  
  # Chart 1: Rating Progression Line Chart
  output$plot_rating_trend <- renderPlot({
    df <- games_state()
    if (nrow(df) == 0) return(NULL)
    
    trend <- get_chronological_trend(df)
    
    # Configure base R plotting parameters for Sleek Dark Theme
    par(
      bg       = "#1A1D23",
      fg       = "#E5E7EB",
      col.axis = "#9CA3AF",
      col.lab  = "#E5E7EB",
      col.main = "#FFFFFF",
      mar      = c(4.5, 4.5, 2, 2),
      family   = "sans",
      las      = 1
    )
    
    y_min <- min(trend$Rating) - 15
    y_max <- max(trend$Rating) + 20
    
    # Plot background canvas and trend line
    plot(
      x    = trend$Game,
      y    = trend$Rating,
      type = "n",
      ylim = c(y_min, y_max),
      xlab = "Game Sequence (Chronological)",
      ylab = "Rating",
      xaxt = "n",
      yaxt = "n",
      bty  = "n"
    )
    
    # Subtle dark grid lines
    grid(nx = NULL, ny = NULL, col = "#262A33", lty = "dotted")
    
    # Rating line
    lines(trend$Game, trend$Rating, col = "#2ECC71", lwd = 3)
    
    # Data point circles with glowing fill
    points(trend$Game, trend$Rating, pch = 21, bg = "#1A1D23", col = "#2ECC71", lwd = 2.5, cex = 1.4)
    
    # Text labels showing ratings above each point
    text(trend$Game, trend$Rating + 4, labels = trend$Rating, col = "#E5E7EB", cex = 0.85, pos = 3)
    
    # Custom axes
    axis(1, at = trend$Game, labels = paste0("G", trend$Game), col.axis = "#9CA3AF", col = "#262A33")
    axis(2, col.axis = "#9CA3AF", col = "#262A33")
  })
  
  # Chart 2: Results Breakdown (Wins, Losses, Draws)
  output$plot_results_bar <- renderPlot({
    df <- games_state()
    if (nrow(df) == 0) return(NULL)
    
    stats <- calc_basic_stats(df)
    counts <- c(Wins = stats$wins, Losses = stats$losses, Draws = stats$draws)
    bar_colors <- c("#2ECC71", "#E74C3C", "#9CA3AF")
    
    par(
      bg       = "#1A1D23",
      fg       = "#E5E7EB",
      col.axis = "#9CA3AF",
      col.lab  = "#E5E7EB",
      col.main = "#FFFFFF",
      mar      = c(3, 4, 2, 1),
      las      = 1
    )
    
    max_c <- max(counts)
    bp <- barplot(
      counts,
      col    = bar_colors,
      border = NA,
      ylim   = c(0, max_c * 1.35 + 1),
      ylab   = "Total Games",
      bty    = "n"
    )
    
    # Count labels directly on top of bars
    text(bp, counts + (max_c * 0.08 + 0.3), labels = counts, col = "#FFFFFF", font = 2, cex = 1.1)
  })
  
  # Chart 3: Openings Frequency Bar Chart
  output$plot_openings_bar <- renderPlot({
    df <- games_state()
    if (nrow(df) == 0) return(NULL)
    
    op_table <- table(df$Opening)
    op_table <- sort(op_table, decreasing = FALSE) # Bottom to top in horizontal barplot
    
    par(
      bg       = "#1A1D23",
      fg       = "#E5E7EB",
      col.axis = "#9CA3AF",
      col.lab  = "#E5E7EB",
      col.main = "#FFFFFF",
      mar      = c(4, 9, 2, 2),
      las      = 1
    )
    
    max_o <- max(op_table)
    bp <- barplot(
      op_table,
      horiz  = TRUE,
      col    = "#B08D57", # Chess gold/wood accent
      border = NA,
      xlim   = c(0, max_o * 1.35 + 1),
      xlab   = "Games Played",
      bty    = "n"
    )
    
    # Count labels at the end of each bar
    text(op_table + (max_o * 0.08 + 0.2), bp, labels = op_table, col = "#FFFFFF", font = 2)
  })
  
  # ----------------------------------------------------------------------------
  # 8. SUMMARY REPORT (Match Report Cards & Detailed Insights)
  # ----------------------------------------------------------------------------
  output$insights_dashboard <- renderUI({
    df <- games_state()
    if (nrow(df) == 0) return(p("Add games to generate your match report."))
    
    stats       <- calc_basic_stats(df)
    op_info     <- find_opening_stats(df, min_games = 2)
    form_info   <- classify_form(df, n_recent = 5)
    color_info  <- calc_color_breakdown(df)
    trend       <- get_chronological_trend(df)
    
    # Rating difference
    net_rating_change <- if (nrow(trend) >= 2) {
      trend$Rating[nrow(trend)] - trend$Rating[1]
    } else {
      0
    }
    
    rating_sign <- if (net_rating_change >= 0) paste0("+", net_rating_change) else as.character(net_rating_change)
    rating_color <- if (net_rating_change >= 0) "#2ECC71" else "#E74C3C"
    
    tagList(
      # Row 1: Key Metric Cards Grid
      div(
        class = "stat-grid",
        
        # Card 1: Total Games & Record
        div(
          class = "stat-box",
          div(class = "stat-label", "♟ Total Games"),
          div(class = "stat-value", stats$total),
          div(class = "stat-subtext", paste(stats$wins, "W /", stats$losses, "L /", stats$draws, "D"))
        ),
        
        # Card 2: Overall Win Rate
        div(
          class = "stat-box",
          div(class = "stat-label", "🎯 Win Rate"),
          div(class = "stat-value", paste0(stats$win_rate, "%")),
          div(class = "stat-subtext", paste("Avg moves:", stats$avg_moves))
        ),
        
        # Card 3: Best Opening (min 2 games)
        div(
          class = "stat-box",
          div(class = "stat-label", "🏆 Best Opening"),
          div(class = "stat-value", style = "font-size: 20px; color: #F5B942;", op_info$best_opening),
          div(class = "stat-subtext", op_info$best_status)
        ),
        
        # Card 4: Current Form
        div(
          class = "stat-box",
          div(class = "stat-label", "🔥 Current Form (Last 5)"),
          div(
            class = "stat-value",
            style = "font-size: 20px;",
            span(class = paste("pill", form_info$badge_class), form_info$label)
          ),
          div(class = "stat-subtext", form_info$desc)
        ),
        
        # Card 5: Net Rating Change
        div(
          class = "stat-box",
          div(class = "stat-label", "📈 Rating Shift"),
          div(class = "stat-value", style = paste0("color: ", rating_color, ";"), rating_sign),
          div(
            class = "stat-subtext",
            if (nrow(trend) >= 1) paste("Start:", trend$Rating[1], "→ Current:", trend$Rating[nrow(trend)]) else ""
          )
        )
      ),
      
      # Row 2: Deep Dive Cards (White vs Black Performance & Opening Highlights)
      fluidRow(
        column(
          width = 6,
          div(
            class = "chess-card",
            div(class = "chess-card-title", "⚪ vs ⚫ Performance by Color"),
            tags$table(
              class = "custom-table",
              tags$thead(
                tags$tr(
                  tags$th("Color"),
                  tags$th("Games"),
                  tags$th("Record (W-L-D)"),
                  tags$th("Win %")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("⚪ White"),
                  tags$td(color_info$White$total),
                  tags$td(paste(color_info$White$wins, "-", color_info$White$losses, "-", color_info$White$draws)),
                  tags$td(tags$strong(paste0(color_info$White$win_rate, "%")))
                ),
                tags$tr(
                  tags$td("⚫ Black"),
                  tags$td(color_info$Black$total),
                  tags$td(paste(color_info$Black$wins, "-", color_info$Black$losses, "-", color_info$Black$draws)),
                  tags$td(tags$strong(paste0(color_info$Black$win_rate, "%")))
                )
              )
            )
          )
        ),
        column(
          width = 6,
          div(
            class = "chess-card",
            div(class = "chess-card-title", "📖 Most Played vs Best Opening"),
            p(
              tags$strong("Most Frequent Opening: "),
              span(style = "color: #B08D57;", op_info$most_played),
              paste0(" (", op_info$most_played_count, " games played)")
            ),
            p(
              tags$strong("Highest Win-Rate Opening: "),
              span(style = "color: #F5B942;", op_info$best_opening),
              paste0(" (", op_info$best_win_rate, "% win rate)")
            ),
            tags$hr(style = "border-color: #262A33;"),
            p(
              style = "color: #9CA3AF; font-size: 13px;",
              "💡 Tip: Maintain opening consistency! The 'Best Opening' metric filters for at least 2 games to avoid small sample size bias."
            )
          )
        )
      )
    )
  })
}

# ------------------------------------------------------------------------------
# APP LAUNCHER
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
