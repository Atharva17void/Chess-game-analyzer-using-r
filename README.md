# ♟️ Chess Performance Analyzer

An interactive chess performance and rating trend analysis dashboard built in **R** and **Shiny**.

[![Live Demo](https://img.shields.io/badge/Live%20Demo-GitHub%20Pages-brightgreen?style=for-the-badge&logo=github)](https://atharva17void.github.io/Chess-game-analyzer-using-r/)
[![Deploy Status](https://github.com/Atharva17void/Chess-game-analyzer-using-r/actions/workflows/deploy.yml/badge.svg)](https://github.com/Atharva17void/Chess-game-analyzer-using-r/actions/workflows/deploy.yml)
[![R](https://img.shields.io/badge/Built%20With-R%20%2B%20Shiny-blue?style=for-the-badge&logo=r)](https://shiny.posit.co/)

---

## 🌐 Launch the App Directly

### Option 1: View in Browser (GitHub Pages)
You can directly interact with the live web application in your browser powered by **WebAssembly (Shinylive / webR)** without installing anything:

👉 **[Launch Chess Performance Analyzer](https://atharva17void.github.io/Chess-game-analyzer-using-r/)**

*(Note: On first load, Shinylive downloads the in-browser R runtime which takes a few seconds).*

---

### Option 2: Run directly from GitHub in R / RStudio
Anyone with R installed can run this app with a single command straight from this repository:

```r
# Install Shiny if you haven't already
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")

# Run directly from this GitHub repository
shiny::runGitHub(repo = "Chess-game-analyzer-using-r", username = "Atharva17void")
```

---

## 🚀 Key Features

- **📊 Comprehensive Game Analytics**:
  - Win/Loss/Draw tracking and percentage calculations.
  - Average moves per game and breakdown by piece color (White vs. Black).
  - Most played opening and best performing opening identification.
- **📈 Rating Progression & Trends**:
  - Interactive visualization of rating development over time.
  - Form classification engine based on recent match sequences.
- **➕ Interactive Match Logging**:
  - Add new matches dynamically via a responsive input panel.
  - Real-time reactivity updates all charts, summaries, and leaderboard metrics instantly.
- **🎨 Modern Dark UI / UX**:
  - Built with custom CSS featuring glassmorphism, responsive cards, and vibrant visual hierarchy.

---

## 📂 Project Structure

```text
├── app.R                     # Main Shiny application (UI & reactive server)
├── chess_backend.R           # Core statistics and data logic (Base R)
├── test_backend.R            # Validation and unit test suite
├── www/
│   └── style.css             # Custom modern dark theme styling
├── .github/
│   └── workflows/
│       └── deploy.yml        # Shinylive GitHub Pages automated deployment
├── DESCRIPTION               # R package metadata & dependencies
└── README.md                 # Project documentation
```

---

## 🛠️ Local Development

To clone and run locally:

```bash
git clone https://github.com/Atharva17void/Chess-game-analyzer-using-r.git
cd Chess-game-analyzer-using-r
```

In R or RStudio:

```r
shiny::runApp()
```

---

## 👤 Author

- **Atharva Palve** ([@Atharva17void](https://github.com/Atharva17void))
