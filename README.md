# Dynamic Pitch Selection Capstone Project

## Overview

This capstone project builds a dynamic pitch selection model using pitch-by-pitch Statcast data to evaluate and optimize pitch decisions for MLB pitcher Nathan Eovaldi during the 2024 season. The model accounts for game context such as count, outs, baserunners, and outcomes to predict the optimal pitch type and location.

## Project Structure

The project is structured with both Python and R scripts for different stages of the pipeline—from data collection to model training and dynamic scenario simulation.

### `pitch_by_pitch_collection.py`
- **Language**: Python
- **Purpose**: Uses the `pybaseball` package to collect Statcast data from every 2024 MLB game.
- **Output**: Raw pitch-by-pitch data files for all games.

### `combine_rangers_data.R`
- **Language**: R
- **Purpose**: Filters and processes the data collected in Python to include only Texas Rangers games, then consolidates it into a single CSV file.
- **Output**: Cleaned dataset used for modeling and analysis.

### `heatmap_pitcher_pso.R`
- **Language**: R
- **Purpose**: Builds a Random Forest model to predict outcomes based on pitch type and zone for Nathan Eovaldi.
- **Features**: Includes pitch type, location, count, base state, and more.
- **Output**: Model accuracy and feature importance visualizations.

### `xgboost.R`
- **Language**: R
- **Purpose**: Trains an XGBoost model on the same dataset and features as the Random Forest to compare performance.

### `heatmaps.R`
- **Language**: R
- **Purpose**: Performs exploratory data analysis by generating heatmaps of pitch types, locations, and outcomes.
- **Visuals**: Pitch frequency, pitch success by zone, pitch tendencies.

### `dynamic_pitcher.R`
- **Language**: R
- **Purpose**: Simulates live game scenarios where the user can input game state parameters (count, outs, baserunners, previous pitch, etc.).
- **Function**: Dynamically recommends the best pitch type and location based on trained models.
- **Goal**: Emulate a pitcher's real-time decision-making process.

## Getting Started

```bash
# === Install Python Dependencies ===
pip install pybaseball pandas

# === Install R Packages ===
# Run this in your R console
install.packages(c("tidyverse", "randomForest", "xgboost", "ggplot2", "dplyr"))

# === Run Data Collection ===
# Run this in your terminal or Python environment
python pitch_by_pitch_collection.py

# === Clean and Merge Data ===
# Run this in your R console
source("combine_rangers_data.R")

# === Train Models ===
source("heatmap_pitcher_pso.R")
source("xgboost.R")

# === Visualize Pitch Trends ===
source("heatmaps.R")

# === Simulate Pitching Scenarios ===
source("dynamic_pitcher.R")
```
Landon Myhill

github.com/landon-myhill
