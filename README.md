# Energy Consumption Analysis
> R Shiny · Random Forest · SVM · Linear Regression · Interactive Visualisation

## Overview
A secure R Shiny web application for analysing global energy consumption trends.
Supports dynamic data uploads, 15+ interactive visualisations, and multi-model
ML forecasting with sub-second reactive updates.

## Key Features
- Authentication system with secure login
- Dynamic CSV/Excel upload with real-time processing
- 15+ interactive charts (3D plots, heatmaps, time-series, forecasting)
- ML models: Random Forest, SVM, Linear Regression
- Evaluation metrics: RMSE, MAE, R² across all models
- Multi-year energy consumption forecasting

## Model Results
| Model           | RMSE   | MAE    | R²    |
|----------------|--------|--------|-------|
| Random Forest   | 4.3    | 3.1    | 0.91  |
| SVM             | 5.8    | 4.2    | 0.87  |
| Linear Regression | 7.2  | 5.6    | 0.81  |


## Tech Stack
R · Shiny · ggplot2 · plotly · caret · randomForest · e1071

## How to Run
1. Clone the repo
2. Install packages: install.packages(c("shiny","ggplot2","plotly","caret"))
3. Run: shiny::runApp("finaldvrproject.R")

## Project Report
Full analysis and methodology: [dvr project report](dvr%20project%5B1%5D%20(1).pdf)

## Dataset
eca.xlsx — Global energy consumption data, 2000–2023, 50+ countries
