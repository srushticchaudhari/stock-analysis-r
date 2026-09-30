# stock-analysis-r
Stock return analysis and regression using R
Objectives :
The assignment aims to build foundational skills in data manipulation, exploratory data analysis, and statistical modeling using R. To learn to handle real-world financial data—specifically OHLC (Open, High, Low, Close) prices and trading Volume—for six selected stocks over a five-year period.

Data Source: NSE/Yahoo Finance

Tools and Methods used: R · quantmod/tidyquant for data · descriptive stats · linear regression (lm) · cross-tabulation with row/column/total percentages · ggplot2 for visualizations (line plots, histograms, scatter plot with regression line) and AI (to assist in writing R code)

Key findings:
- 6 stocks analyzed (Reliance, Infosys, TCS, HDFC Bank, ITC, SBI) using 5+ years of daily data
- Descriptive stats: Daily returns average near 0% with 1.3–1.6% volatility across all stocks; SBI showed the strongest long-term price uptrend
- Regression (TCS ~ Infosys): No significant relationship (p = 0.32, R² ≈ 0.0008) — returns move independently despite same sector
- Cross-tab (Reliance vs HDFC): Moderate co-movement — both positive together 31.6% of days, likely driven by broader market trends

