# ==============================================================================
# SYMBIOSIS COLLEGE OF ARTS AND COMMERCE
# FINTECH ASSIGNMENT: BUSINESS STATISTICS WITH R
# TOPIC: STOCK DATA ANALYSIS 
# Srushti C. Chaudhari (9020) - MA ECONOMICS PART II
#===============================================================================
#
# Stocks Selected:
# 1. Vodafone Idea Limited                   - IDEA.NS
# 2. Bharti Airtel Limited                   - BHARTIARTL.NS
# 3. Bharat Petroleum Corporation Limited    - BPCL.NS
# 4. Oil and Natural Gas Corporation Limited - ONGC.NS
# 5. Tata Consumer Products Limited          - TATACONSUM.NS
# 6. Hindustan Unilever Limited              - HINDUNILVR.NS
#
# Data Source: Yahoo Finance
# Exchange: NSE (National Stock Exchange of India)
# Period: Approximately last 5 years
#
#===============================================================================

#===============================================================================
# Section 0: INSTALL AND LOAD REQUIRED PACKAGES
# ==============================================================================

# Install the packages required for downloading, cleaning, analysing and
# visualising the stock data.
#
# NOTE:
# These install.packages() commands only need to be run when the packages
# are not already installed on the computer.

install.packages("quantmod")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("scales")

# Load the installed Packages

library(quantmod)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#===============================================================================
# Section 1: DATA ACQUISITION
# ==============================================================================

#-------------------------------------------------------------------------------
# 1.1 Define the study period
#-------------------------------------------------------------------------------

# Sys.Date() automatically returns the current date.
# Therefore, the analysis period automatically updates whenever the script
# is run in the future.

end_date <- Sys.Date()

# 365 x 5 gives approximately five years.
# This provides roughly five years of historical daily observations.

start_date <- end_date - 365*5

#-------------------------------------------------------------------------------
# 1.2 Define the six NSE stock tickers
#-------------------------------------------------------------------------------

# These are Yahoo Finance ticker symbols for stocks listed on NSE.
# The ".NS" suffix indicates the National Stock Exchange of India.


stocks <- c(
  "IDEA.NS",
  "BHARTIARTL.NS",
  "BPCL.NS",
  "ONGC.NS",
  "TATACONSUM.NS",
  "HINDUNILVR.NS"
)

#-------------------------------------------------------------------------------
# 1.3 Download historical stock data
#-------------------------------------------------------------------------------

# getSymbols() downloads historical OHLCV data from Yahoo Finance.
#
# OHLCV means:
# O = Open price
# H = High price
# L = Low price
# C = Closing price
# V = Trading Volume
#
# auto.assign = FALSE ensures that the downloaded data are returned directly
# and stored in the object specified below.


IDEA <- getSymbols(
  "IDEA.NS",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

BHARTIARTL <- getSymbols(
  "BHARTIARTL.NS",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

BPCL <- getSymbols(
  "BPCL.NS",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

ONGC <- getSymbols(
  "ONGC.NS",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

TATACONSUM <- getSymbols(
  "TATACONSUM.NS",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

HINDUNILVR <- getSymbols(
  "HINDUNILVR.NS",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

#-------------------------------------------------------------------------------
# 1.4 Check the downloaded data
#-------------------------------------------------------------------------------


# head() displays the first few observations.
# This helps confirm that the data have been downloaded correctly.

# Displaying the first few observations

head(IDEA)
head(BHARTIARTL)
head(BPCL)
head(ONGC)
head(TATACONSUM)
head(HINDUNILVR)

# str() displays the structure of the data object.
# quantmod stores the downloaded stock data as an xts object.

str(IDEA)

# nrow() counts the number of observations (trading days).

nrow(IDEA)
nrow(BHARTIARTL)
nrow(BPCL)
nrow(ONGC)
nrow(TATACONSUM)
nrow(HINDUNILVR)

#===============================================================================
# SECTION 2: DATA CLEANING AND DAILY RETURNS
#===============================================================================

#-------------------------------------------------------------------------------
# 2.1 Extracting Closing Price
#-------------------------------------------------------------------------------

# Cl() extracts only the Closing Price column from the OHLCV data.
# Closing prices are used for calculating daily returns.

idea_close <- Cl(IDEA)
bhartiartl_close <- Cl(BHARTIARTL)
bpcl_close <- Cl(BPCL)
ongc_close <- Cl(ONGC)
tataconsum_close <- Cl(TATACONSUM)
hindunilvr_close <- Cl(HINDUNILVR)

#-------------------------------------------------------------------------------
# 2.2 Calculating Daily Returns
#-------------------------------------------------------------------------------

# dailyReturn() calculates the return from one trading day to the next.
#
# type = "arithmetic" calculates:
#
# Return = [(Today's Close - Previous Close) / Previous Close] x 100
#
# Multiplication by 100 converts the return into percentage terms.


idea_return <- dailyReturn(
  idea_close,
  type = "arithmetic"
) * 100

bhartiartl_return <- dailyReturn(
  bhartiartl_close,
  type = "arithmetic"
) * 100

bpcl_return <- dailyReturn(
  bpcl_close,
  type = "arithmetic"
) * 100

ongc_return <- dailyReturn(
  ongc_close,
  type = "arithmetic"
) * 100

tataconsum_return <- dailyReturn(
  tataconsum_close,
  type = "arithmetic"
) * 100

hindunilvr_return <- dailyReturn(
  hindunilvr_close,
  type = "arithmetic"
) * 100

#-------------------------------------------------------------------------------
# 2.3 Removing missing values
#-------------------------------------------------------------------------------

# The first observation in a daily-return series cannot have a return because
# there is no previous trading day's price for comparison.
#
# na.omit() removes missing (NA) observations before statistical analysis.


idea_return <- na.omit(idea_return)
bhartiartl_return <- na.omit(bhartiartl_return)
bpcl_return <- na.omit(bpcl_return)
ongc_return <- na.omit(ongc_return)
tataconsum_return <- na.omit(tataconsum_return)
hindunilvr_return <- na.omit(hindunilvr_return)


# Check the first few calculated returns.
head(idea_return)

#===============================================================================
# SECTION 3: DESCRIPTIVE STATISTICS - CLOSE PRICE
#===============================================================================

#-------------------------------------------------------------------------------
# 3.1 Function to calculate descriptive statistics
#-------------------------------------------------------------------------------

# This function calculates six basic descriptive statistics:
#
# Mean                 = average value
# Median               = middle value
# Standard Deviation   = average dispersion around the mean
# Variance             = squared measure of dispersion
# Minimum              = lowest observed value
# Maximum              = highest observed value


descriptive_stats <- function(x) {
  c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Standard_Deviation = sd(x, na.rm = TRUE),
    Variance = var(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE)
  )
}


#-------------------------------------------------------------------------------
# 3.2 Descriptive Statistics for closing prices
#-------------------------------------------------------------------------------


# rbind() combines the statistics for all six stocks into one table.
# as.numeric() converts the xts price series into numerical values.


close_statistics <- rbind(
  Vodafone_Idea = descriptive_stats(as.numeric(idea_close)),
  Bharti_Airtel = descriptive_stats(as.numeric(bhartiartl_close)),
  BPCL = descriptive_stats(as.numeric(bpcl_close)),
  ONGC = descriptive_stats(as.numeric(ongc_close)),
  TATACONSUM = descriptive_stats(as.numeric(tataconsum_close)),
  Hindunilvr = descriptive_stats(as.numeric(hindunilvr_close))
)

# Display the descriptive statistics table.

close_statistics

# scipen controls scientific notation in R.
# A high value makes R display large/small numbers in ordinary decimal form
# where possible.

options(scipen = 999)

#===============================================================================
# SECTION 4: DESCRIPTIVE STATISTICS - DAILY RETURNS
#===============================================================================

# The same six descriptive statistics are calculated for daily returns.
# This allows comparison of average performance and return volatility.

#-------------------------------------------------------------------------------
# 4.1 Descriptive statistics for Daily Returns
#-------------------------------------------------------------------------------

return_statistics <- rbind(
  Vodafone_Idea = descriptive_stats(as.numeric(idea_return)),
  Bharti_Airtel = descriptive_stats(as.numeric(bhartiartl_return)),
  BPCL = descriptive_stats(as.numeric(bpcl_return)),
  ONGC = descriptive_stats(as.numeric(ongc_return)),
  TATACONSUM = descriptive_stats(as.numeric(tataconsum_return)),
  Hindunilvr = descriptive_stats(as.numeric(hindunilvr_return))
)

# Displaying the return statistics 
return_statistics

#===============================================================================
# SECTION 5: LINE PLOTS OF CLOSING PRICES
#===============================================================================

# Line plots show how each stock's closing price changed over the study period.
# Date is placed on the X-axis and closing price on the Y-axis.

#-------------------------------------------------------------------------------
# 5.1 Vodafone Idea Limited - Line Plot
# ------------------------------------------------------------------------------

ggplot(
  data = data.frame(
    Date = index(idea_close),
    Close = as.numeric(idea_close)
  ),
  aes(x = Date, y = Close)
) +
  geom_line(colour = "yellow", linewidth = 0.8) +
  labs(
    title = "Vodafone Idea Limited - Closing Price",
    x = "Date",
    y = "Closing Price (INR)"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 5.2 Bharti Airtel Limited - Line Plot
#-------------------------------------------------------------------------------

ggplot(
  data = data.frame(
    Date = index(bhartiartl_close),
    Close = as.numeric(bhartiartl_close)
  ),
  aes(x = Date, y = Close)
) +
  geom_line(colour = "darkred", linewidth = 0.8) +
  labs(
    title = "Bharti Airtel Limited - Closing Price",
    x = "Date",
    y = "Closing Price (INR)"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 5.3 Bharat Petroleum Corporation Limited - Line Plot
#-------------------------------------------------------------------------------

ggplot(
  data = data.frame(
    Date = index(bpcl_close),
    Close = as.numeric(bpcl_close)
  ),
  aes(x = Date, y = Close)
) +
  geom_line(colour = "salmon", linewidth = 0.8) +
  labs(
    title = "Bharat Petroleum Corporation Limited - Closing Price",
    x = "Date",
    y = "Closing Price (INR)"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 5.4 ONGC  - Line Plot
#-------------------------------------------------------------------------------

ggplot(
  data = data.frame(
    Date = index(ongc_close),
    Close = as.numeric(ongc_close)
  ),
  aes(x = Date, y = Close)
) +
  geom_line(colour = "purple", linewidth = 0.8) +
  labs(
    title = "Oil and Natural Gas Corporation - Closing Price",
    x = "Date",
    y = "Closing Price (INR)"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 5.5 Tata Consumer Products Limited - Line Plot
#-------------------------------------------------------------------------------

ggplot(
  data = data.frame(
    Date = index(tataconsum_close),
    Close = as.numeric(tataconsum_close)
  ),
  aes(x = Date, y = Close)
) +
  geom_line(colour = "springgreen", linewidth = 0.8) +
  labs(
    title = "Tata Consumer Products Limited - Closing Price",
    x = "Date",
    y = "Closing Price (INR)"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 5.6 Hindustan Unilever Limited - Line Plot
#-------------------------------------------------------------------------------

ggplot(
  data = data.frame(
    Date = index(hindunilvr_close),
    Close = as.numeric(hindunilvr_close)
  ),
  aes(x = Date, y = Close)
) +
  geom_line(colour = "darkgrey", linewidth = 0.8) +
  labs(
    title = "Hindustan Unilever Limited - Closing Price",
    x = "Date",
    y = "Closing Price (INR)"
  ) + 
  theme_minimal()

#===============================================================================
# SECTION 6: HISTOGRAMS OF DAILY RETURNS
#===============================================================================

# Histograms show the frequency distribution of daily returns.
#
# Returns concentrated around zero indicate that most daily price changes
# are relatively small.
#
# Observations far from zero represent unusually large daily gains or losses.

#-------------------------------------------------------------------------------
# 6.1 Vodafone Idea Limited - Histogram
#-------------------------------------------------------------------------------

ggplot(
  data.frame(Return = as.numeric(idea_return)),
  aes(x = Return)
) + 
  geom_histogram(
    bins = 50,
    fill = "yellow",
    color = "white"
  ) + 
  labs(
    title = "Vodafone Idea Limited - Distribution of Daily Returns",
    x = "Daily Returns (%)",
    y = "Frequency"
  ) + 
  theme_minimal()


#-------------------------------------------------------------------------------
# Bharti Airtel Limited - Histogram
#-------------------------------------------------------------------------------

ggplot(
  data.frame(Return = as.numeric(bhartiartl_return)),
  aes(x = Return)
) + 
  geom_histogram(
    bins = 50,
    fill = "darkred",
    color = "white"
  ) + 
  labs(
    title = "Bharti Airtel Limited - Distribution of Daily Returns",
    x = "Daily Returns (%)",
    y = "Frequency"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 6.3 Bharat Petroluem Corporation Limited - Histogram
#-------------------------------------------------------------------------------

ggplot(
  data.frame(Return = as.numeric(bpcl_return)),
  aes(x = Return)
) + 
  geom_histogram(
    bins = 50,
    fill = "salmon",
    color = "white"
  ) + 
  labs(
    title = "Bharat Petroluem Corporation Limited - Distribution of Daily Returns",
    x = "Daily Returns (%)",
    y = "Frequency"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 6.4 Oil and Natural Gas Corporation Ltd - Histogram
#-------------------------------------------------------------------------------

ggplot(
  data.frame(Return = as.numeric(ongc_return)),
  aes(x = Return)
) + 
  geom_histogram(
    bins = 50,
    fill = "purple",
    color = "white"
  ) + 
  labs(
    title = "Oil and Natural Gas Corporation Ltd - Distribution of Daily Returns",
    x = "Daily Returns (%)",
    y = "Frequency"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 6.5 Tata Consumer Products Ltd - Histogram
#-------------------------------------------------------------------------------

ggplot(
  data.frame(Return = as.numeric(tataconsum_return)),
  aes(x = Return)
) + 
  geom_histogram(
    bins = 50,
    fill = "springgreen",
    color = "white"
  ) + 
  labs(
    title = "Tata Consumer Products Ltd - Distribution of Daily Returns",
    x = "Daily Returns (%)",
    y = "Frequency"
  ) + 
  theme_minimal()

#-------------------------------------------------------------------------------
# 6.6 Hindustan Unilever Ltd - Histogram
#-------------------------------------------------------------------------------

ggplot(
  data.frame(Return = as.numeric(hindunilvr_return)),
  aes(x = Return)
) + 
  geom_histogram(
    bins = 50,
    fill = "darkgrey",
    color = "white"
  ) + 
  labs(
    title = "Hindustan Unilever Ltd - Distribution of Daily Returns",
    x = "Daily Returns (%)",
    y = "Frequency"
  ) + 
  theme_minimal()


#===============================================================================
#SECTION 7: SIMPLE LINEAR REGRESSION
#===============================================================================

# The regression examines whether BPCL daily returns are statistically
# associated with ONGC daily returns.
#
# Dependent Variable (Y)   = ONGC daily returns
# Independent Variable (X) = BPCL daily returns
#
# Model:
#
# ONGC Return = Intercept + Slope(BPCL Return) + Error
#
# The slope measures the expected change in ONGC's return associated with
# a one-percentage-point change in BPCL's return.

#-------------------------------------------------------------------------------
# 7.1 Create a common data frame
#-------------------------------------------------------------------------------


# merge() combines the two return series according to their trading dates.
# join = "inner" keeps only dates available for BOTH stocks.
#
# This is important because regression requires observations for both X and Y
# on the same dates.


regression_data <- merge(
  ongc_return,
  bpcl_return,
  join = "inner"
)


# Rename the columns
colnames(regression_data) <- c(
  "ONGC_Return",
  "BPCL_Return"
)

# Convert the xts object into an ordinary data frame.

regression_data <- data.frame(
  Date = index(regression_data),
  coredata(regression_data)
)  

# Remove any remaining missing values

regression_data <- na.omit(regression_data)

# Check the first few observations. 

head(regression_data)


#-------------------------------------------------------------------------------
# 7.2 Run the Simple Linear Regression
#-------------------------------------------------------------------------------

# lm() estimates a linear regression model using the least squares method.
#
# ONGC_Return ~ BPCL_Return means:
#
# ONGC_Return is the dependent variable.
# BPCL_Return is the independent variable.


regression_data$ONGC_Return <- as.numeric(regression_data$ONGC_Return)

regression_data$BPCL_Return <- as.numeric(regression_data$BPCL_Return)


regression_model <- lm(
  ONGC_Return ~ BPCL_Return,
  data = regression_data
)

#-------------------------------------------------------------------------------
# 7.3 Display the regression results
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 7.3 Display the complete regression results
# ------------------------------------------------------------------------------

# summary() provides:
# - coefficient estimates
# - standard errors
# - t-statistics
# - p-values
# - residual standard error
# - R-squared
# - adjusted R-squared
# - F-statistic 

summary(regression_model)

#-------------------------------------------------------------------------------
# 7.4 Regression Coefficients
#-------------------------------------------------------------------------------

# coef() extracts the estimated intercept and slope.

coef(regression_model)

#-------------------------------------------------------------------------------
# 7.5 Confidence intervals for coefficients
#-------------------------------------------------------------------------------

# confint() provides the 95% confidence intervals for the estimated
# regression coefficients.

confint(regression_model)

#-------------------------------------------------------------------------------
# 7.6 R-Squared
#-------------------------------------------------------------------------------

# R-squared measures the proportion of variation in ONGC returns that is
# statistically explained by BPCL returns in this simple regression model.

summary(regression_model)$r.squared

#-------------------------------------------------------------------------------
# 7.7 Adjusted R-Squared
#-------------------------------------------------------------------------------

# Adjusted R-squared adjusts the R-squared value for the number of explanatory
# variables in the model. It is particularly useful when comparing models
# containing different numbers of independent variables.

summary(regression_model)$adj.r.squared

# ------------------------------------------------------------------------------
# 7.8 Display the regression equation
# ------------------------------------------------------------------------------

# Extract the estimated intercept and slope from the model.

intercept <- coef(regression_model)[1]
slope <- coef(regression_model)[2]

# Print the estimated regression equation in a readable format.

cat(
  "Regression Equation:\n",
  "ONGC Return =",
  round(intercept, 4),
  " + (",
  round(slope, 4),
  " * BPCL Return)\n"
)

#===============================================================================
# SECTION 8: SCATTER PLOT WITH REGRESSION LINE
#===============================================================================

# The scatter plot visually shows the relationship between the two stocks.
#
# X-axis = BPCL daily returns
# Y-axis = ONGC daily returns
#
# Each point represents one common trading day.
#
# The fitted line represents the estimated linear relationship between
# BPCL and ONGC returns.
#
# The shaded region represents the confidence interval around the fitted line.


regression_xts <- merge(
  ongc_return,
  bpcl_return,
  join = "inner"
)

# Convert the xts object into a normal data frame
# and assign the correct column names

regression_data <- data.frame(
  Date = index(regression_xts),
  ONGC_Return = as.numeric(regression_xts[, 1]),
  BPCL_Return = as.numeric(regression_xts[, 2])
)

# Remove any missing values

regression_data <- na.omit(regression_data)


# Check the first few observations

head(regression_data)

#  Scatter Plot

ggplot(
  regression_data,
  aes(
    x = BPCL_Return,
    y = ONGC_Return
  )
) +
  geom_point(
    color = "steelblue",
    alpha = 0.5,
    size = 1.5
  )+
  geom_smooth(
    method = "lm",
    se = TRUE,
    color ="red",
    fill ="lightpink"
  ) + 
  labs(
    title = "ONGC Returns vs BPCL Returns",
    subtitle = "Scatter Plot with Fitted Regression Line",
    x = "BPCL Daily Returns (%)",
    y = "ONGC Daily Returns (%)"
  ) +
  theme_minimal()


# ==============================================================================
# SECTION 9: CROSS-TABULATION / CATEGORICAL ANALYSIS
# ==============================================================================

# Vodafone Idea and Bharti Airtel are used for the cross-tabulation.
#
# Rows    = Vodafone Idea return category
# Columns = Bharti Airtel return category
#
# Each daily return is classified as:
#
# Positive = return > 0
# Negative = return < 0
# Flat     = return = 0


# ------------------------------------------------------------------------------
# 9.1 Combine Vodafone Idea and Bharti Airtel returns
# ------------------------------------------------------------------------------

crosstab_xts <- merge(
  idea_return,
  bhartiartl_return,
  join = "inner"
)

# ------------------------------------------------------------------------------
# 9.2 Convert to a data frame
# ------------------------------------------------------------------------------

crosstab_data <- data.frame(
  Date = index(crosstab_xts),
  Vodafone_Idea_Return = as.numeric(crosstab_xts[, 1]),
  Bharti_Airtel_Return = as.numeric(crosstab_xts[, 2])
)

# Remove missing values
crosstab_data <- na.omit(crosstab_data)


# ------------------------------------------------------------------------------
# 9.3 Create categorical variables
# ------------------------------------------------------------------------------
# Vodafone Idea:
#
# ifelse() checks the return and assigns a category:
# Positive if return > 0
# Negative if return < 0
# Flat if return = 0

crosstab_data$Vodafone_Idea_Category <- ifelse(
  crosstab_data$Vodafone_Idea_Return > 0,
  "Positive",
  ifelse(
    crosstab_data$Vodafone_Idea_Return < 0,
    "Negative",
    "Flat"
  )
)

# Bharti Airtel Ltd:
# Positive = return > 0
# Negative = return < 0
# Flat     = return = 0

crosstab_data$Bharti_Airtel_Category <- ifelse(
  crosstab_data$Bharti_Airtel_Return > 0,
  "Positive",
  ifelse(
    crosstab_data$Bharti_Airtel_Return < 0,
    "Negative",
    "Flat"
  )
)

# ------------------------------------------------------------------------------
# 9.4 Convert categories into factors
# ------------------------------------------------------------------------------

# factor() converts the categorical variables into factors.
#
# Specifying levels ensures that the categories appear in the desired order:
# Positive -> Negative -> Flat


crosstab_data$Vodafone_Idea_Category <- factor(
  crosstab_data$Vodafone_Idea_Category,
  levels = c("Positive", "Negative", "Flat")
)

crosstab_data$Bharti_Airtel_Category <- factor(
  crosstab_data$Bharti_Airtel_Category,
  levels = c("Positive", "Negative", "Flat")
)


# ==============================================================================
# SECTION 10: FREQUENCY CROSS-TABULATION
# ==============================================================================

# table() creates the frequency cross-tabulation.
#
# Each cell shows the number of trading days on which the two stocks
# simultaneously belonged to the corresponding return categories.


frequency_table <- table(
  Vodafone_Idea = crosstab_data$Vodafone_Idea_Category,
  Bharti_Airtel = crosstab_data$Bharti_Airtel_Category
)


# Display the table with clear labels
frequency_table

# prop.table(..., margin = 1) calculates percentages across rows.
#
# Therefore, each row adds up to approximately 100%.
#
# Row percentages answer questions such as:
# "When Vodafone Idea was positive, how often was Bharti Airtel positive?"

row_percentages <- prop.table(
  frequency_table,
  margin = 1
) * 100

# Display row percentages
round(row_percentages, 2)

# Re-checking the row percentages

row_percentages <- prop.table(frequency_table, margin = 1) * 100

# prop.table(..., margin = 2) calculates percentages down columns.
#
# Therefore, each column adds up to approximately 100%.
#
# Column percentages answer questions such as:
# "When Bharti Airtel was negative, how often was Vodafone Idea negative?"


column_percentages <- prop.table(frequency_table, margin = 2) * 100

round(column_percentages, 2)

#===============================================================================
# SECTION 13: TOTAL PERCENTAGES
#===============================================================================

# prop.table() without specifying a margin calculates each cell's percentage
# of the entire sample.
#
# All cells together add up to 100%.


total_percentages <- prop.table(frequency_table) * 100

#Display total percentages

round(total_percentages, 2)


#===============================================================================
# SECTION 14: NUMBER AND PERCENTAGE OF BOTH STOCKS HAVING POSITIVE 
# RETURNS ON THE SAME DAY
#===============================================================================

# This section specifically identifies trading days when BOTH Vodafone Idea
# and Bharti Airtel recorded positive daily returns.

both_positive <- sum(
  crosstab_data$Vodafone_Idea_Category == "Positive" &
    crosstab_data$Bharti_Airtel_Category == "Positive"
)

# Count the total number of common trading-day observations.

total_observations <- nrow(crosstab_data)

# Calculate the percentage of all observations where both stocks were positive.

both_positive_percentage <- (
  both_positive / total_observations
) * 100

# Display the number of days.

cat(
  "Number of days when both Vodafone Idea Ltd and Bharti Airtel Ltd had 
  positive returns:",
  both_positive,
  "\n"
)

# Display the number of days.

cat(
  "Percentage of days when both Vodafone Idea Ltd and Bharti Airtel Ltd
  had positive returns:",
  round(both_positive_percentage, 2),
  "%\n"
)

#===============================================================================
# END OF ASSIGNMENT CODE
#===============================================================================

cat(
  "\n=============================================\n",
  "ASSIGNMENT ANALYSIS COMPLETED\n",
  "==============================================\n"
)



