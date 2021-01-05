# Note - This code was written in an Rmarkdown document. 
# Some markdown specific utilities may not run
# All Graphs and Tables are created in the script

# knitr::opts_chunk$set(echo = FALSE, include = TRUE, warning = FALSE, error = FALSE, message = FALSE)
library(tidyverse)
library(tidycensus)
library(lubridate)
library(urca) # unit root testing
library(forecast)
library(vars)
census_api_key('...') # Census Key (Redacted)
fredr::fredr_set_key('...') # Fred API Key (Redacted)


# Setup --------------------------------------------------------------------------------
# Load Data
# Poverty Data
Poverty_Data <- readxl::read_excel(
  'C:\\Users\\aadus\\OneDrive\\Documents\\GW\\Fall 2020\\Time Series Econometrics\\Paper\\PovertyTable_US.xlsx', 
  skip = 3)

# Define the 'date' column as an actual date object
Poverty_Data$date <- rev(seq(as.Date('1959-01-01'), as.Date('2019-01-01'), by = 'year'))



# GDP per Capita
GDP_Data <- fredr::fredr(
  series_id = 'A939RX0Q048SBEA',
  observation_start = as.Date('1967-01-01'),
  observation_end = as.Date('2019-01-01')) %>% 
  dplyr::select(-series_id) %>% 
  mutate(date = as.Date(date)) %>% 
  rename(GDP.per.Capita = value)
# Gini Ratio
# NOTE: Should I use Gini ratio by household or by family?
Gini_Data <- fredr::fredr(
  series_id = 'GINIALLRH') %>% 
  dplyr::select(-series_id) %>% 
  mutate(date = as.Date(date)) %>% 
  rename(Gini.Ratio = value)

# Unemployment - Convert to annaul series by taking the yearly mean
UNRATE <- fredr::fredr(
  series_id = 'UNRATE') %>% 
  dplyr::select(-series_id) %>% 
  mutate(date = as.Date(date)) %>% 
  rename(UNRATE = value) %>% 
  mutate(date = year(date)) %>% 
  group_by(date) %>% 
  summarise(UNRATE = mean(UNRATE)) %>% 
  ungroup() %>% 
  mutate(date = seq(as.Date('1948-01-01'), as.Date('2020-01-01'), by = 'year'))


# Merge Datasets
TriangleData <- Poverty_Data %>% 
  inner_join(GDP_Data, by = 'date') %>% 
  inner_join(Gini_Data, by = 'date') %>% 
  inner_join(UNRATE, by = 'date') %>% 
  arrange(date)

# Create TS Objects
GDP.TS <- ts(TriangleData$GDP.per.Capita, start = c(1967, 1), frequency = 1)
# Take Log of GDP 
GDP.TS_Log <- log(GDP.TS)
# Gini
Gini.TS <- ts(TriangleData$Gini.Ratio, start = c(1967, 1), frequency = 1)
# Poverty
NationalPov.TS <- ts(TriangleData$PovertyRate, start = c(1967, 1), frequency = 1)
# Unemployment
UNRATE.TS <- ts(TriangleData$UNRATE, start = c(1967, 1), frequency = 1)


# EDA ---------------------------------------------------------------------
# Time Series Plots:
# Plot GDP
GDP.Plot <- ggplot(TriangleData) +
  geom_line(aes(x = date, y = GDP.per.Capita)) + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Year', y = 'GDP Per Capita',
       title = 'GDP per Capita Over Time', 
       caption = 'Source: Federal Reserve Bank of St. Louis') +
  theme_minimal()
# Clear Upward Trend
# Logged GDP
GDP.Plot_Log <- ggplot(TriangleData) +
  geom_line(aes(x = date, y = log(GDP.per.Capita))) + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Year', y = 'Log of GDP Per Capita',
       title = 'Logged GDP per Capita Over Time', 
       caption = 'Source: Federal Reserve Bank of St. Louis') +
  theme_minimal()



# Plot Gini Ratio
Gini.Plot <- ggplot(TriangleData) +
  geom_line(aes(x = date, y = Gini.Ratio), color = 'blue') +
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Year', y = 'Gini Ratio',
       title = 'Gini Ratio Over Time', 
       caption = 'Source: Federal Reserve Bank of St. Louis') +
  theme_minimal()
# Clear Upward Trend

# Plot Poverty
Poverty.Plot <- ggplot(TriangleData) +
  geom_line(aes(x = date, y = PovertyRate), color = 'red') + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  labs(x = 'Date', y = 'National Poverty Rate %', 
       title = 'National Poverty Rate Over Time', caption = 'Source: US Census Bureau') +
  theme_minimal()

# Plot Unemployment
UNRATE.Plot <- ggplot(TriangleData) +
  geom_line(aes(x = date, y = UNRATE), color = 'brown') + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  labs(x = 'Date', y = 'Unemployment Rate %', 
       title = 'Unemployment Rate Over Time', 
       caption = 'Source: Federal Reserve Bank of St. Louis') +
  theme_minimal()

# First Difference Plots
# Create New Object
TriangleData.FD <- data.frame(
  date = TriangleData$date[2:length(TriangleData$date)],
  GDP.FD = diff(log(TriangleData$GDP.per.Capita)),
  Gini.FD = diff(TriangleData$Gini.Ratio),
  Poverty.FD = diff(TriangleData$PovertyRate),
  UNRATE.FD = diff(TriangleData$UNRATE)
)
# GDP
GDP.FD.Plot <- ggplot(TriangleData.FD) +
  geom_line(aes(x = date, y = GDP.FD)) + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Year', y = 'Delta rGDP',
       title = 'Real GDP Per Capita', 
       caption = 'Source: Federal Reserve Bank of St. Louis') +
  theme_minimal()
# Gini
Gini.FD.Plot <- ggplot(TriangleData.FD) +
  geom_line(aes(x = date, y = Gini.FD), color = 'blue') + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Year', y = 'Delta Gini',
       title = 'Gini Ratio', 
       caption = 'Source: Federal Reserve Bank of St. Louis') +
  theme_minimal()
# Poverty
Poverty.FD.Plot <- ggplot(TriangleData.FD) +
  geom_line(aes(x = date, y = Poverty.FD), color = 'red') + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Year', y = 'Delta Poverty',
       title = 'National Poverty Rate', 
       caption = 'Source: US Census Bureau') +
  theme_minimal()
# Unemployment
UNRATE.FD.Plot <- ggplot(TriangleData.FD) +
  geom_line(aes(x = date, y = UNRATE.FD), color = 'brown') + 
  scale_x_date(date_breaks = '10 years', date_labels = '%Y') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Year', y = 'Delta Unemployment',
       title = 'Unemployment Rate', 
       caption = 'Source: Federal Reserve Bank of St. Louis') +
  theme_minimal()



# Calculate Summary Statistics
SummaryStats <- tibble(
  Series = c('GDP Per Capita', 'Gini Ratio', 'National Poverty Rate %', 'Unemployment Rate %'),
  Mean = sapply(dplyr::select(TriangleData, 3, 4, 2, 5), mean),
  `Standard Deviation` = sapply(dplyr::select(TriangleData, 3, 4, 2, 5), sd),
  Median = sapply(dplyr::select(TriangleData, 3, 4, 2, 5), median),
  `Interquartile Range` = sapply(dplyr::select(TriangleData, 3, 4, 2, 5), IQR),
  `Maximum Value` = sapply(dplyr::select(TriangleData, 3, 4, 2, 5), max),
  `Minimum Value` = sapply(dplyr::select(TriangleData, 3, 4, 2, 5), min)) 
SummaryStats[, 2:7] <- round(SummaryStats[, 2:7], 2)

# Unit Root: -----------------------------------------------------------------

# Stationarity Testing:
# Poverty 
# TS Plots - ACF and PACF
# ggtsdisplay(Poverty.US.TS)
# Unit Root Test: ADF
# summary(ur.df(NationalPov.TS, lags = 4, type = 'trend', selectlags = 'BIC'))
# We fail to reject the null, all tests 
# Repeat for none:
summary(ur.df(NationalPov.TS, lags = 4, type = 'drift', selectlags = 'BIC'))

# Difference
NationalPov.TS_diff <- diff(NationalPov.TS)
summary(ur.df(NationalPov.TS_diff, lags = 4, type = 'none', selectlags = 'BIC'))
# Reject null after first differencing
# Kpss Test:
summary(ur.kpss(NationalPov.TS, lags = 'short', type = 'mu'))
summary(ur.kpss(diff(NationalPov.TS), lags = 'short', type = 'mu'))
# We fail to reject the null of stationarity
# After first differencing, we still fail to reject
# Total US appears stationary from these results: d = 1, (both tests align after first difference)


# GDP
# TS Plots - ACF and PACF
# ggtsdisplay(GDP.TS)
# Clearly Nonstationary - visible trend and slow decaying ACF
summary(ur.df(GDP.TS_Log, type = 'trend', lags = 4, selectlags = 'BIC'))
# ADF Test confirms as such. Reject null that random walk is 'true' model, 
# Fail to reject unit root
summary(ur.kpss(GDP.TS, type = 'tau', lags = 'short'))
# Does differencing remove the unit root?
GDP.TS_diff <- diff(GDP.TS_Log)
# TS Plots - Differenced GDP Data
# ggtsdisplay(GDP.TS_diff)
# Still unclear - we see a huge drop around the 2008 recession
# ADF:
summary(ur.df(GDP.TS_diff, type = 'none', lags = 4, selectlags = 'BIC'))
# Series is stationary - we reject the null of unit root at 1% level
# Reject null of random walk vs random walk + trend + drift
# KPSS:
summary(ur.kpss(GDP.TS_diff, type = 'mu', lags = 'short'))
# Therefore - Order of integration is 1, but questions about how to handle trend (decomposition?)

# Gini Ratio:
# TS Plots
# ggtsdisplay(Gini.TS)
# Similar to GDP, clearly nonstationary
summary(ur.df(Gini.TS, type = 'trend', lags = 4, selectlags = 'BIC')) # fail to reject 
summary(ur.kpss(Gini.TS, lags = 'short', type = 'tau'))
# Take First Differences
Gini.TS_diff <- diff(Gini.TS)
# ggtsdisplay(Gini.TS_diff)
# Appears stationary from the graph, with no trend. Will test:
# ADF:
summary(ur.df(Gini.TS_diff, type = 'none', lags = 4, selectlags = 'BIC'))
# Reject the Null of Unit Root at 1%
# KPSS:
summary(ur.kpss(Gini.TS_diff, lags = 'short'))
# Fail to reject the null of stationarity
# Therefore, D = 1


# Unemployment
summary(ur.df(UNRATE.TS, type = 'drift', lags = 4, selectlags = 'BIC'))
summary(ur.df(diff(UNRATE.TS), type = 'none', lags = 4, selectlags = 'BIC'))
summary(ur.kpss(UNRATE.TS, type = 'mu', lags = 'short'))
summary(ur.kpss(diff(UNRATE.TS), type = 'mu', lags = 'short'))
# Order of Integration is 0

# Summarize Results in a Table:
Stationary.Results <- tibble(
  Series = c('National Poverty Rate', 'GDP Per Capita', 'Gini Ratio', 'Unem. Rate'),
  `ADF Statistic` = c(-2.87, -2.275, -1.62, -3.37), 
  `ADF CV-T1` = c(-2.89, -3.45, -3.45, -2.89),
  `ADF Stat-FD` = c(-3.72, -3.24, -5.32, -5.22),
  `ADF CV-T2` = c(-1.95, -1.95, -1.95, -1.95),
  `KPSS Statistic` = c(0.128, 0.232, 0.171, 0.10),
  `KPSS CV-T1` = c(0.463, 0.146, 0.146, 0.463),
  `KPSS Stat-FD` = c(0.097, 0.047, 0.120, 0.129),
  `KPSS CV-T2` = c(0.463, 0.463, 0.463, 0.463),
  `Order` = c(1, 1, 1, 0)
)

# Cointegration Testing
# Since Unemployment is D = 0, the four variable system is not cointegrated
# But the triangle system may be (unlikely given visual evidence)
# Select 2 Lags on the basis of the VAR Select below
# Define Test Object
CI.Test <- cbind(GDP.TS, Gini.TS, NationalPov.TS)
# summary(ca.jo(x = CI.Test, type = 'trace', ecdet = 'trend', K = 2))
# summary(ca.jo(x = CI.Test, type = 'eigen', ecdet = 'trend', K = 2))
# Fail to reject the Null of R = 0 for both tests - no cointegration


# Modeling  ---------------------------------------------------------------

# Create Data for VAR Model
VAR.Data_US <- cbind(GDP.TS_diff, Gini.TS_diff, NationalPov.TS_diff)

# Alternative - Include Unemployment Rate as first differences
VAR.Data_Alt <- cbind(GDP.TS_diff, Gini.TS_diff, NationalPov.TS_diff, UNRATE.TS[2:length(UNRATE.TS)])
# With First Difference Unemployment
VAR.Data_Alt.FD <- cbind(GDP.TS_diff, Gini.TS_diff, NationalPov.TS_diff, diff(UNRATE.TS))
# What Lag Order Should We Use?
VARselect(VAR.Data_US, exogen = UNRATE.TS[2:length(UNRATE.TS)]) 
# Differing Results - BIC is optimizing for lag length of 1, AIC for 2
# Will test between lag length of 1 and 2
# Exogenous Unemployment
VAR_Exog.1 <- VAR(VAR.Data_US, p = 1, exogen = UNRATE.TS[2:length(UNRATE.TS)], type = 'none')
VAR_Exog.2 <- VAR(VAR.Data_US, p = 2, exogen = UNRATE.TS[2:length(UNRATE.TS)], type = 'none')
# First Differenced, Exogenous Unemployment
VAR_Exog.1FD <- VAR(VAR.Data_US, p = 1, exogen = diff(UNRATE.TS), type = 'none')
VAR_Exog.2FD <- VAR(VAR.Data_US, p = 2, exogen = diff(UNRATE.TS), type = 'none')
# Endogenous Unemployment
VAR_Endog.1 <- VAR(VAR.Data_Alt, p = 1, type = 'none')
VAR_Endog.2 <- VAR(VAR.Data_Alt, p = 2, type = 'none')
# First Differenced, Endogenous Unemployment
VAR_Endog.1FD <- VAR(VAR.Data_Alt.FD, p = 1, type = 'none')
VAR_Endog.2FD <- VAR(VAR.Data_Alt.FD, p = 2, type = 'none')
# Collect Models in a List
ModelList <- list(VAR_Exog.1, VAR_Exog.2, VAR_Exog.1FD, VAR_Exog.2FD, VAR_Endog.1,
                  VAR_Endog.2, VAR_Endog.1FD, VAR_Endog.2FD)

# Specifications
VAR.Specification <- tibble(
  Unemployment = c('Exogenous', 'Exogenous', 'Exogenous, First Differenced', 
                   'Exogenous, First Differenced',
                   'Endogenous', 'Endogenous',
                   'Endogenous, First Differenced',
                   'Endogenous, First Differenced'),
  `Lag Order` = c(1, 2, 1, 2, 1, 2, 1, 2),
  AIC = sapply(ModelList, AIC),
  BIC = sapply(ModelList, BIC),
  `Log Likelihood` = sapply(ModelList, logLik))
# VAR_Exog.1FD is the best model (Exogenous Unemployment in First Differences, p = 1)

# Is the system well specified?
roots(VAR_Exog.1FD, modulus = TRUE)

# Coefficient Estimates (Recreate the Summary Output)
# Extract to table
VAR.Results <- broom::tidy(VAR_Exog.1FD$varresult$NationalPov.TS_diff)
VAR.Summary <- tibble(
  Variable = c('Delta rGDP per Capita - 1st Lag', 'Delta Gini Ratio - 1st Lag', 
               'Delta Poverty - 1st Lag', 'Delta Unemployment (Exog) - 1st Lag'),
  Estimate = c(VAR.Results$estimate),
  `Standard Error` = c(VAR.Results$std.error),
  `T-Statistic` = c(VAR.Results$statistic),
  `P-Value` = c(VAR.Results$p.value)
)
# Round Results
VAR.Summary[, 2:5] = round(VAR.Summary[, 2:5], 3)

# Check Residuals
SerialCorr.Resid <- serial.test(VAR_Exog.1FD, lags.pt = 12, type = "BG")
# P value of 0.7166, no autocorrelation
# Arch Test (Homoscedasticity)
Triangle.Arch <- arch.test(VAR_Exog.1FD, lags.multi = 12, multivariate.only = TRUE)
# P-value of 1, no heteroscedasticity


# Granger Causality
# Does rGDP and Gini granger cause Poverty?
causality(VAR_Exog.1FD, cause = c('GDP.TS_diff', 'Gini.TS_diff'))
# Fail to reject, no granger causality





