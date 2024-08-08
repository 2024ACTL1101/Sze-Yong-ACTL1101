---
title: "ACTL1101 Assignment Part B"
author: "Sze Yong Ng"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
library(dplyr)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1.  **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2.  **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3.  **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4.  **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

\pagebreak

## Instructions

### Step 1: Data Loading

-   We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
-   `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
-   Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data<-getSymbols("AMD",src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
gspc_data<-getSymbols("^GSPC",src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
rf_data<-getSymbols("DTB4WK",src="FRED",from=start_date,tO=end_date,auto.assign=FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  
# Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```


### Data Processing


```{r dataprocessing}
# Use colSums function
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

\pagebreak

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula

The formula for CAPM is given by:

$$ E(R_i) = R_f + \beta_i (E(R_m) - R_f) $$

Where:

-   $E(R_i)$ is the expected return on the capital asset,
-   $R_f$ is the risk-free rate,
-   $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
-   $E(R_m)$ is the expected return of the market.

#### CAPM Model Daily Estimation

-   **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period. $$
    \text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
    $$

```{r return}
df <- df %>%
  mutate(
    AMD_return = c(NA, diff(AMD) / head(AMD, -1)),
    GSPC_return = c(NA, diff(GSPC) / head(GSPC, -1))
  )
```

-   **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula: $$
    \text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
    $$

```{r riskfree}
df$daily_RFR <- ((1 + df$RF/100) ^ (1/360)) - 1
```

-   **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
df$AMD_excess_return <- df$AMD_return - df$daily_RFR
df$GSPC_excess_return <- df$GSPC_return - df$daily_RFR
```

-   **Perform Regression Analysis**: Using linear regression, we estimate the beta ($\beta$) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
#Removing first row with "NA" values
df2 <- df %>%
  filter(!is.na(AMD_excess_return), !is.na(GSPC_excess_return))

#Using linear regression
model <- lm(AMD_excess_return ~ GSPC_excess_return, data = df2)
beta_1 <- coef(model)[2]
summary(model)
print(beta_1)
```

#### Interpretation

What is your $\beta$? Is AMD more volatile or less volatile than the market?

My regression yielded a value of $\beta\approx1.57$ implying that AMD on average has a 57% greater yield than the S&P500. Since an asset must have more intrinsic risk in order to produce a higher yield, AMD is more volatile than the rest of the market.

#### Plotting the CAPM Line

Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}

ggplot(df2, aes(x = GSPC_excess_return, y = AMD_excess_return)) +
  geom_point(size = 1/3) +
  geom_smooth(method = lm, se = TRUE) +
  labs(title = "AMD vs S&P500 (Daily Excess Returns)",
       x = "S&P500 Excess Daily Returns",
       y = "AMD Excess Daily Returns")
```

\pagebreak

### Step 3: Predictions Interval

Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast (*$s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.

```{r pi}
#given initial annual returns
annual_rfr <- 0.05
annual_GSPC_return <- 0.133

#convert annual risk free rate to daily risk free rate assuming compounding
daily_rfr <- (1 + annual_rfr)^(1/360) - 1

#convert annual GSPC returns into daily returns
#assuming simple average returns over 252 trading days
daily_GSPC_return <- annual_GSPC_return/252

#calculate daily annual excess returns for GSPC
X_f <- daily_GSPC_return - daily_rfr

#record number of observations
n <- nrow(df2)

#calculate s_e
s_e <- sqrt(sum(residuals(model)^2) / (n-1-1))

#calculate SSX
SSX <- sum((df2$GSPC_excess_return - mean(df2$GSPC_excess_return))^2)

#calculate s_f
s_f <- s_e * sqrt(1 + 1/n + (X_f - mean(df2$GSPC_excess_return))^2/SSX)

#estimate annual s_f
s_f_annual <- s_f*sqrt(252)

#use CAPM model to predict annual AMD excess returns
Y_f_annual <- annual_rfr + beta_1 * (annual_GSPC_return - annual_rfr)

#given a 90% confidence interval, calculate t_value and bounds to be
alpha <- 0.1 
t_value <- qt(1 - alpha/2, df = n - 2)
lower_bound <- Y_f_annual - t_value * s_f_annual
upper_bound <- Y_f_annual + t_value * s_f_annual
cat("The estimated annual AMD excess returns is ", round(Y_f_annual * 100, 4),"%", "\n")
cat(sprintf("The 90%% prediction interval is [%.2f%%, %.2f%%]\n",lower_bound*100,upper_bound*100))

```

### Appendix

```{r appendix}

head(df2)

```
