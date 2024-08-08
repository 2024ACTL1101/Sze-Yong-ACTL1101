---
title: "ACTL1101 Assignment Part A"
author: "Sze Yong Ng"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


### Step 1: Data Loading

The following code was run to load the AMD stock data, convert it into a dataframe and plot a graph of the share price vs time.

```{r load-data}
amd_df <- read.csv("AMD.csv")
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
To set up the trading algorithm, the following variables and columns were initialized.

```{r trading 1}

amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

```

The algorithm uses a for loop to execute the following strategy:

(1) On the final trading day, sell all held stocks

(2) If the previous price was 0, buy 100 stocks

(3) If the previous price was greater than the current price, buy 100 stocks

(4) If the previous price was less than or equal to the current price, do nothing

This was implemented in the algorithm below.

```{r trading 2}

for (i in 1:nrow(amd_df)) {
  if(i == nrow(amd_df)){
    amd_df$trade_type[i] <- "Sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
    amd_df$accumulated_shares[i] <- 0
  } # Executes condition (1)
  else if(previous_price == 0){
    amd_df$trade_type[i] <- "Buy"
    amd_df$costs_proceeds[i] <- share_size * amd_df$close[i] * -1
    amd_df$accumulated_shares[i] <- share_size + accumulated_shares
  } # Executes condition (2)
  else if(amd_df$close[i] < previous_price){
    amd_df$trade_type[i] <- "Buy"
    amd_df$costs_proceeds[i] <- share_size * amd_df$close[i] * -1
    amd_df$accumulated_shares[i] <- share_size + accumulated_shares
  } # Executes condition (3)
  else if(amd_df$close[i] >= previous_price){
    amd_df$trade_type[i] <- NA
    amd_df$costs_proceeds[i] <- 0
    amd_df$accumulated_shares[i] <- accumulated_shares
  } # Executes condition (4)
previous_price <- amd_df$close[i]
accumulated_shares <- amd_df$accumulated_shares[i]
# Initializes variables for the next trading day
}

```


\pagebreak


## Step 3: Customize Trading Period
The trading period chosen was the 01/09/2021 to 01/06/2022. Below, the amd_df dataframe was filtered to only include the rows which fell in this timeframe, and a graph showing the closing price at each included date was plotted.
```{r period}
start_date <- as.Date('2021-09-01')
end_date <- as.Date('2022-06-01')    
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date,]
plot(amd_df$date, amd_df$close,'l')
```


\pagebreak


## Step 4: Run Your Algorithm and Analyze Results
The trading algorithm shown in Step 2 was run again using the new trading period. The total profit and invested capital were computed to determine the ROI using this strategy.

```{r}
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  if(i == nrow(amd_df)){
    amd_df$trade_type[i] <- "Sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
    amd_df$accumulated_shares[i] <- 0
  }
  else if(previous_price == 0){
    amd_df$trade_type[i] <- "Buy"
    amd_df$costs_proceeds[i] <- share_size * amd_df$close[i] * -1
    amd_df$accumulated_shares[i] <- share_size + accumulated_shares
  }
  else if(amd_df$close[i] < previous_price){
    amd_df$trade_type[i] <- "Buy"
    amd_df$costs_proceeds[i] <- share_size * amd_df$close[i] * -1
    amd_df$accumulated_shares[i] <- share_size + accumulated_shares
  }
  else if(amd_df$close[i] >= previous_price){
    amd_df$trade_type[i] <- NA
    amd_df$costs_proceeds[i] <- 0
    amd_df$accumulated_shares[i] <- accumulated_shares
  }
previous_price <- amd_df$close[i]
accumulated_shares <- amd_df$accumulated_shares[i]
}
profit <- sum(amd_df$costs_proceeds)
invested_capital <- -sum(amd_df$costs_proceeds)+amd_df$costs_proceeds[nrow(amd_df)]
ROI <- profit/invested_capital*100
print(profit)
print(ROI)
```


\pagebreak


## Step 5: Stop-Loss Mechanism
This strategy will be extended to include a stop-loss mechanism by adding one more condition:

(5) If the current price falls 5% below the average purchase price, then half of the shares in the current portfolio will be sold. No shares will be bought.

The columns and variables below were initialized for trading logic.

```{r option}
amd_df$trade_type <- NA
amd_df$costs_proceeds <- 0
amd_df$accumulated_shares <- 0
previous_price <- 0 
share_size <- 100
accumulated_shares <- 0
stop_loss_percentage <- 0.95 
# Percentage of average price at which the algorithm will "stop-loss sell"
average_purchase_price <- 0
invested_capital <- 0
shares_held <- rep(c(0),each=nrow(amd_df))
# Creates a column vector with the nth row recording the quantity of shares held
# from a purchase on the nth day
```

Average purchase price is calculated by multiplying the quantity of purchased shares owned by their respective purchase prices, and dividing it by the quantity of shares sold. By using the newly introduced "shares_held" vector, taking its dot product with the "amd_df$close" vector, and dividing it by the sum of the "shares_held" vector, we can compute the average price after every trading day, since the nth rows of both vectors correspond to the quantity of shares held and the share price on the same nth day. Furthermore, when a stop-loss sell is executed, it is not possible to determine which specific stocks were sold. Therefore, it is assumed half of every stock in the current portfolio was sold.The algorithm below is similar to the previous model.

```{r stop-loss}
for (i in 1:nrow(amd_df)) {
  if(i == nrow(amd_df)){
    amd_df$trade_type[i] <- "Sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]
    amd_df$accumulated_shares[i] <- 0
  }
  else if(previous_price == 0){
    amd_df$trade_type[i] <- "Buy"
    amd_df$costs_proceeds[i] <- share_size * amd_df$close[i] * -1
    amd_df$accumulated_shares[i] <- share_size + accumulated_shares
    invested_capital <- share_size * amd_df$close[i] + invested_capital
    shares_held[i] <- share_size 
    # Records the quantity of each purchase in a running portfolio
  }
  else if(amd_df$close[i] <= average_purchase_price*stop_loss_percentage){
    amd_df$trade_type[i] <- "Stop-Loss Sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i] * 1/2
    amd_df$accumulated_shares[i] <- 1/2 * accumulated_shares
    shares_held <- 1/2*shares_held 
    # Stop-loss sell halves the quantity of all shares in the shares_held vector
  }
  else if(amd_df$close[i] < previous_price){
    amd_df$trade_type[i] <- "Buy"
    amd_df$costs_proceeds[i] <- share_size * amd_df$close[i] * -1
    amd_df$accumulated_shares[i] <- share_size + accumulated_shares
    invested_capital <- share_size * amd_df$close[i] + invested_capital
    shares_held[i] <- share_size
  }
  else if(amd_df$close[i] >= previous_price){
    amd_df$trade_type[i] <- NA
    amd_df$costs_proceeds[i] <- 0
    amd_df$accumulated_shares[i] <- accumulated_shares
  }
  previous_price <- amd_df$close[i]
  accumulated_shares <- amd_df$accumulated_shares[i]
  average_purchase_price <- sum(t(amd_df$close)%*%shares_held)/sum(shares_held)
  # The matrix transposition and multiplication in the numerater is equivalent 
  # to taking the dot product of these two column vectors
}

```

## Step 6: Summarize Your Findings

The profit and ROI of the stop-loss strategy was calculated below.

```{r}
profit <- sum(amd_df$costs_proceeds)
ROI <- profit/invested_capital*100
print(profit)
print(ROI)
```

The ROI and profit of both strategies was negative,because the trend during this period was a sharp increase from around October 2021 to January 2022, followed by a gradual decrease to a price lower than the original starting price (\$109.99 vs \$101.22). In the case of the original algorithm, it only sold shares on the final day at a price of \$101.22, but bought many shares during the peak, for an average purchase price of \$116.05. However, the stop-loss strategy performed better, improving the P/L from -\$142384 to -\$59139.06, and ROI from -12.78% to -9.07%. This was because it was able to sell its shares as early as January 2022 when AMD stock started falling from its peak, thus reducing losses.

The significant growth during October to December 2021 was likely due to the release of Windows 11 on October 5, 2021, which was compatible with AMD's Ryzen processors.  Following the success of Windows 11, demand for AMD processors drove up their stock price, as investors expected high revenue and profits in the following quarters. Within two days, the stock price rose 4.6% from \$101.81 to \$106.45, and this strong growth continued until December.

The gradual decline in share price can be partially attributed to the Federal Reserve announcement on December 15. The statement projected that there would be three interest rate hikes in 2022. As a result, investors expected future contractionary economic conditions to halt high-growth stocks like AMD, resulting in the share price falling by 5.4% from \$146.50 to \$138.64. Although this was not enough to trigger the stop-loss sell (since many stocks were bought earlier at a lower price), when the stock continued to fall to \$121.89 on January 20, 2022, this triggered the first stop-loss sell at a reasonably high price, mitigating losses.
