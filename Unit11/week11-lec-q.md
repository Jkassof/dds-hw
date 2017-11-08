Week 11 BLT Assignment
================
Jordan Kassof

``` r
library(tseries)

## S&P 500 (^GSPC)
###    SNP - SNP Real Time Price. Currency in USD

# TODO: Download the data of SP500 '^gspc'.
SNPdata <- get.hist.quote("^GSPC",quote="Close")
```

    ## time series ends   2017-11-03

``` r
# TODO: Calculate the log returns, which is the subtraction of log(lag(SNPdata)) and log(SNPdata)
SNPret <- na.omit(log(SNPdata) - log(lag(SNPdata)))

# TODO: Calculate volatility measure that is to multiply sd(SNPret),sqrt(250), 100
SNPvol <- sd(SNPret) * sqrt(250) * 100


## Define getVol function for volatility
getVol <- function(d, logrets) {
    var = 0
    lam = 0
    varlist <- c()

    for (r in logrets) {
        lam = lam*(1 - 1/d) + 1
      var = (1 - 1/lam)*var + (1/lam)*r^2
        varlist <- c(varlist, var)
    }

    sqrt(varlist)
}

# Calculate volatility over entire length of series for various three different decay factors: 10 30. 100

# TODO: call getVol function with the parameters: 10,SNPret
volest <- getVol(10, SNPret)

# TODO: call getVol function with the parameters: 30,SNPret
volest2 <- getVol(30, SNPret)

# TODO: call getVol function with the parameters: 100,SNPret
volest3 <- getVol(100, SNPret)

# Plot the results, overlaying the volatility curves on the data, just as was done in the S&P example.
plot(volest,type="l")

# TODO: Add connected line segments for volest2 with the parameters: type="l",col="red"
lines(volest2, type = "l", col = "red")

# TODO: Add connected line segments for volest3 with the parameters: type="l",col="blue"
lines(volest3, type = "l", col = "blue")
```

![](week11-lec-q_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)
