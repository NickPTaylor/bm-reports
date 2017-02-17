Classical Time Series Decomposition
================

Objective
---------

To manaully perform procedure as described on (Hyndman and Athanasopoulos 2014, sec. 6.3) for an additive time series.

Simulate Data
-------------

``` r
# for reproducibility ------------------------------------------------------
set.seed(20170214) 

# sample times -------------------------------------------------------------
start_date <- ymd(20050101)
end_date <- ymd(20141201)
date <- seq(from = start_date, to = end_date, by = "month")
         
# data components ----------------------------------------------------------
level <- 1
trend <- (row_number(date) - 1) / 12
seasonal <- sin(2 * pi * row_number(date) / 12) 
error <- rnorm(length(date), sd = 0.5)
value <- level + trend + seasonal + error

# timeseries as data frame -------------------------------------------------
ts_df <- data_frame(date, value)
```

Step 1
------

> If *m* is an even number, compute the trend-cycle component using a 2 × *m*-MA to obtain $\\hat{T}\_t$. If *m* is an odd number, compute the trend-cycle component using an *m*-MA to obtain $\\hat{T}\_t$.

$$
\\hat x
$$

References
----------

Hyndman, Rob J., and George Athanasopoulos. 2014. *Forecasting: Principles and Practice*. OTexts.
