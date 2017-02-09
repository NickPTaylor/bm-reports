BM Report Analysis
================
2017-02-09

Introduction
============

The purpose of this report is to study and analyse the data available from the BM (Balancing Mechanism) reports. These reports contain current and historical data relating to the supply of electricity on the UK national grid. The reports include data relating to transmission, generation, demand and balancing. We access the data using an API (Application Programming Interface) which is described in detail [(Ghanty 2016)](https://www.elexon.co.uk/wp-content/uploads/2016/10/Application-Programming-Interfaces-API-and-Data-Push-user-guide.pdf) and is available on the Elexon website .

Herein, we will show how the R programming language can be used to analyse and acquire knowledge from the data; we will perform the analysis in a reproducible fashion. Specifically, we will demonstrate the `tidyverse` style of R programming, as described by [(Wickham and Grolemund 2017)](http://r4ds.had.co.nz/). The `tidyverse` is a set of packages (groups of functions) that are highly consistent in style and interface. When tools all work in the same way, it is much easier to use them and understand them. The `tidyverse` tools include functions for reading, tidying, iterating, plotting and processing data.

In addition to demonstrating the `tidyverse`, we will demonstrate analysis techniques which are appropriate to developing predictive and forecasting models for time-series, which are presented in [(Hyndman and Athanasopoulos 2014)](https://www.otexts.org/fpp).

Basic Data Aqusition
====================

Reading the Data
----------------

As discussed above, we use an API to access the BM Report data. This requires a key which is available upon registration with Elexon. The code below requires each user to enter their unique API key. If you run this code, you will be prompted to enter your personal key. The method for connecting to the API is described in Ghanty (2016) pp. 12-13.

In the example below, we extract the 'Rolling System Frequency' between the 16th and 18th January, 2017. Accessing this particular data is described in Ghanty (2016) p. 81. We use the `urltools` package to encode the API URL (Universal Resource Locator) given the query. In this case, we request the data in XML (Extensible Markup Language) format.

``` r
# get API key from user  -------------------------------------------------------
api_key <- getPass("Enter API key")
stopifnot(!is.null(api_key))

# construct query API URL ------------------------------------------------------
service_url <- paste(
    "https://api.bmreports.com:443",
    "BMRS/FREQ",
    "v1",
    sep = "/")

query_url <- service_url %>% 
    param_set("APIKey", api_key) %>% 
    param_set("FromDateTime", url_encode("2017-01-16 00:00:00")) %>% 
    param_set("ToDateTime", url_encode("2017-01-18 00:00:00")) %>% 
    param_set("ServiceType", "xml")  

# connect to API and request data ---------------------------------------------
print(paste0("[", Sys.time(), "] Connected to BM Reports API ..."))
```

    ## [1] "[2017-02-09 16:01:09] Connected to BM Reports API ..."

``` r
xmlfile <- read_xml(query_url)

# tidy up sensitive information -----------------------------------------------
rm(api_key,  query_url)
```

To prevent unnecessary connections to the API, the results of the code above are cached. Care should be taken in general to be 'polite', only connecting to the API when really necessary. API providers, including Elexon, set out rules defining what acceptable usage means.

Extracting Data
---------------

The `xml2` package is used to parse and process the data. Note that HTTP (Hyper Text Transfer Protocol) status code for 'OK' is 200 so we first check that data was retrieved from the API without error. Next, we extract the relevant data. The XML nodes are selected using the XPath language, which is described in W3Schools (2017) (see [here](http://www.w3schools.com/xml/xpath_intro.asp)).

``` r
# check succsess - http code must be 200 --------------------------------------
status <- xmlfile %>% 
    xml_find_first("responseMetadata/httpCode") %>% 
    xml_text()
stopifnot(status == 200)

# extract raw data from XML document and save ---------------------------------
data_xpaths <- list(
    date = "//reportSnapshotTime", 
    time = "//spotTime", 
    freq = "//frequency"
)

df_raw <- data_xpaths %>% 
    map_df(~ xmlfile %>%  xml_find_all(.x) %>%  xml_text())

saveRDS(df_raw, "df_raw.rds")
```

The raw data from the XML file requires some tidying. Here, we are using the `dplyr` package (a part of the `tidyverse`) to manipulate the raw data. The `lubridate` package is used to work with time-based data. In our experience, great care must be taken with time stamped data. Be aware of local time zones, local time formatting, daylight saving hours, leap years, leap minutes, etc. The `lubridate` package takes care of this detail. We simply tell `lubridate` to work in 'London' time.

``` r
df_tidy <- df_raw %>% 
    mutate(
        date_time = ymd_hms(paste(date, time), tz = "Europe/London"),
        freq = as.numeric(freq)) %>% 
    select(date_time, freq, -c(date, time))   
    
knitr::kable(
  head(df_tidy), 
  caption = "Data retrieved from BM reports API"
)
```

| date\_time          |    freq|
|:--------------------|-------:|
| 2017-01-16 00:00:00 |  50.033|
| 2017-01-16 00:00:15 |  49.984|
| 2017-01-16 00:00:30 |  49.969|
| 2017-01-16 00:00:45 |  49.908|
| 2017-01-16 00:01:00 |  49.881|
| 2017-01-16 00:01:15 |  49.868|

Plotting Data
-------------

Finally, we use the `ggplot` package (also part of the `tidyverse`) to produce a variety of intuitive plots.

### Time series

A time series line plot gives some insight into:

-   Overall patterns: trend, seasonality, cyclic components.

-   Levels and variance.

-   Whether the data is stationary.

-   Anomalies, outliers, errors, etc.

``` r
# time series plot ------------------------------------------------------------
df_tidy %>% ggplot(aes(date_time, freq)) +
    geom_line(colour = 'red', size = 0.2) +
    labs(title = "The UK National Grid frequency must be balanced at 50Hz", 
         subtitle = "Data from BM Reports",
         x = "Local Time",
         y = "Frequency (Hz)") +
    geom_hline(yintercept = 50, linetype = 'dashed') +
    scale_x_datetime(
        breaks = 
            seq(min(df_tidy$date_time), max(df_tidy$date_time), "6 hours"),
        date_labels = "%d-%b\n%H:%M"
    ) + 
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
```

![](README_files/figure-markdown_github/plot_df_tidy-1.png)

References
==========

Ghanty, Zaahir. 2016. *BMRS Api and Data Push User Guide*. Elexon.

Hyndman, Rob J., and George Athanasopoulos. 2014. *Forecasting: Principles and Practice*. OTexts.

W3Schools. 2017. “XPath Tutorial.” <http://www.w3schools.com/xml/xpath_intro.asp>.

Wickham, Hadley, and Garrett Grolemund. 2017. *R for Data Science: Import, Tidy, Transform, Visualize, and Model Data*. 1st ed. Paperback; O’Reilly Media.
