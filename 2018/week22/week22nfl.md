Tidy Tuesday - Week 22 - NFL 2000-2017 Player Data
================
Hayden MacDonald
2018-08-28

``` r
library(formatR)
library(knitr)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(tidy.opts=list(width.cutoff=50),tidy=TRUE)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
```

------------------------------------------------------------------------

Import Data
-----------

``` r
fb <- read.csv("nfl_2010-2017.csv", header = TRUE)
```

------------------------------------------------------------------------

Wrangle
-------

``` r
fb2 <- fb %>% select(game_year, rush_fumbles, rec_fumbles, 
    pass_fumbles) %>% group_by(game_year) %>% mutate_if(is.numeric, 
    funs(ifelse(is.na(.), 0, .))) %>% summarize(pct_rush_fum = sum(rush_fumbles, 
    na.rm = TRUE)/n() * 100, pct_rec_fum = sum(rec_fumbles, 
    na.rm = TRUE)/n() * 100, pct_pass_fum = sum(pass_fumbles, 
    na.rm = TRUE)/n() * 100)

# wide data
fb2
```

    ## # A tibble: 18 x 4
    ##    game_year pct_rush_fum pct_rec_fum pct_pass_fum
    ##        <int>        <dbl>       <dbl>        <dbl>
    ##  1      2000       6.58        3.07         3.96  
    ##  2      2001       4.07        3.25         2.00  
    ##  3      2002       4.34        2.99         2.49  
    ##  4      2003       4.51        3.07         2.91  
    ##  5      2004       5.05        3.82         2.91  
    ##  6      2005       0.0240      0.0240       0     
    ##  7      2006       0           0            0     
    ##  8      2007       0           0.0232       0.0232
    ##  9      2008       0           0            0     
    ## 10      2009       4.81        3.72         2.36  
    ## 11      2010       4.36        3.62         2.24  
    ## 12      2011       4.16        2.56         2.58  
    ## 13      2012       4.24        3.35         2.39  
    ## 14      2013       4.09        3.02         2.41  
    ## 15      2014       3.46        3.19         1.96  
    ## 16      2015       3.90        3.14         2.05  
    ## 17      2016       3.47        2.77         2.22  
    ## 18      2017       3.60        2.66         2.16

``` r
# convert data to long format
fb2l <- fb2 %>% gather(fum_type, percent_fum, -game_year)

fb2l
```

    ## # A tibble: 54 x 3
    ##    game_year fum_type     percent_fum
    ##        <int> <chr>              <dbl>
    ##  1      2000 pct_rush_fum      6.58  
    ##  2      2001 pct_rush_fum      4.07  
    ##  3      2002 pct_rush_fum      4.34  
    ##  4      2003 pct_rush_fum      4.51  
    ##  5      2004 pct_rush_fum      5.05  
    ##  6      2005 pct_rush_fum      0.0240
    ##  7      2006 pct_rush_fum      0     
    ##  8      2007 pct_rush_fum      0     
    ##  9      2008 pct_rush_fum      0     
    ## 10      2009 pct_rush_fum      4.81  
    ## # ... with 44 more rows

------------------------------------------------------------------------

Visualize
---------

``` r
fbvis <- fb2l %>% ggplot(aes(x = game_year)) + geom_line(aes(y = percent_fum, 
    group = fum_type, color = fum_type)) + scale_color_manual(name = "Fumble Type", 
    labels = c("Pass", "Receive", "Rush"), values = c("#003399", 
        "#aaaabb", "#cc0033")) + labs(x = "Year", y = "Percent Fumbled", 
    title = "Percentage of fumbles per year by fumble type", 
    subtitle = "Fumbles by passing, receiving or rushing") + 
    theme(axis.line = element_line(size = 0.5, linetype = "solid"), 
        panel.grid.major = element_line(linetype = "dashed"), 
        panel.grid.minor = element_line(linetype = "dashed"), 
        panel.background = element_rect(linetype = "dashed"), 
        legend.position = c(0.4, 0.9), legend.direction = "horizontal", 
        legend.background = element_rect("gray92"), 
        axis.title = element_text(size = 12), axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), title = element_text(size = 16))
```

------------------------------------------------------------------------

Add Logo and Export Visualization
---------------------------------

``` r
library(grid)
library(png)
library(Cairo)

# import logo
logo <- readPNG("nfllogo.png")

# insert logo
fbvis2 <- fbvis + annotation_raster(logo, ymin = 5, 
    ymax = 7, xmin = 2012.5, xmax = 2017.5)


# render final image
png(filename = "week22nfl.png", type = "cairo", units = "in", 
    width = 16, height = 12, pointsize = 12, res = 300)
print(fbvis2)
dev.off()
```

    ## png 
    ##   2

![](week22nfl.png)

------------------------------------------------------------------------

Session Info
------------

    ## - Session info ----------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 3.5.1 (2018-07-02)
    ##  os       Windows 10 x64              
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_Canada.1252         
    ##  ctype    English_Canada.1252         
    ##  tz       America/New_York            
    ##  date     2018-08-30                  
    ## 
    ## - Packages --------------------------------------------------------------
    ##  package     * version    date       source                            
    ##  assertthat    0.2.0      2017-04-11 CRAN (R 3.5.1)                    
    ##  backports     1.1.2      2017-12-13 CRAN (R 3.5.0)                    
    ##  bindr         0.1.1      2018-03-13 CRAN (R 3.5.1)                    
    ##  bindrcpp    * 0.2.2      2018-03-29 CRAN (R 3.5.1)                    
    ##  Cairo       * 1.5-9      2015-09-26 CRAN (R 3.5.0)                    
    ##  cellranger    1.1.0      2016-07-27 CRAN (R 3.5.1)                    
    ##  cli           1.0.0      2017-11-05 CRAN (R 3.5.1)                    
    ##  clisymbols    1.2.0      2017-05-21 CRAN (R 3.5.1)                    
    ##  colorspace    1.3-2      2016-12-14 CRAN (R 3.5.1)                    
    ##  crayon        1.3.4      2017-09-16 CRAN (R 3.5.1)                    
    ##  digest        0.6.16     2018-08-22 CRAN (R 3.5.1)                    
    ##  dplyr       * 0.7.6      2018-06-29 CRAN (R 3.5.1)                    
    ##  evaluate      0.11       2018-07-17 CRAN (R 3.5.1)                    
    ##  fansi         0.3.0      2018-08-13 CRAN (R 3.5.1)                    
    ##  formatR     * 1.5        2017-04-25 CRAN (R 3.5.1)                    
    ##  ggplot2     * 3.0.0      2018-07-03 CRAN (R 3.5.1)                    
    ##  glue          1.3.0      2018-07-17 CRAN (R 3.5.1)                    
    ##  gtable        0.2.0      2016-02-26 CRAN (R 3.5.1)                    
    ##  htmltools     0.3.6      2017-04-28 CRAN (R 3.5.1)                    
    ##  knitr       * 1.20       2018-02-20 CRAN (R 3.5.1)                    
    ##  labeling      0.3        2014-08-23 CRAN (R 3.5.0)                    
    ##  lazyeval      0.2.1      2017-10-29 CRAN (R 3.5.1)                    
    ##  magrittr      1.5        2014-11-22 CRAN (R 3.5.1)                    
    ##  munsell       0.5.0      2018-06-12 CRAN (R 3.5.1)                    
    ##  pillar        1.3.0      2018-07-14 CRAN (R 3.5.1)                    
    ##  pkgconfig     2.0.2      2018-08-16 CRAN (R 3.5.1)                    
    ##  plyr          1.8.4      2016-06-08 CRAN (R 3.5.1)                    
    ##  png         * 0.1-7      2013-12-03 CRAN (R 3.5.0)                    
    ##  purrr         0.2.5      2018-05-29 CRAN (R 3.5.1)                    
    ##  R6            2.2.2      2017-06-17 CRAN (R 3.5.1)                    
    ##  Rcpp          0.12.18    2018-07-23 CRAN (R 3.5.1)                    
    ##  readxl      * 1.1.0      2018-04-20 CRAN (R 3.5.1)                    
    ##  rlang         0.2.2      2018-08-16 CRAN (R 3.5.1)                    
    ##  rmarkdown     1.10       2018-06-11 CRAN (R 3.5.1)                    
    ##  rprojroot     1.3-2      2018-01-03 CRAN (R 3.5.1)                    
    ##  scales        1.0.0      2018-08-09 CRAN (R 3.5.1)                    
    ##  sessioninfo   1.0.1.9000 2018-08-30 Github (r-lib/sessioninfo@418dc5e)
    ##  stringi       1.1.7      2018-03-12 CRAN (R 3.5.0)                    
    ##  stringr       1.3.1      2018-05-10 CRAN (R 3.5.1)                    
    ##  tibble        1.4.2      2018-01-22 CRAN (R 3.5.1)                    
    ##  tidyr       * 0.8.1      2018-05-18 CRAN (R 3.5.1)                    
    ##  tidyselect    0.2.4      2018-02-26 CRAN (R 3.5.1)                    
    ##  utf8          1.1.4      2018-05-24 CRAN (R 3.5.1)                    
    ##  withr         2.1.2      2018-03-15 CRAN (R 3.5.1)                    
    ##  yaml          2.2.0      2018-07-25 CRAN (R 3.5.1)
