understand the gender pay gap three ways
================
chad allison \| 5 december 2022

following [this tutorial](https://juliasilge.com/blog/pay-gap-uk/) from
Julia Silge

------------------------------------------------------------------------

### loading libraries and setting preferences

``` r
library(tidyverse)

knitr::opts_chunk$set(message = F, warning = F, fig.align = "center")
options(scipen = 999)
theme_set(theme_minimal())
```

### loading data

``` r
df_raw = read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv",
  col_types = cols())

head(df_raw)
```

    ## # A tibble: 6 x 27
    ##   employer_name           employer_id address post_code company_number sic_codes
    ##   <chr>                         <dbl> <chr>   <chr>     <chr>          <chr>    
    ## 1 Bryanston School, Inco~         676 Bryans~ DT11 0PX  00226143       85310    
    ## 2 RED BAND CHEMICAL COMP~       16879 19 Smi~ EH6 8NU   SC016876       47730    
    ## 3 123 EMPLOYEES LTD             17677 34 Rou~ LS7 1AB   10530651       78300    
    ## 4 1610 LIMITED                    682 Trinit~ TA6 3JA   06727055       93110    
    ## 5 1879 EVENTS MANAGEMENT~       17101 The Su~ SR5 1SU   07743495       56210:70~
    ## 6 1LIFE MANAGEMENT SOLUT~         687 Ldh Ho~ PE27 4AA  02566586       93110:93~
    ## # ... with 21 more variables: diff_mean_hourly_percent <dbl>,
    ## #   diff_median_hourly_percent <dbl>, diff_mean_bonus_percent <dbl>,
    ## #   diff_median_bonus_percent <dbl>, male_bonus_percent <dbl>,
    ## #   female_bonus_percent <dbl>, male_lower_quartile <dbl>,
    ## #   female_lower_quartile <dbl>, male_lower_middle_quartile <dbl>,
    ## #   female_lower_middle_quartile <dbl>, male_upper_middle_quartile <dbl>,
    ## #   female_upper_middle_quartile <dbl>, male_top_quartile <dbl>, ...

### counting SIC codes

``` r
df_raw |>
  select(sic_codes) |>
  separate_rows(sic_codes, sep = ":") |>
  count(sic_codes, sort = T) |>
  head(10)
```

    ## # A tibble: 10 x 2
    ##    sic_codes     n
    ##    <chr>     <int>
    ##  1 1          6584
    ##  2 85310      3020
    ##  3 <NA>       2894
    ##  4 82990      2588
    ##  5 85200      2219
    ##  6 84110      1886
    ##  7 70100      1541
    ##  8 86900      1246
    ##  9 78200      1149
    ## 10 86210      1074

### histogram of median difference in hourly pay

``` r
df_raw |>
  ggplot(aes(diff_median_hourly_percent / 100)) +
  geom_histogram(bins = 25, fill = "springgreen4", alpha = 0.75) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(x = "percent difference in median hourly pay") +
  annotate("text", x = -0.275, y = 5000,
           label = "values greater than zero indicate\nmen making more than women")
```

<img src="pay_gap_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
