Exploratory
================
12/4/2018

Data manipulation
-----------------

``` r
cancer_df = read_csv("Cancer_Registry.csv") %>% 
  janitor::clean_names() %>%
  select(-geography)
colSums(is.na(cancer_df))                # check number of missing values #
```

    ##              avg_ann_count        avg_deaths_per_year 
    ##                          0                          0 
    ##          target_death_rate             incidence_rate 
    ##                          0                          0 
    ##                 med_income                pop_est2015 
    ##                          0                          0 
    ##            poverty_percent              study_per_cap 
    ##                          0                          0 
    ##                 binned_inc                 median_age 
    ##                          0                          0 
    ##            median_age_male          median_age_female 
    ##                          0                          0 
    ##         avg_household_size            percent_married 
    ##                          0                          0 
    ##             pct_no_hs18_24                pct_hs18_24 
    ##                          0                          0 
    ##          pct_some_col18_24          pct_bach_deg18_24 
    ##                       2285                          0 
    ##              pct_hs25_over        pct_bach_deg25_over 
    ##                          0                          0 
    ##        pct_employed16_over      pct_unemployed16_over 
    ##                        152                          0 
    ##       pct_private_coverage pct_private_coverage_alone 
    ##                          0                        609 
    ##      pct_emp_priv_coverage        pct_public_coverage 
    ##                          0                          0 
    ##  pct_public_coverage_alone                  pct_white 
    ##                          0                          0 
    ##                  pct_black                  pct_asian 
    ##                          0                          0 
    ##             pct_other_race     pct_married_households 
    ##                          0                          0 
    ##                 birth_rate 
    ##                          0

``` r
cancer_df = cancer_df %>%
  select(-pct_employed16_over, - pct_private_coverage_alone)

  
                            

cancer_df %>%
  lm(target_death_rate ~ pct_no_hs18_24 + pct_hs18_24 + pct_bach_deg18_24 + pct_some_col18_24 + pct_hs25_over + pct_bach_deg25_over, data=.) %>%
  summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_no_hs18_24 + pct_hs18_24 + 
    ##     pct_bach_deg18_24 + pct_some_col18_24 + pct_hs25_over + pct_bach_deg25_over, 
    ##     data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -79.841 -14.643   1.189  14.222 106.332 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          571.0955  1564.0002   0.365  0.71510    
    ## pct_no_hs18_24        -4.1739    15.6310  -0.267  0.78952    
    ## pct_hs18_24           -3.6694    15.6392  -0.235  0.81456    
    ## pct_bach_deg18_24     -4.3610    15.6517  -0.279  0.78061    
    ## pct_some_col18_24     -3.8261    15.6373  -0.245  0.80677    
    ## pct_hs25_over          0.5149     0.1938   2.656  0.00807 ** 
    ## pct_bach_deg25_over   -1.7899     0.2929  -6.112 1.58e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.29 on 755 degrees of freedom
    ##   (2285 observations deleted due to missingness)
    ## Multiple R-squared:  0.2473, Adjusted R-squared:  0.2413 
    ## F-statistic: 41.34 on 6 and 755 DF,  p-value: < 2.2e-16

``` r
cancer_df %>%
  lm(target_death_rate ~ pct_no_hs18_24 + pct_hs18_24 + pct_bach_deg18_24 + pct_some_col18_24 , data=.) %>%
  summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_no_hs18_24 + pct_hs18_24 + 
    ##     pct_bach_deg18_24 + pct_some_col18_24, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -83.919 -15.657  -0.327  15.587 108.040 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)        1704.13    1681.01   1.014    0.311
    ## pct_no_hs18_24      -15.33      16.80  -0.912    0.362
    ## pct_hs18_24         -14.83      16.81  -0.882    0.378
    ## pct_bach_deg18_24   -16.94      16.82  -1.007    0.314
    ## pct_some_col18_24   -15.33      16.81  -0.912    0.362
    ## 
    ## Residual standard error: 26.18 on 757 degrees of freedom
    ##   (2285 observations deleted due to missingness)
    ## Multiple R-squared:  0.1233, Adjusted R-squared:  0.1187 
    ## F-statistic: 26.63 on 4 and 757 DF,  p-value: < 2.2e-16

``` r
data=cancer_df %>%
  select(target_death_rate, pct_no_hs18_24 , pct_hs18_24 , pct_bach_deg18_24 , pct_some_col18_24 , pct_hs25_over , pct_bach_deg25_over)
round(cor(data),3)
```

    ##                     target_death_rate pct_no_hs18_24 pct_hs18_24
    ## target_death_rate               1.000          0.088       0.262
    ## pct_no_hs18_24                  0.088          1.000       0.085
    ## pct_hs18_24                     0.262          0.085       1.000
    ## pct_bach_deg18_24              -0.288         -0.381      -0.389
    ## pct_some_col18_24                  NA             NA          NA
    ## pct_hs25_over                   0.405          0.217       0.439
    ## pct_bach_deg25_over            -0.485         -0.397      -0.405
    ##                     pct_bach_deg18_24 pct_some_col18_24 pct_hs25_over
    ## target_death_rate              -0.288                NA         0.405
    ## pct_no_hs18_24                 -0.381                NA         0.217
    ## pct_hs18_24                    -0.389                NA         0.439
    ## pct_bach_deg18_24               1.000                NA        -0.384
    ## pct_some_col18_24                  NA                 1            NA
    ## pct_hs25_over                  -0.384                NA         1.000
    ## pct_bach_deg25_over             0.600                NA        -0.741
    ##                     pct_bach_deg25_over
    ## target_death_rate                -0.485
    ## pct_no_hs18_24                   -0.397
    ## pct_hs18_24                      -0.405
    ## pct_bach_deg18_24                 0.600
    ## pct_some_col18_24                    NA
    ## pct_hs25_over                    -0.741
    ## pct_bach_deg25_over               1.000

``` r
cancer_df %>%
  lm(target_death_rate ~ pct_no_hs18_24 + pct_hs18_24 + pct_bach_deg18_24 + pct_some_col18_24 +pct_hs25_over + pct_bach_deg25_over+ pct_hs25_over * pct_bach_deg25_over, data=.) %>%
  summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_no_hs18_24 + pct_hs18_24 + 
    ##     pct_bach_deg18_24 + pct_some_col18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_hs25_over * pct_bach_deg25_over, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -81.317 -14.589   1.921  14.228 107.365 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                        761.73051 1560.62253   0.488 0.625625
    ## pct_no_hs18_24                      -6.27473   15.60133  -0.402 0.687657
    ## pct_hs18_24                         -5.76378   15.60933  -0.369 0.712043
    ## pct_bach_deg18_24                   -6.51321   15.62311  -0.417 0.676873
    ## pct_some_col18_24                   -5.89624   15.60691  -0.378 0.705688
    ## pct_hs25_over                        1.11864    0.31121   3.595 0.000346
    ## pct_bach_deg25_over                 -0.43470    0.62057  -0.700 0.483840
    ## pct_hs25_over:pct_bach_deg25_over   -0.04774    0.01929  -2.475 0.013557
    ##                                      
    ## (Intercept)                          
    ## pct_no_hs18_24                       
    ## pct_hs18_24                          
    ## pct_bach_deg18_24                    
    ## pct_some_col18_24                    
    ## pct_hs25_over                     ***
    ## pct_bach_deg25_over                  
    ## pct_hs25_over:pct_bach_deg25_over *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.21 on 754 degrees of freedom
    ##   (2285 observations deleted due to missingness)
    ## Multiple R-squared:  0.2533, Adjusted R-squared:  0.2464 
    ## F-statistic: 36.55 on 7 and 754 DF,  p-value: < 2.2e-16

``` r
a=cancer_df %>%
  select()
```

-   "avg\_deaths\_per\_year", "avg\_ann\_count" and "pop\_est2015" are highly correlated. The last three steps show that we should choose avg\_ann\_count, because the p-value is the smallest, showing significant relation between target\_death\_rate and avg\_ann\_count.
