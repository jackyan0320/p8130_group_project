Exploratory
================
12/4/2018

Data manipulation
-----------------

``` r
cancer_df <-
  read_csv("Cancer_Registry.csv") %>% 
  janitor::clean_names() %>%
  separate(geography, into = c("county", "state"), sep = ",") %>%
  select(-pct_no_hs18_24, -pct_some_col18_24, -binned_inc) %>%     # lots of same county names, so I did not remove state column #
  arrange(county)
  
colSums(is.na(cancer_df))                             # check number of missing values #
```

    ##              avg_ann_count        avg_deaths_per_year 
    ##                          0                          0 
    ##          target_death_rate             incidence_rate 
    ##                          0                          0 
    ##                 med_income                pop_est2015 
    ##                          0                          0 
    ##            poverty_percent              study_per_cap 
    ##                          0                          0 
    ##                 median_age            median_age_male 
    ##                          0                          0 
    ##          median_age_female                     county 
    ##                          0                          0 
    ##                      state         avg_household_size 
    ##                          0                          0 
    ##            percent_married                pct_hs18_24 
    ##                          0                          0 
    ##          pct_bach_deg18_24              pct_hs25_over 
    ##                          0                          0 
    ##        pct_bach_deg25_over        pct_employed16_over 
    ##                          0                        152 
    ##      pct_unemployed16_over       pct_private_coverage 
    ##                          0                          0 
    ## pct_private_coverage_alone      pct_emp_priv_coverage 
    ##                        609                          0 
    ##        pct_public_coverage  pct_public_coverage_alone 
    ##                          0                          0 
    ##                  pct_white                  pct_black 
    ##                          0                          0 
    ##                  pct_asian             pct_other_race 
    ##                          0                          0 
    ##     pct_married_households                 birth_rate 
    ##                          0                          0

``` r
cancer_df = 
  cancer_df %>%
  select(-pct_employed16_over, -pct_private_coverage_alone) %>%
  mutate(pct_white = ifelse(pct_white > 75, 1, 0)) %>% # 1 represents high percentage of white #
  select(target_death_rate, everything()) %>% 
  mutate(state = fct_reorder(state, target_death_rate))

str(cancer_df)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    3047 obs. of  30 variables:
    ##  $ target_death_rate        : num  184 230 216 152 163 ...
    ##  $ avg_ann_count            : num  143 323 221 1757 105 ...
    ##  $ avg_deaths_per_year      : int  61 151 106 561 43 46 52 22 73 21 ...
    ##  $ incidence_rate           : num  431 493 479 469 420 ...
    ##  $ med_income               : int  35525 40269 38390 57908 35425 31558 31386 48216 42063 47423 ...
    ##  $ pop_est2015              : int  24932 62577 32973 434211 25378 19027 22004 7228 20148 19254 ...
    ##  $ poverty_percent          : num  21.4 22 19.4 11.6 26.9 26.3 27.2 10.3 17.8 19 ...
    ##  $ study_per_cap            : num  0 0 0 415 0 ...
    ##  $ median_age               : num  43.3 35.7 45.3 35.8 27.9 41 37 45.9 51.4 28.9 ...
    ##  $ median_age_male          : num  40.7 34.7 42.7 35 27.8 39.1 36.4 45 49.7 28.3 ...
    ##  $ median_age_female        : num  44.9 37.2 47.3 36.6 27.9 42.5 38.2 47.7 53.3 29.9 ...
    ##  $ county                   : chr  "Abbeville County" "Acadia Parish" "Accomack County" "Ada County" ...
    ##  $ state                    : Factor w/ 51 levels " Utah"," Hawaii",..: 39 47 35 5 43 51 45 19 18 22 ...
    ##  $ avg_household_size       : num  2.52 2.7 2.29 2.61 2.36 2.5 2.78 2.25 2.47 3.24 ...
    ##  $ percent_married          : num  46.8 47.3 52.6 53.6 39.1 50.9 52.7 56.2 50.9 56.9 ...
    ##  $ pct_hs18_24              : num  40 35.9 40.4 30.1 12.9 25.6 47.6 46.1 44.6 36.9 ...
    ##  $ pct_bach_deg18_24        : num  3.2 2.9 12.9 11.4 11.1 4.7 2.8 11.5 3.3 1.6 ...
    ##  $ pct_hs25_over            : num  37.5 39.2 39.9 21.4 33.9 36.3 42.6 44.7 43.7 25.9 ...
    ##  $ pct_bach_deg25_over      : num  8.6 7.6 11 24.8 15.2 9.3 10 11.9 8.5 9.1 ...
    ##  $ pct_unemployed16_over    : num  10.7 10.1 6.8 6.6 7.9 7.7 7.9 3.2 10.1 9.6 ...
    ##  $ pct_private_coverage     : num  56.6 56.5 61.8 76.1 73.9 56.1 38.6 75.5 63.9 45 ...
    ##  $ pct_emp_priv_coverage    : num  37.6 38.9 37.3 53.6 50.6 34.2 26.6 43.4 34.1 30.5 ...
    ##  $ pct_public_coverage      : num  43.8 37.4 36.7 23.7 27.4 40.9 41.9 38 50.8 40 ...
    ##  $ pct_public_coverage_alone: num  25.9 23.1 18.4 11.1 13.2 23.8 29.4 14.2 22.6 30.5 ...
    ##  $ pct_white                : num  0 1 0 1 1 1 0 1 1 0 ...
    ##  $ pct_black                : num  27.95 17.43 28.68 1.14 2.07 ...
    ##  $ pct_asian                : num  0 0.195 0.145 2.665 2.387 ...
    ##  $ pct_other_race           : num  0.24 1.001 1.323 1.284 0.192 ...
    ##  $ pct_married_households   : num  47.3 47.1 47.5 52.6 43.1 ...
    ##  $ birth_rate               : num  5.82 6.94 4.35 4.78 3.77 ...

``` r
cancer_df %>% 
  lm(target_death_rate ~ state, data = .) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ state, data = .)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -105.285  -13.750   -0.396   12.852  183.370 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 135.752      4.389  30.931  < 2e-16 ***
    ## state Hawaii                  7.223     12.218   0.591 0.554443    
    ## state Colorado                5.810      5.285   1.099 0.271717    
    ## state Arizona                13.335      7.344   1.816 0.069510 .  
    ## state Idaho                  18.451      5.625   3.280 0.001050 ** 
    ## state Wyoming                23.235      6.471   3.591 0.000335 ***
    ## state Connecticut            21.961      9.180   2.392 0.016808 *  
    ## state New Mexico             20.429      5.959   3.428 0.000616 ***
    ## state California             22.345      5.328   4.194 2.82e-05 ***
    ## state Nebraska               24.802      5.076   4.886 1.08e-06 ***
    ## state Minnesota              25.730      5.024   5.121 3.22e-07 ***
    ## state South Dakota           27.794      5.299   5.245 1.67e-07 ***
    ## state Massachusetts          29.048      7.511   3.868 0.000112 ***
    ## state North Dakota           24.609      5.428   4.534 6.02e-06 ***
    ## state Rhode Island           29.468     11.103   2.654 0.007995 ** 
    ## state Montana                27.434      5.486   5.001 6.05e-07 ***
    ## state Kansas                 32.082      4.936   6.500 9.37e-11 ***
    ## state Wisconsin              36.622      5.146   7.116 1.38e-12 ***
    ## state Iowa                   30.737      4.951   6.208 6.11e-10 ***
    ## state New Jersey             32.881      6.635   4.955 7.62e-07 ***
    ## state Oregon                 34.537      5.806   5.949 3.02e-09 ***
    ## state Washington             30.740      5.709   5.384 7.84e-08 ***
    ## state New Hampshire          35.128      8.442   4.161 3.26e-05 ***
    ## state New York               35.888      5.258   6.825 1.06e-11 ***
    ## state Texas                  35.834      4.636   7.729 1.47e-14 ***
    ## state Vermont                40.520      7.511   5.395 7.39e-08 ***
    ## state Maryland               40.727      6.398   6.366 2.24e-10 ***
    ## state Florida                43.678      5.210   8.384  < 2e-16 ***
    ## state Delaware               43.115     13.879   3.107 0.001911 ** 
    ## state Pennsylvania           39.671      5.199   7.631 3.11e-14 ***
    ## state Nevada                 41.683      7.061   5.903 3.96e-09 ***
    ## state Michigan               42.070      5.053   8.326  < 2e-16 ***
    ## state North Carolina         42.155      4.951   8.514  < 2e-16 ***
    ## state Illinois               47.473      4.936   9.618  < 2e-16 ***
    ## state Virginia               46.844      4.840   9.679  < 2e-16 ***
    ## state District of Columbia   46.548     23.224   2.004 0.045125 *  
    ## state Georgia                47.044      4.756   9.892  < 2e-16 ***
    ## state Maine                  47.398      7.195   6.588 5.26e-11 ***
    ## state South Carolina         52.755      5.529   9.542  < 2e-16 ***
    ## state Indiana                52.369      4.992  10.492  < 2e-16 ***
    ## state Ohio                   51.131      5.024  10.177  < 2e-16 ***
    ## state Alaska                 57.665      6.939   8.310  < 2e-16 ***
    ## state Missouri               53.943      4.877  11.061  < 2e-16 ***
    ## state Alabama                56.977      5.246  10.862  < 2e-16 ***
    ## state Oklahoma               58.197      5.101  11.410  < 2e-16 ***
    ## state West Virginia          60.959      5.359  11.375  < 2e-16 ***
    ## state Louisiana              61.922      5.233  11.832  < 2e-16 ***
    ## state Tennessee              65.124      4.974  13.094  < 2e-16 ***
    ## state Arkansas               64.339      5.118  12.570  < 2e-16 ***
    ## state Mississippi            67.086      5.060  13.258  < 2e-16 ***
    ## state Kentucky               79.564      4.858  16.379  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 22.81 on 2996 degrees of freedom
    ## Multiple R-squared:  0.3358, Adjusted R-squared:  0.3247 
    ## F-statistic: 30.29 on 50 and 2996 DF,  p-value: < 2.2e-16
