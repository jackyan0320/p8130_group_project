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

``` r
cancer_df=
  cancer_df %>%
  select(-state, -county)
round(cor(cancer_df),3)                         # correlation matrix #
```

    ##                           target_death_rate avg_ann_count
    ## target_death_rate                     1.000        -0.144
    ## avg_ann_count                        -0.144         1.000
    ## avg_deaths_per_year                  -0.091         0.939
    ## incidence_rate                        0.449         0.074
    ## med_income                           -0.429         0.269
    ## pop_est2015                          -0.120         0.927
    ## poverty_percent                       0.429        -0.136
    ## study_per_cap                        -0.022         0.082
    ## median_age                            0.004        -0.024
    ## median_age_male                      -0.022        -0.125
    ## median_age_female                     0.012        -0.123
    ## avg_household_size                   -0.037         0.065
    ## percent_married                      -0.267        -0.106
    ## pct_hs18_24                           0.262        -0.182
    ## pct_bach_deg18_24                    -0.288         0.284
    ## pct_hs25_over                         0.405        -0.311
    ## pct_bach_deg25_over                  -0.485         0.321
    ## pct_unemployed16_over                 0.378        -0.009
    ## pct_private_coverage                 -0.386         0.132
    ## pct_emp_priv_coverage                -0.267         0.202
    ## pct_public_coverage                   0.405        -0.174
    ## pct_public_coverage_alone             0.449        -0.094
    ## pct_white                            -0.168        -0.127
    ## pct_black                             0.257         0.031
    ## pct_asian                            -0.186         0.435
    ## pct_other_race                       -0.190         0.209
    ## pct_married_households               -0.293        -0.106
    ## birth_rate                           -0.087        -0.035
    ##                           avg_deaths_per_year incidence_rate med_income
    ## target_death_rate                      -0.091          0.449     -0.429
    ## avg_ann_count                           0.939          0.074      0.269
    ## avg_deaths_per_year                     1.000          0.063      0.223
    ## incidence_rate                          0.063          1.000     -0.001
    ## med_income                              0.223         -0.001      1.000
    ## pop_est2015                             0.978          0.027      0.236
    ## poverty_percent                        -0.067          0.009     -0.789
    ## study_per_cap                           0.063          0.077      0.044
    ## median_age                             -0.025          0.018     -0.013
    ## median_age_male                        -0.148         -0.015     -0.092
    ## median_age_female                      -0.144         -0.009     -0.153
    ## avg_household_size                      0.086         -0.118      0.112
    ## percent_married                        -0.181         -0.120      0.355
    ## pct_hs18_24                            -0.151          0.023     -0.190
    ## pct_bach_deg18_24                       0.260          0.047      0.493
    ## pct_hs25_over                          -0.296          0.122     -0.471
    ## pct_bach_deg25_over                     0.293         -0.038      0.705
    ## pct_unemployed16_over                   0.070          0.100     -0.453
    ## pct_private_coverage                    0.056          0.105      0.724
    ## pct_emp_priv_coverage                   0.160          0.150      0.747
    ## pct_public_coverage                    -0.132          0.046     -0.755
    ## pct_public_coverage_alone              -0.027          0.041     -0.720
    ## pct_white                              -0.175         -0.022      0.161
    ## pct_black                               0.085          0.113     -0.270
    ## pct_asian                               0.443         -0.008      0.426
    ## pct_other_race                          0.215         -0.209      0.084
    ## pct_married_households                 -0.160         -0.152      0.446
    ## birth_rate                             -0.074         -0.118     -0.010
    ##                           pop_est2015 poverty_percent study_per_cap
    ## target_death_rate              -0.120           0.429        -0.022
    ## avg_ann_count                   0.927          -0.136         0.082
    ## avg_deaths_per_year             0.978          -0.067         0.063
    ## incidence_rate                  0.027           0.009         0.077
    ## med_income                      0.236          -0.789         0.044
    ## pop_est2015                     1.000          -0.065         0.056
    ## poverty_percent                -0.065           1.000        -0.056
    ## study_per_cap                   0.056          -0.056         1.000
    ## median_age                     -0.025          -0.029        -0.026
    ## median_age_male                -0.177          -0.214        -0.037
    ## median_age_female              -0.178          -0.148        -0.031
    ## avg_household_size              0.110           0.074        -0.004
    ## percent_married                -0.160          -0.643        -0.038
    ## pct_hs18_24                    -0.152           0.094        -0.057
    ## pct_bach_deg18_24               0.248          -0.387         0.064
    ## pct_hs25_over                  -0.312           0.194        -0.085
    ## pct_bach_deg25_over             0.297          -0.532         0.109
    ## pct_unemployed16_over           0.051           0.655        -0.032
    ## pct_private_coverage            0.053          -0.823         0.093
    ## pct_emp_priv_coverage           0.159          -0.683         0.100
    ## pct_public_coverage            -0.160           0.651        -0.051
    ## pct_public_coverage_alone      -0.041           0.799        -0.056
    ## pct_white                      -0.181          -0.430         0.019
    ## pct_black                       0.073           0.512        -0.020
    ## pct_asian                       0.464          -0.157         0.063
    ## pct_other_race                  0.241           0.047        -0.015
    ## pct_married_households         -0.128          -0.605        -0.052
    ## birth_rate                     -0.058          -0.012         0.011
    ##                           median_age median_age_male median_age_female
    ## target_death_rate              0.004          -0.022             0.012
    ## avg_ann_count                 -0.024          -0.125            -0.123
    ## avg_deaths_per_year           -0.025          -0.148            -0.144
    ## incidence_rate                 0.018          -0.015            -0.009
    ## med_income                    -0.013          -0.092            -0.153
    ## pop_est2015                   -0.025          -0.177            -0.178
    ## poverty_percent               -0.029          -0.214            -0.148
    ## study_per_cap                 -0.026          -0.037            -0.031
    ## median_age                     1.000           0.129             0.125
    ## median_age_male                0.129           1.000             0.934
    ## median_age_female              0.125           0.934             1.000
    ## avg_household_size            -0.032          -0.343            -0.368
    ## percent_married                0.046           0.450             0.375
    ## pct_hs18_24                    0.051           0.241             0.243
    ## pct_bach_deg18_24             -0.017          -0.034            -0.071
    ## pct_hs25_over                  0.037           0.318             0.345
    ## pct_bach_deg25_over           -0.020          -0.132            -0.181
    ## pct_unemployed16_over          0.019          -0.143            -0.111
    ## pct_private_coverage           0.005           0.082             0.047
    ## pct_emp_priv_coverage         -0.037          -0.209            -0.252
    ## pct_public_coverage            0.049           0.399             0.455
    ## pct_public_coverage_alone     -0.003           0.002             0.048
    ## pct_white                      0.000           0.310             0.265
    ## pct_black                     -0.017          -0.243            -0.157
    ## pct_asian                     -0.038          -0.238            -0.259
    ## pct_other_race                -0.030          -0.267            -0.274
    ## pct_married_households         0.015           0.222             0.162
    ## birth_rate                    -0.008          -0.104            -0.099
    ##                           avg_household_size percent_married pct_hs18_24
    ## target_death_rate                     -0.037          -0.267       0.262
    ## avg_ann_count                          0.065          -0.106      -0.182
    ## avg_deaths_per_year                    0.086          -0.181      -0.151
    ## incidence_rate                        -0.118          -0.120       0.023
    ## med_income                             0.112           0.355      -0.190
    ## pop_est2015                            0.110          -0.160      -0.152
    ## poverty_percent                        0.074          -0.643       0.094
    ## study_per_cap                         -0.004          -0.038      -0.057
    ## median_age                            -0.032           0.046       0.051
    ## median_age_male                       -0.343           0.450       0.241
    ## median_age_female                     -0.368           0.375       0.243
    ## avg_household_size                     1.000          -0.101       0.027
    ## percent_married                       -0.101           1.000       0.133
    ## pct_hs18_24                            0.027           0.133       1.000
    ## pct_bach_deg18_24                     -0.061           0.053      -0.389
    ## pct_hs25_over                         -0.139           0.102       0.439
    ## pct_bach_deg25_over                    0.014           0.104      -0.405
    ## pct_unemployed16_over                  0.132          -0.551       0.131
    ## pct_private_coverage                  -0.144           0.449      -0.254
    ## pct_emp_priv_coverage                  0.011           0.233      -0.244
    ## pct_public_coverage                   -0.135          -0.247       0.278
    ## pct_public_coverage_alone              0.061          -0.460       0.234
    ## pct_white                             -0.126           0.545       0.029
    ## pct_black                              0.030          -0.622      -0.025
    ## pct_asian                              0.132          -0.149      -0.200
    ## pct_other_race                         0.229          -0.105      -0.060
    ## pct_married_households                 0.091           0.870       0.120
    ## birth_rate                             0.076           0.141       0.058
    ##                           pct_bach_deg18_24 pct_hs25_over
    ## target_death_rate                    -0.288         0.405
    ## avg_ann_count                         0.284        -0.311
    ## avg_deaths_per_year                   0.260        -0.296
    ## incidence_rate                        0.047         0.122
    ## med_income                            0.493        -0.471
    ## pop_est2015                           0.248        -0.312
    ## poverty_percent                      -0.387         0.194
    ## study_per_cap                         0.064        -0.085
    ## median_age                           -0.017         0.037
    ## median_age_male                      -0.034         0.318
    ## median_age_female                    -0.071         0.345
    ## avg_household_size                   -0.061        -0.139
    ## percent_married                       0.053         0.102
    ## pct_hs18_24                          -0.389         0.439
    ## pct_bach_deg18_24                     1.000        -0.384
    ## pct_hs25_over                        -0.384         1.000
    ## pct_bach_deg25_over                   0.600        -0.741
    ## pct_unemployed16_over                -0.309         0.082
    ## pct_private_coverage                  0.488        -0.222
    ## pct_emp_priv_coverage                 0.451        -0.223
    ## pct_public_coverage                  -0.422         0.428
    ## pct_public_coverage_alone            -0.422         0.297
    ## pct_white                             0.055         0.118
    ## pct_black                            -0.094        -0.024
    ## pct_asian                             0.346        -0.437
    ## pct_other_race                        0.007        -0.286
    ## pct_married_households                0.000         0.062
    ## birth_rate                           -0.125         0.017
    ##                           pct_bach_deg25_over pct_unemployed16_over
    ## target_death_rate                      -0.485                 0.378
    ## avg_ann_count                           0.321                -0.009
    ## avg_deaths_per_year                     0.293                 0.070
    ## incidence_rate                         -0.038                 0.100
    ## med_income                              0.705                -0.453
    ## pop_est2015                             0.297                 0.051
    ## poverty_percent                        -0.532                 0.655
    ## study_per_cap                           0.109                -0.032
    ## median_age                             -0.020                 0.019
    ## median_age_male                        -0.132                -0.143
    ## median_age_female                      -0.181                -0.111
    ## avg_household_size                      0.014                 0.132
    ## percent_married                         0.104                -0.551
    ## pct_hs18_24                            -0.405                 0.131
    ## pct_bach_deg18_24                       0.600                -0.309
    ## pct_hs25_over                          -0.741                 0.082
    ## pct_bach_deg25_over                     1.000                -0.373
    ## pct_unemployed16_over                  -0.373                 1.000
    ## pct_private_coverage                    0.603                -0.634
    ## pct_emp_priv_coverage                   0.539                -0.475
    ## pct_public_coverage                    -0.636                 0.530
    ## pct_public_coverage_alone              -0.606                 0.655
    ## pct_white                               0.067                -0.399
    ## pct_black                              -0.146                 0.469
    ## pct_asian                               0.438                -0.022
    ## pct_other_race                          0.039                 0.028
    ## pct_married_households                  0.098                -0.470
    ## birth_rate                             -0.088                -0.068
    ##                           pct_private_coverage pct_emp_priv_coverage
    ## target_death_rate                       -0.386                -0.267
    ## avg_ann_count                            0.132                 0.202
    ## avg_deaths_per_year                      0.056                 0.160
    ## incidence_rate                           0.105                 0.150
    ## med_income                               0.724                 0.747
    ## pop_est2015                              0.053                 0.159
    ## poverty_percent                         -0.823                -0.683
    ## study_per_cap                            0.093                 0.100
    ## median_age                               0.005                -0.037
    ## median_age_male                          0.082                -0.209
    ## median_age_female                        0.047                -0.252
    ## avg_household_size                      -0.144                 0.011
    ## percent_married                          0.449                 0.233
    ## pct_hs18_24                             -0.254                -0.244
    ## pct_bach_deg18_24                        0.488                 0.451
    ## pct_hs25_over                           -0.222                -0.223
    ## pct_bach_deg25_over                      0.603                 0.539
    ## pct_unemployed16_over                   -0.634                -0.475
    ## pct_private_coverage                     1.000                 0.827
    ## pct_emp_priv_coverage                    0.827                 1.000
    ## pct_public_coverage                     -0.720                -0.778
    ## pct_public_coverage_alone               -0.886                -0.729
    ## pct_white                                0.353                 0.237
    ## pct_black                               -0.345                -0.237
    ## pct_asian                                0.189                 0.282
    ## pct_other_race                          -0.176                -0.064
    ## pct_married_households                   0.435                 0.323
    ## birth_rate                              -0.040                -0.094
    ##                           pct_public_coverage pct_public_coverage_alone
    ## target_death_rate                       0.405                     0.449
    ## avg_ann_count                          -0.174                    -0.094
    ## avg_deaths_per_year                    -0.132                    -0.027
    ## incidence_rate                          0.046                     0.041
    ## med_income                             -0.755                    -0.720
    ## pop_est2015                            -0.160                    -0.041
    ## poverty_percent                         0.651                     0.799
    ## study_per_cap                          -0.051                    -0.056
    ## median_age                              0.049                    -0.003
    ## median_age_male                         0.399                     0.002
    ## median_age_female                       0.455                     0.048
    ## avg_household_size                     -0.135                     0.061
    ## percent_married                        -0.247                    -0.460
    ## pct_hs18_24                             0.278                     0.234
    ## pct_bach_deg18_24                      -0.422                    -0.422
    ## pct_hs25_over                           0.428                     0.297
    ## pct_bach_deg25_over                    -0.636                    -0.606
    ## pct_unemployed16_over                   0.530                     0.655
    ## pct_private_coverage                   -0.720                    -0.886
    ## pct_emp_priv_coverage                  -0.778                    -0.729
    ## pct_public_coverage                     1.000                     0.866
    ## pct_public_coverage_alone               0.866                     1.000
    ## pct_white                              -0.116                    -0.296
    ## pct_black                               0.196                     0.330
    ## pct_asian                              -0.306                    -0.181
    ## pct_other_race                         -0.079                     0.084
    ## pct_married_households                 -0.362                    -0.474
    ## birth_rate                             -0.031                    -0.005
    ##                           pct_white pct_black pct_asian pct_other_race
    ## target_death_rate            -0.168     0.257    -0.186         -0.190
    ## avg_ann_count                -0.127     0.031     0.435          0.209
    ## avg_deaths_per_year          -0.175     0.085     0.443          0.215
    ## incidence_rate               -0.022     0.113    -0.008         -0.209
    ## med_income                    0.161    -0.270     0.426          0.084
    ## pop_est2015                  -0.181     0.073     0.464          0.241
    ## poverty_percent              -0.430     0.512    -0.157          0.047
    ## study_per_cap                 0.019    -0.020     0.063         -0.015
    ## median_age                    0.000    -0.017    -0.038         -0.030
    ## median_age_male               0.310    -0.243    -0.238         -0.267
    ## median_age_female             0.265    -0.157    -0.259         -0.274
    ## avg_household_size           -0.126     0.030     0.132          0.229
    ## percent_married               0.545    -0.622    -0.149         -0.105
    ## pct_hs18_24                   0.029    -0.025    -0.200         -0.060
    ## pct_bach_deg18_24             0.055    -0.094     0.346          0.007
    ## pct_hs25_over                 0.118    -0.024    -0.437         -0.286
    ## pct_bach_deg25_over           0.067    -0.146     0.438          0.039
    ## pct_unemployed16_over        -0.399     0.469    -0.022          0.028
    ## pct_private_coverage          0.353    -0.345     0.189         -0.176
    ## pct_emp_priv_coverage         0.237    -0.237     0.282         -0.064
    ## pct_public_coverage          -0.116     0.196    -0.306         -0.079
    ## pct_public_coverage_alone    -0.296     0.330    -0.181          0.084
    ## pct_white                     1.000    -0.734    -0.228         -0.216
    ## pct_black                    -0.734     1.000     0.017         -0.023
    ## pct_asian                    -0.228     0.017     1.000          0.201
    ## pct_other_race               -0.216    -0.023     0.201          1.000
    ## pct_married_households        0.486    -0.574    -0.087         -0.027
    ## birth_rate                   -0.001    -0.068    -0.062          0.060
    ##                           pct_married_households birth_rate
    ## target_death_rate                         -0.293     -0.087
    ## avg_ann_count                             -0.106     -0.035
    ## avg_deaths_per_year                       -0.160     -0.074
    ## incidence_rate                            -0.152     -0.118
    ## med_income                                 0.446     -0.010
    ## pop_est2015                               -0.128     -0.058
    ## poverty_percent                           -0.605     -0.012
    ## study_per_cap                             -0.052      0.011
    ## median_age                                 0.015     -0.008
    ## median_age_male                            0.222     -0.104
    ## median_age_female                          0.162     -0.099
    ## avg_household_size                         0.091      0.076
    ## percent_married                            0.870      0.141
    ## pct_hs18_24                                0.120      0.058
    ## pct_bach_deg18_24                          0.000     -0.125
    ## pct_hs25_over                              0.062      0.017
    ## pct_bach_deg25_over                        0.098     -0.088
    ## pct_unemployed16_over                     -0.470     -0.068
    ## pct_private_coverage                       0.435     -0.040
    ## pct_emp_priv_coverage                      0.323     -0.094
    ## pct_public_coverage                       -0.362     -0.031
    ## pct_public_coverage_alone                 -0.474     -0.005
    ## pct_white                                  0.486     -0.001
    ## pct_black                                 -0.574     -0.068
    ## pct_asian                                 -0.087     -0.062
    ## pct_other_race                            -0.027      0.060
    ## pct_married_households                     1.000      0.102
    ## birth_rate                                 0.102      1.000
