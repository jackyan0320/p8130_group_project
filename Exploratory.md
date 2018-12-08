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
    ##  $ pct_white                : num  69.9 79 68 91.4 93.5 ...
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


mult.fit = lm(target_death_rate ~ ., data=cancer_df)     # stepwise regression, and get 17 variables left#
step(mult.fit, direction='both')
```

    ## Start:  AIC=18083.53
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + study_per_cap + 
    ##     median_age + median_age_male + median_age_female + avg_household_size + 
    ##     percent_married + pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + 
    ##     pct_bach_deg25_over + pct_unemployed16_over + pct_private_coverage + 
    ##     pct_emp_priv_coverage + pct_public_coverage + pct_public_coverage_alone + 
    ##     pct_white + pct_black + pct_asian + pct_other_race + pct_married_households + 
    ##     birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_public_coverage        1         2 1130759 18082
    ## - median_age_female          1         3 1130761 18082
    ## - study_per_cap              1         6 1130763 18082
    ## - pct_asian                  1        40 1130797 18082
    ## - median_age                 1        44 1130801 18082
    ## - pct_public_coverage_alone  1        94 1130852 18082
    ## - avg_household_size         1        97 1130854 18082
    ## - pct_bach_deg18_24          1       121 1130878 18082
    ## - pct_black                  1       170 1130928 18082
    ## - med_income                 1       395 1131152 18083
    ## <none>                                   1130757 18084
    ## - pct_white                  1      1140 1131898 18085
    ## - median_age_male            1      2060 1132817 18087
    ## - pct_unemployed16_over      1      2199 1132956 18088
    ## - pop_est2015                1      3240 1133998 18090
    ## - pct_emp_priv_coverage      1      3357 1134115 18091
    ## - pct_private_coverage       1      3815 1134573 18092
    ## - pct_hs25_over              1      5444 1136201 18096
    ## - poverty_percent            1      6145 1136902 18098
    ## - avg_ann_count              1      8334 1139091 18104
    ## - birth_rate                 1      9000 1139758 18106
    ## - avg_deaths_per_year        1      9143 1139901 18106
    ## - pct_hs18_24                1     11842 1142599 18113
    ## - percent_married            1     13718 1144475 18118
    ## - pct_married_households     1     16130 1146888 18125
    ## - pct_other_race             1     20089 1150847 18135
    ## - pct_bach_deg25_over        1     23814 1154571 18145
    ## - incidence_rate             1    267736 1398493 18729
    ## 
    ## Step:  AIC=18081.54
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + study_per_cap + 
    ##     median_age + median_age_male + median_age_female + avg_household_size + 
    ##     percent_married + pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + 
    ##     pct_bach_deg25_over + pct_unemployed16_over + pct_private_coverage + 
    ##     pct_emp_priv_coverage + pct_public_coverage_alone + pct_white + 
    ##     pct_black + pct_asian + pct_other_race + pct_married_households + 
    ##     birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - median_age_female          1         2 1130761 18080
    ## - study_per_cap              1         6 1130765 18080
    ## - pct_asian                  1        40 1130799 18080
    ## - median_age                 1        44 1130803 18080
    ## - avg_household_size         1        96 1130855 18080
    ## - pct_bach_deg18_24          1       124 1130883 18080
    ## - pct_black                  1       175 1130935 18080
    ## - med_income                 1       396 1131155 18081
    ## - pct_public_coverage_alone  1       403 1131162 18081
    ## <none>                                   1130759 18082
    ## - pct_white                  1      1139 1131898 18083
    ## + pct_public_coverage        1         2 1130757 18084
    ## - median_age_male            1      2093 1132852 18085
    ## - pct_unemployed16_over      1      2282 1133041 18086
    ## - pop_est2015                1      3254 1134013 18088
    ## - pct_emp_priv_coverage      1      4099 1134858 18091
    ## - pct_private_coverage       1      4812 1135571 18093
    ## - pct_hs25_over              1      5445 1136204 18094
    ## - poverty_percent            1      6172 1136931 18096
    ## - avg_ann_count              1      8333 1139092 18102
    ## - birth_rate                 1      9002 1139761 18104
    ## - avg_deaths_per_year        1      9159 1139918 18104
    ## - pct_hs18_24                1     11881 1142640 18111
    ## - percent_married            1     13755 1144514 18116
    ## - pct_married_households     1     16169 1146928 18123
    ## - pct_other_race             1     20101 1150860 18133
    ## - pct_bach_deg25_over        1     24603 1155362 18145
    ## - incidence_rate             1    267813 1398572 18727
    ## 
    ## Step:  AIC=18079.54
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + study_per_cap + 
    ##     median_age + median_age_male + avg_household_size + percent_married + 
    ##     pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_black + pct_asian + 
    ##     pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - study_per_cap              1         6 1130767 18078
    ## - pct_asian                  1        40 1130801 18078
    ## - median_age                 1        45 1130806 18078
    ## - avg_household_size         1       100 1130861 18078
    ## - pct_bach_deg18_24          1       123 1130884 18078
    ## - pct_black                  1       186 1130948 18078
    ## - med_income                 1       396 1131157 18079
    ## - pct_public_coverage_alone  1       401 1131163 18079
    ## <none>                                   1130761 18080
    ## - pct_white                  1      1155 1131916 18081
    ## + median_age_female          1         2 1130759 18082
    ## + pct_public_coverage        1         0 1130761 18082
    ## - pct_unemployed16_over      1      2304 1133065 18084
    ## - pop_est2015                1      3262 1134023 18086
    ## - pct_emp_priv_coverage      1      4300 1135061 18089
    ## - pct_private_coverage       1      4985 1135746 18091
    ## - pct_hs25_over              1      5443 1136205 18092
    ## - poverty_percent            1      6182 1136943 18094
    ## - median_age_male            1      8257 1139018 18100
    ## - avg_ann_count              1      8352 1139113 18100
    ## - birth_rate                 1      9000 1139761 18102
    ## - avg_deaths_per_year        1      9183 1139944 18102
    ## - pct_hs18_24                1     11882 1142643 18109
    ## - percent_married            1     13853 1144614 18115
    ## - pct_married_households     1     16217 1146979 18121
    ## - pct_other_race             1     20100 1150861 18131
    ## - pct_bach_deg25_over        1     24641 1155402 18143
    ## - incidence_rate             1    268888 1399649 18728
    ## 
    ## Step:  AIC=18077.56
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age + 
    ##     median_age_male + avg_household_size + percent_married + 
    ##     pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_black + pct_asian + 
    ##     pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_asian                  1        39 1130806 18076
    ## - median_age                 1        44 1130811 18076
    ## - avg_household_size         1        99 1130866 18076
    ## - pct_bach_deg18_24          1       121 1130889 18076
    ## - pct_black                  1       186 1130954 18076
    ## - pct_public_coverage_alone  1       398 1131166 18077
    ## - med_income                 1       406 1131173 18077
    ## <none>                                   1130767 18078
    ## - pct_white                  1      1160 1131927 18079
    ## + study_per_cap              1         6 1130761 18080
    ## + median_age_female          1         2 1130765 18080
    ## + pct_public_coverage        1         0 1130767 18080
    ## - pct_unemployed16_over      1      2303 1133070 18082
    ## - pop_est2015                1      3258 1134026 18084
    ## - pct_emp_priv_coverage      1      4295 1135062 18087
    ## - pct_private_coverage       1      4996 1135763 18089
    ## - pct_hs25_over              1      5468 1136235 18090
    ## - poverty_percent            1      6211 1136979 18092
    ## - median_age_male            1      8259 1139026 18098
    ## - avg_ann_count              1      8393 1139160 18098
    ## - birth_rate                 1      9036 1139803 18100
    ## - avg_deaths_per_year        1      9188 1139955 18100
    ## - pct_hs18_24                1     11876 1142643 18107
    ## - percent_married            1     13872 1144640 18113
    ## - pct_married_households     1     16211 1146979 18119
    ## - pct_other_race             1     20099 1150866 18129
    ## - pct_bach_deg25_over        1     24714 1155481 18141
    ## - incidence_rate             1    269366 1400133 18727
    ## 
    ## Step:  AIC=18075.66
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age + 
    ##     median_age_male + avg_household_size + percent_married + 
    ##     pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_black + pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - median_age                 1        45 1130851 18074
    ## - avg_household_size         1        96 1130902 18074
    ## - pct_bach_deg18_24          1       110 1130917 18074
    ## - pct_black                  1       279 1131085 18074
    ## - pct_public_coverage_alone  1       413 1131220 18075
    ## - med_income                 1       458 1131264 18075
    ## <none>                                   1130806 18076
    ## + pct_asian                  1        39 1130767 18078
    ## + study_per_cap              1         5 1130801 18078
    ## + median_age_female          1         2 1130804 18078
    ## + pct_public_coverage        1         1 1130806 18078
    ## - pct_white                  1      1497 1132304 18078
    ## - pct_unemployed16_over      1      2302 1133109 18080
    ## - pop_est2015                1      3221 1134027 18082
    ## - pct_emp_priv_coverage      1      4338 1135144 18085
    ## - pct_private_coverage       1      4957 1135763 18087
    ## - pct_hs25_over              1      5429 1136235 18088
    ## - poverty_percent            1      6348 1137154 18091
    ## - median_age_male            1      8227 1139033 18096
    ## - avg_ann_count              1      8409 1139216 18096
    ## - birth_rate                 1      9135 1139942 18098
    ## - avg_deaths_per_year        1      9151 1139958 18098
    ## - pct_hs18_24                1     11900 1142707 18106
    ## - percent_married            1     13848 1144655 18111
    ## - pct_married_households     1     16231 1147038 18117
    ## - pct_other_race             1     20310 1151117 18128
    ## - pct_bach_deg25_over        1     24750 1155556 18140
    ## - incidence_rate             1    269327 1400133 18725
    ## 
    ## Step:  AIC=18073.78
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     avg_household_size + percent_married + pct_hs18_24 + pct_bach_deg18_24 + 
    ##     pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + 
    ##     pct_private_coverage + pct_emp_priv_coverage + pct_public_coverage_alone + 
    ##     pct_white + pct_black + pct_other_race + pct_married_households + 
    ##     birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - avg_household_size         1        94 1130945 18072
    ## - pct_bach_deg18_24          1       110 1130961 18072
    ## - pct_black                  1       279 1131130 18073
    ## - pct_public_coverage_alone  1       421 1131272 18073
    ## - med_income                 1       460 1131312 18073
    ## <none>                                   1130851 18074
    ## + median_age                 1        45 1130806 18076
    ## + pct_asian                  1        40 1130811 18076
    ## + study_per_cap              1         5 1130847 18076
    ## + median_age_female          1         2 1130849 18076
    ## + pct_public_coverage        1         1 1130851 18076
    ## - pct_white                  1      1499 1132350 18076
    ## - pct_unemployed16_over      1      2277 1133128 18078
    ## - pop_est2015                1      3239 1134091 18081
    ## - pct_emp_priv_coverage      1      4346 1135197 18084
    ## - pct_private_coverage       1      4952 1135803 18085
    ## - pct_hs25_over              1      5434 1136285 18086
    ## - poverty_percent            1      6378 1137229 18089
    ## - median_age_male            1      8391 1139242 18094
    ## - avg_ann_count              1      8401 1139252 18094
    ## - birth_rate                 1      9145 1139996 18096
    ## - avg_deaths_per_year        1      9178 1140030 18096
    ## - pct_hs18_24                1     11881 1142732 18104
    ## - percent_married            1     13843 1144695 18109
    ## - pct_married_households     1     16208 1147059 18115
    ## - pct_other_race             1     20328 1151179 18126
    ## - pct_bach_deg25_over        1     24747 1155598 18138
    ## - incidence_rate             1    269313 1400164 18723
    ## 
    ## Step:  AIC=18072.04
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     percent_married + pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + 
    ##     pct_bach_deg25_over + pct_unemployed16_over + pct_private_coverage + 
    ##     pct_emp_priv_coverage + pct_public_coverage_alone + pct_white + 
    ##     pct_black + pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_bach_deg18_24          1       108 1131053 18070
    ## - pct_black                  1       312 1131258 18071
    ## - pct_public_coverage_alone  1       418 1131363 18071
    ## - med_income                 1       488 1131434 18071
    ## <none>                                   1130945 18072
    ## + avg_household_size         1        94 1130851 18074
    ## + median_age                 1        43 1130902 18074
    ## + pct_asian                  1        37 1130908 18074
    ## + median_age_female          1         6 1130939 18074
    ## + study_per_cap              1         4 1130942 18074
    ## + pct_public_coverage        1         0 1130945 18074
    ## - pct_white                  1      1587 1132532 18074
    ## - pct_unemployed16_over      1      2321 1133267 18076
    ## - pop_est2015                1      3228 1134173 18079
    ## - pct_emp_priv_coverage      1      4373 1135318 18082
    ## - pct_private_coverage       1      5130 1136075 18084
    ## - pct_hs25_over              1      5425 1136371 18085
    ## - poverty_percent            1      6378 1137323 18087
    ## - avg_ann_count              1      8402 1139347 18093
    ## - median_age_male            1      8853 1139798 18094
    ## - birth_rate                 1      9094 1140040 18094
    ## - avg_deaths_per_year        1      9169 1140115 18095
    ## - pct_hs18_24                1     11998 1142943 18102
    ## - percent_married            1     13792 1144737 18107
    ## - pct_married_households     1     16496 1147442 18114
    ## - pct_other_race             1     20271 1151216 18124
    ## - pct_bach_deg25_over        1     24734 1155680 18136
    ## - incidence_rate             1    269380 1400325 18721
    ## 
    ## Step:  AIC=18070.33
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_black + pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_black                  1       316 1131369 18069
    ## - pct_public_coverage_alone  1       395 1131448 18069
    ## - med_income                 1       428 1131481 18070
    ## <none>                                   1131053 18070
    ## + pct_bach_deg18_24          1       108 1130945 18072
    ## + avg_household_size         1        92 1130961 18072
    ## + median_age                 1        43 1131010 18072
    ## + pct_asian                  1        26 1131027 18072
    ## + median_age_female          1         5 1131048 18072
    ## + study_per_cap              1         3 1131050 18072
    ## + pct_public_coverage        1         1 1131052 18072
    ## - pct_white                  1      1611 1132664 18073
    ## - pct_unemployed16_over      1      2407 1133460 18075
    ## - pop_est2015                1      3195 1134248 18077
    ## - pct_emp_priv_coverage      1      4369 1135422 18080
    ## - pct_private_coverage       1      5322 1136375 18083
    ## - pct_hs25_over              1      5334 1136387 18083
    ## - poverty_percent            1      6287 1137340 18085
    ## - avg_ann_count              1      8398 1139450 18091
    ## - median_age_male            1      8925 1139978 18092
    ## - birth_rate                 1      8998 1140051 18093
    ## - avg_deaths_per_year        1      9095 1140148 18093
    ## - pct_hs18_24                1     13043 1144096 18103
    ## - percent_married            1     13684 1144737 18105
    ## - pct_married_households     1     16541 1147594 18113
    ## - pct_other_race             1     20425 1151478 18123
    ## - pct_bach_deg25_over        1     26593 1157646 18139
    ## - incidence_rate             1    269512 1400565 18720
    ## 
    ## Step:  AIC=18069.18
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_public_coverage_alone  1       427 1131796 18068
    ## - med_income                 1       517 1131886 18069
    ## <none>                                   1131369 18069
    ## + pct_black                  1       316 1131053 18070
    ## + avg_household_size         1       125 1131244 18071
    ## + pct_asian                  1       115 1131254 18071
    ## + pct_bach_deg18_24          1       111 1131258 18071
    ## + median_age                 1        43 1131326 18071
    ## + median_age_female          1        26 1131343 18071
    ## + pct_public_coverage        1         4 1131365 18071
    ## + study_per_cap              1         2 1131366 18071
    ## - pct_white                  1      1692 1133061 18072
    ## - pct_unemployed16_over      1      2402 1133771 18074
    ## - pop_est2015                1      3138 1134507 18076
    ## - pct_emp_priv_coverage      1      4219 1135588 18079
    ## - pct_hs25_over              1      5510 1136879 18082
    ## - pct_private_coverage       1      5706 1137075 18083
    ## - poverty_percent            1      6056 1137425 18083
    ## - avg_ann_count              1      8244 1139612 18089
    ## - birth_rate                 1      8800 1140169 18091
    ## - avg_deaths_per_year        1      8941 1140310 18091
    ## - median_age_male            1      9613 1140982 18093
    ## - pct_hs18_24                1     13114 1144483 18102
    ## - percent_married            1     13880 1145249 18104
    ## - pct_married_households     1     16641 1148010 18112
    ## - pct_other_race             1     20590 1151959 18122
    ## - pct_bach_deg25_over        1     26286 1157655 18137
    ## - incidence_rate             1    269350 1400719 18718
    ## 
    ## Step:  AIC=18068.33
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_white + pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - med_income                 1       535 1132331 18068
    ## <none>                                   1131796 18068
    ## + pct_public_coverage_alone  1       427 1131369 18069
    ## + pct_black                  1       348 1131448 18069
    ## + pct_public_coverage        1       337 1131459 18069
    ## + pct_asian                  1       147 1131649 18070
    ## + avg_household_size         1       123 1131673 18070
    ## + pct_bach_deg18_24          1        88 1131708 18070
    ## + median_age                 1        51 1131745 18070
    ## + median_age_female          1        11 1131785 18070
    ## + study_per_cap              1         1 1131795 18070
    ## - pct_white                  1      1501 1133297 18070
    ## - pct_unemployed16_over      1      2899 1134696 18074
    ## - pop_est2015                1      3311 1135107 18075
    ## - pct_emp_priv_coverage      1      4502 1136298 18078
    ## - pct_hs25_over              1      5796 1137592 18082
    ## - poverty_percent            1      6867 1138663 18085
    ## - avg_ann_count              1      8063 1139859 18088
    ## - birth_rate                 1      8720 1140516 18090
    ## - avg_deaths_per_year        1      9152 1140948 18091
    ## - median_age_male            1      9198 1140995 18091
    ## - pct_private_coverage       1     11205 1143001 18096
    ## - pct_hs18_24                1     12986 1144782 18101
    ## - percent_married            1     13976 1145773 18104
    ## - pct_married_households     1     17287 1149084 18113
    ## - pct_other_race             1     20366 1152162 18121
    ## - pct_bach_deg25_over        1     26532 1158328 18137
    ## - incidence_rate             1    280557 1412353 18741
    ## 
    ## Step:  AIC=18067.77
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     pop_est2015 + poverty_percent + median_age_male + percent_married + 
    ##     pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + 
    ##     pct_private_coverage + pct_emp_priv_coverage + pct_white + 
    ##     pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## <none>                                   1132331 18068
    ## + med_income                 1       535 1131796 18068
    ## + pct_public_coverage_alone  1       444 1131886 18069
    ## + pct_black                  1       443 1131887 18069
    ## + pct_public_coverage        1       281 1132050 18069
    ## + pct_asian                  1       271 1132059 18069
    ## + avg_household_size         1       164 1132167 18069
    ## + median_age                 1        54 1132277 18070
    ## + pct_bach_deg18_24          1        30 1132301 18070
    ## + median_age_female          1        15 1132316 18070
    ## + study_per_cap              1         7 1132324 18070
    ## - pct_white                  1      2398 1134728 18072
    ## - pct_unemployed16_over      1      3116 1135447 18074
    ## - pop_est2015                1      3170 1135500 18074
    ## - pct_hs25_over              1      5349 1137680 18080
    ## - pct_emp_priv_coverage      1      6750 1139081 18084
    ## - poverty_percent            1      6855 1139185 18084
    ## - avg_ann_count              1      7961 1140292 18087
    ## - birth_rate                 1      8457 1140787 18088
    ## - median_age_male            1      8911 1141241 18090
    ## - avg_deaths_per_year        1      8935 1141266 18090
    ## - pct_private_coverage       1     11953 1144284 18098
    ## - percent_married            1     13458 1145789 18102
    ## - pct_hs18_24                1     13752 1146083 18103
    ## - pct_married_households     1     17238 1149569 18112
    ## - pct_other_race             1     20179 1152509 18120
    ## - pct_bach_deg25_over        1     26506 1158837 18136
    ## - incidence_rate             1    282026 1414357 18743

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + avg_deaths_per_year + 
    ##     incidence_rate + pop_est2015 + poverty_percent + median_age_male + 
    ##     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_white + pct_other_race + pct_married_households + birth_rate, 
    ##     data = cancer_df)
    ## 
    ## Coefficients:
    ##            (Intercept)           avg_ann_count     avg_deaths_per_year  
    ##              1.233e+02              -3.536e-03               1.883e-02  
    ##         incidence_rate             pop_est2015         poverty_percent  
    ##              1.943e-01              -1.564e-05               5.455e-01  
    ##        median_age_male         percent_married             pct_hs18_24  
    ##             -4.968e-01               8.526e-01               2.793e-01  
    ##          pct_hs25_over     pct_bach_deg25_over   pct_unemployed16_over  
    ##              3.524e-01              -1.180e+00               4.432e-01  
    ##   pct_private_coverage   pct_emp_priv_coverage               pct_white  
    ##             -5.016e-01               3.520e-01              -8.097e-02  
    ##         pct_other_race  pct_married_households              birth_rate  
    ##             -8.719e-01              -8.514e-01              -9.063e-01

``` r
seventeen_variable = cancer_df %>%
  select(target_death_rate, avg_ann_count , avg_deaths_per_year , 
     incidence_rate , pop_est2015 , poverty_percent ,median_age_male , 
     percent_married , pct_hs18_24 , pct_hs25_over , pct_bach_deg25_over , 
     pct_unemployed16_over , pct_private_coverage , pct_emp_priv_coverage , 
    pct_white , pct_other_race , pct_married_households , birth_rate)
round(cor(seventeen_variable),3)                    # correlation matrix #
```

    ##                        target_death_rate avg_ann_count avg_deaths_per_year
    ## target_death_rate                  1.000        -0.144              -0.091
    ## avg_ann_count                     -0.144         1.000               0.939
    ## avg_deaths_per_year               -0.091         0.939               1.000
    ## incidence_rate                     0.449         0.074               0.063
    ## pop_est2015                       -0.120         0.927               0.978
    ## poverty_percent                    0.429        -0.136              -0.067
    ## median_age_male                   -0.022        -0.125              -0.148
    ## percent_married                   -0.267        -0.106              -0.181
    ## pct_hs18_24                        0.262        -0.182              -0.151
    ## pct_hs25_over                      0.405        -0.311              -0.296
    ## pct_bach_deg25_over               -0.485         0.321               0.293
    ## pct_unemployed16_over              0.378        -0.009               0.070
    ## pct_private_coverage              -0.386         0.132               0.056
    ## pct_emp_priv_coverage             -0.267         0.202               0.160
    ## pct_white                         -0.177        -0.137              -0.187
    ## pct_other_race                    -0.190         0.209               0.215
    ## pct_married_households            -0.293        -0.106              -0.160
    ## birth_rate                        -0.087        -0.035              -0.074
    ##                        incidence_rate pop_est2015 poverty_percent
    ## target_death_rate               0.449      -0.120           0.429
    ## avg_ann_count                   0.074       0.927          -0.136
    ## avg_deaths_per_year             0.063       0.978          -0.067
    ## incidence_rate                  1.000       0.027           0.009
    ## pop_est2015                     0.027       1.000          -0.065
    ## poverty_percent                 0.009      -0.065           1.000
    ## median_age_male                -0.015      -0.177          -0.214
    ## percent_married                -0.120      -0.160          -0.643
    ## pct_hs18_24                     0.023      -0.152           0.094
    ## pct_hs25_over                   0.122      -0.312           0.194
    ## pct_bach_deg25_over            -0.038       0.297          -0.532
    ## pct_unemployed16_over           0.100       0.051           0.655
    ## pct_private_coverage            0.105       0.053          -0.823
    ## pct_emp_priv_coverage           0.150       0.159          -0.683
    ## pct_white                      -0.015      -0.190          -0.509
    ## pct_other_race                 -0.209       0.241           0.047
    ## pct_married_households         -0.152      -0.128          -0.605
    ## birth_rate                     -0.118      -0.058          -0.012
    ##                        median_age_male percent_married pct_hs18_24
    ## target_death_rate               -0.022          -0.267       0.262
    ## avg_ann_count                   -0.125          -0.106      -0.182
    ## avg_deaths_per_year             -0.148          -0.181      -0.151
    ## incidence_rate                  -0.015          -0.120       0.023
    ## pop_est2015                     -0.177          -0.160      -0.152
    ## poverty_percent                 -0.214          -0.643       0.094
    ## median_age_male                  1.000           0.450       0.241
    ## percent_married                  0.450           1.000       0.133
    ## pct_hs18_24                      0.241           0.133       1.000
    ## pct_hs25_over                    0.318           0.102       0.439
    ## pct_bach_deg25_over             -0.132           0.104      -0.405
    ## pct_unemployed16_over           -0.143          -0.551       0.131
    ## pct_private_coverage             0.082           0.449      -0.254
    ## pct_emp_priv_coverage           -0.209           0.233      -0.244
    ## pct_white                        0.398           0.677       0.045
    ## pct_other_race                  -0.267          -0.105      -0.060
    ## pct_married_households           0.222           0.870       0.120
    ## birth_rate                      -0.104           0.141       0.058
    ##                        pct_hs25_over pct_bach_deg25_over
    ## target_death_rate              0.405              -0.485
    ## avg_ann_count                 -0.311               0.321
    ## avg_deaths_per_year           -0.296               0.293
    ## incidence_rate                 0.122              -0.038
    ## pop_est2015                   -0.312               0.297
    ## poverty_percent                0.194              -0.532
    ## median_age_male                0.318              -0.132
    ## percent_married                0.102               0.104
    ## pct_hs18_24                    0.439              -0.405
    ## pct_hs25_over                  1.000              -0.741
    ## pct_bach_deg25_over           -0.741               1.000
    ## pct_unemployed16_over          0.082              -0.373
    ## pct_private_coverage          -0.222               0.603
    ## pct_emp_priv_coverage         -0.223               0.539
    ## pct_white                      0.188               0.049
    ## pct_other_race                -0.286               0.039
    ## pct_married_households         0.062               0.098
    ## birth_rate                     0.017              -0.088
    ##                        pct_unemployed16_over pct_private_coverage
    ## target_death_rate                      0.378               -0.386
    ## avg_ann_count                         -0.009                0.132
    ## avg_deaths_per_year                    0.070                0.056
    ## incidence_rate                         0.100                0.105
    ## pop_est2015                            0.051                0.053
    ## poverty_percent                        0.655               -0.823
    ## median_age_male                       -0.143                0.082
    ## percent_married                       -0.551                0.449
    ## pct_hs18_24                            0.131               -0.254
    ## pct_hs25_over                          0.082               -0.222
    ## pct_bach_deg25_over                   -0.373                0.603
    ## pct_unemployed16_over                  1.000               -0.634
    ## pct_private_coverage                  -0.634                1.000
    ## pct_emp_priv_coverage                 -0.475                0.827
    ## pct_white                             -0.502                0.429
    ## pct_other_race                         0.028               -0.176
    ## pct_married_households                -0.470                0.435
    ## birth_rate                            -0.068               -0.040
    ##                        pct_emp_priv_coverage pct_white pct_other_race
    ## target_death_rate                     -0.267    -0.177         -0.190
    ## avg_ann_count                          0.202    -0.137          0.209
    ## avg_deaths_per_year                    0.160    -0.187          0.215
    ## incidence_rate                         0.150    -0.015         -0.209
    ## pop_est2015                            0.159    -0.190          0.241
    ## poverty_percent                       -0.683    -0.509          0.047
    ## median_age_male                       -0.209     0.398         -0.267
    ## percent_married                        0.233     0.677         -0.105
    ## pct_hs18_24                           -0.244     0.045         -0.060
    ## pct_hs25_over                         -0.223     0.188         -0.286
    ## pct_bach_deg25_over                    0.539     0.049          0.039
    ## pct_unemployed16_over                 -0.475    -0.502          0.028
    ## pct_private_coverage                   0.827     0.429         -0.176
    ## pct_emp_priv_coverage                  1.000     0.270         -0.064
    ## pct_white                              0.270     1.000         -0.234
    ## pct_other_race                        -0.064    -0.234          1.000
    ## pct_married_households                 0.323     0.597         -0.027
    ## birth_rate                            -0.094    -0.009          0.060
    ##                        pct_married_households birth_rate
    ## target_death_rate                      -0.293     -0.087
    ## avg_ann_count                          -0.106     -0.035
    ## avg_deaths_per_year                    -0.160     -0.074
    ## incidence_rate                         -0.152     -0.118
    ## pop_est2015                            -0.128     -0.058
    ## poverty_percent                        -0.605     -0.012
    ## median_age_male                         0.222     -0.104
    ## percent_married                         0.870      0.141
    ## pct_hs18_24                             0.120      0.058
    ## pct_hs25_over                           0.062      0.017
    ## pct_bach_deg25_over                     0.098     -0.088
    ## pct_unemployed16_over                  -0.470     -0.068
    ## pct_private_coverage                    0.435     -0.040
    ## pct_emp_priv_coverage                   0.323     -0.094
    ## pct_white                               0.597     -0.009
    ## pct_other_race                         -0.027      0.060
    ## pct_married_households                  1.000      0.102
    ## birth_rate                              0.102      1.000

``` r
step1=lm(target_death_rate ~ . - avg_ann_count - avg_deaths_per_year, data = seventeen_variable)
summary(step1)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ . - avg_ann_count - avg_deaths_per_year, 
    ##     data = seventeen_variable)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -96.529 -10.783  -0.218  10.644 137.780 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.258e+02  1.069e+01  11.768  < 2e-16 ***
    ## incidence_rate          1.941e-01  7.046e-03  27.550  < 2e-16 ***
    ## pop_est2015            -1.624e-06  1.197e-06  -1.357 0.174994    
    ## poverty_percent         5.032e-01  1.272e-01   3.955 7.83e-05 ***
    ## median_age_male        -4.676e-01  1.016e-01  -4.603 4.34e-06 ***
    ## percent_married         7.758e-01  1.421e-01   5.459 5.18e-08 ***
    ## pct_hs18_24             2.904e-01  4.615e-02   6.292 3.57e-10 ***
    ## pct_hs25_over           3.851e-01  9.342e-02   4.122 3.86e-05 ***
    ## pct_bach_deg25_over    -1.136e+00  1.406e-01  -8.077 9.48e-16 ***
    ## pct_unemployed16_over   5.264e-01  1.533e-01   3.433 0.000604 ***
    ## pct_private_coverage   -5.483e-01  8.872e-02  -6.180 7.29e-10 ***
    ## pct_emp_priv_coverage   3.697e-01  8.316e-02   4.446 9.07e-06 ***
    ## pct_white              -7.160e-02  3.207e-02  -2.233 0.025645 *  
    ## pct_other_race         -9.004e-01  1.192e-01  -7.557 5.44e-14 ***
    ## pct_married_households -8.468e-01  1.251e-01  -6.770 1.55e-11 ***
    ## birth_rate             -9.819e-01  1.908e-01  -5.145 2.84e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.43 on 3031 degrees of freedom
    ## Multiple R-squared:  0.5122, Adjusted R-squared:  0.5098 
    ## F-statistic: 212.2 on 15 and 3031 DF,  p-value: < 2.2e-16

``` r
step2= lm(target_death_rate ~ . - avg_ann_count - pop_est2015, data = seventeen_variable)
summary(step2)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ . - avg_ann_count - pop_est2015, 
    ##     data = seventeen_variable)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -96.160 -10.779  -0.243  10.680 138.094 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.256e+02  1.069e+01  11.742  < 2e-16 ***
    ## avg_deaths_per_year    -4.708e-04  7.845e-04  -0.600 0.548466    
    ## incidence_rate          1.939e-01  7.058e-03  27.478  < 2e-16 ***
    ## poverty_percent         5.077e-01  1.275e-01   3.982 7.00e-05 ***
    ## median_age_male        -4.685e-01  1.017e-01  -4.605 4.29e-06 ***
    ## percent_married         7.763e-01  1.422e-01   5.460 5.15e-08 ***
    ## pct_hs18_24             2.909e-01  4.617e-02   6.302 3.37e-10 ***
    ## pct_hs25_over           3.863e-01  9.344e-02   4.134 3.67e-05 ***
    ## pct_bach_deg25_over    -1.152e+00  1.406e-01  -8.192 3.75e-16 ***
    ## pct_unemployed16_over   5.199e-01  1.536e-01   3.384 0.000723 ***
    ## pct_private_coverage   -5.406e-01  8.879e-02  -6.089 1.28e-09 ***
    ## pct_emp_priv_coverage   3.623e-01  8.323e-02   4.353 1.39e-05 ***
    ## pct_white              -7.004e-02  3.206e-02  -2.185 0.028987 *  
    ## pct_other_race         -9.173e-01  1.190e-01  -7.708 1.72e-14 ***
    ## pct_married_households -8.451e-01  1.252e-01  -6.748 1.79e-11 ***
    ## birth_rate             -9.813e-01  1.909e-01  -5.141 2.91e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.43 on 3031 degrees of freedom
    ## Multiple R-squared:  0.512,  Adjusted R-squared:  0.5096 
    ## F-statistic:   212 on 15 and 3031 DF,  p-value: < 2.2e-16

``` r
step3=lm(target_death_rate ~ . - pop_est2015 - avg_deaths_per_year , data=seventeen_variable)
summary(step3)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ . - pop_est2015 - avg_deaths_per_year, 
    ##     data = seventeen_variable)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -97.532 -10.791  -0.197  10.576 137.294 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.259e+02  1.068e+01  11.784  < 2e-16 ***
    ## avg_ann_count          -6.545e-04  2.772e-04  -2.361 0.018295 *  
    ## incidence_rate          1.952e-01  7.064e-03  27.633  < 2e-16 ***
    ## poverty_percent         4.906e-01  1.273e-01   3.853 0.000119 ***
    ## median_age_male        -4.592e-01  1.016e-01  -4.518 6.47e-06 ***
    ## percent_married         7.827e-01  1.420e-01   5.511 3.88e-08 ***
    ## pct_hs18_24             2.864e-01  4.617e-02   6.204 6.23e-10 ***
    ## pct_hs25_over           3.799e-01  9.340e-02   4.068 4.87e-05 ***
    ## pct_bach_deg25_over    -1.125e+00  1.399e-01  -8.045 1.23e-15 ***
    ## pct_unemployed16_over   5.348e-01  1.532e-01   3.492 0.000486 ***
    ## pct_private_coverage   -5.508e-01  8.834e-02  -6.235 5.15e-10 ***
    ## pct_emp_priv_coverage   3.774e-01  8.299e-02   4.547 5.64e-06 ***
    ## pct_white              -7.302e-02  3.204e-02  -2.279 0.022748 *  
    ## pct_other_race         -8.798e-01  1.190e-01  -7.393 1.84e-13 ***
    ## pct_married_households -8.628e-01  1.253e-01  -6.887 6.90e-12 ***
    ## birth_rate             -9.697e-01  1.908e-01  -5.083 3.94e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.42 on 3031 degrees of freedom
    ## Multiple R-squared:  0.5128, Adjusted R-squared:  0.5104 
    ## F-statistic: 212.7 on 15 and 3031 DF,  p-value: < 2.2e-16

``` r
fifteen_variable = seventeen_variable %>%
  select(-avg_deaths_per_year, - pop_est2015)
```

-   "avg\_deaths\_per\_year", "avg\_ann\_count" and "pop\_est2015" are highly correlated. The last three steps show that we should choose avg\_ann\_count, because the p-value is the smallest, showing significant relation between target\_death\_rate and avg\_ann\_count.
