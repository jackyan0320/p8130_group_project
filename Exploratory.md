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


mult.fit <- lm(target_death_rate ~ ., data=cancer_df)     # stepwise regression, and get 17 variables left#
step(mult.fit, direction='both')
```

    ## Start:  AIC=18080.83
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
    ## - median_age_female          1         0 1129754 18079
    ## - pct_public_coverage        1         2 1129756 18079
    ## - study_per_cap              1        12 1129766 18079
    ## - pct_asian                  1        61 1129815 18079
    ## - median_age                 1        77 1129832 18079
    ## - pct_public_coverage_alone  1        91 1129845 18079
    ## - pct_black                  1       142 1129896 18079
    ## - pct_bach_deg18_24          1       155 1129910 18079
    ## - avg_household_size         1       158 1129912 18079
    ## - med_income                 1       571 1130326 18080
    ## <none>                                   1129754 18081
    ## - pct_white                  1      2144 1131898 18085
    ## - median_age_male            1      2239 1131993 18085
    ## - pct_unemployed16_over      1      2474 1132228 18086
    ## - pct_emp_priv_coverage      1      3404 1133158 18088
    ## - pop_est2015                1      3414 1133169 18088
    ## - pct_private_coverage       1      4149 1133903 18090
    ## - pct_hs25_over              1      5219 1134973 18093
    ## - poverty_percent            1      6084 1135838 18095
    ## - avg_ann_count              1      7993 1137747 18100
    ## - birth_rate                 1      8783 1138537 18102
    ## - avg_deaths_per_year        1      9093 1138847 18103
    ## - pct_hs18_24                1     11965 1141719 18111
    ## - percent_married            1     13360 1143114 18115
    ## - pct_married_households     1     17054 1146808 18125
    ## - pct_other_race             1     20934 1150689 18135
    ## - pct_bach_deg25_over        1     23645 1153399 18142
    ## - incidence_rate             1    266796 1396551 18725
    ## 
    ## Step:  AIC=18078.83
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + study_per_cap + 
    ##     median_age + median_age_male + avg_household_size + percent_married + 
    ##     pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage + pct_public_coverage_alone + pct_white + 
    ##     pct_black + pct_asian + pct_other_race + pct_married_households + 
    ##     birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_public_coverage        1         2 1129756 18077
    ## - study_per_cap              1        12 1129766 18077
    ## - pct_asian                  1        61 1129816 18077
    ## - median_age                 1        78 1129832 18077
    ## - pct_public_coverage_alone  1       100 1129854 18077
    ## - pct_black                  1       152 1129906 18077
    ## - pct_bach_deg18_24          1       155 1129910 18077
    ## - avg_household_size         1       160 1129915 18077
    ## - med_income                 1       571 1130326 18078
    ## <none>                                   1129754 18079
    ## + median_age_female          1         0 1129754 18081
    ## - pct_white                  1      2160 1131915 18083
    ## - pct_unemployed16_over      1      2525 1132279 18084
    ## - pct_emp_priv_coverage      1      3405 1133159 18086
    ## - pop_est2015                1      3422 1133176 18086
    ## - pct_private_coverage       1      4150 1133904 18088
    ## - median_age_male            1      5023 1134778 18090
    ## - pct_hs25_over              1      5225 1134980 18091
    ## - poverty_percent            1      6098 1135853 18093
    ## - avg_ann_count              1      8001 1137755 18098
    ## - birth_rate                 1      8784 1138538 18100
    ## - avg_deaths_per_year        1      9113 1138868 18101
    ## - pct_hs18_24                1     11990 1141744 18109
    ## - percent_married            1     13390 1143144 18113
    ## - pct_married_households     1     17129 1146883 18123
    ## - pct_other_race             1     20935 1150689 18133
    ## - pct_bach_deg25_over        1     23650 1153405 18140
    ## - incidence_rate             1    267770 1397524 18725
    ## 
    ## Step:  AIC=18076.83
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + study_per_cap + 
    ##     median_age + median_age_male + avg_household_size + percent_married + 
    ##     pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_black + pct_asian + 
    ##     pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - study_per_cap              1        11 1129767 18075
    ## - pct_asian                  1        62 1129818 18075
    ## - median_age                 1        78 1129834 18075
    ## - pct_black                  1       155 1129911 18075
    ## - pct_bach_deg18_24          1       159 1129915 18075
    ## - avg_household_size         1       159 1129915 18075
    ## - pct_public_coverage_alone  1       402 1130158 18076
    ## - med_income                 1       575 1130331 18076
    ## <none>                                   1129756 18077
    ## + pct_public_coverage        1         2 1129754 18079
    ## + median_age_female          1         0 1129756 18079
    ## - pct_white                  1      2160 1131916 18081
    ## - pct_unemployed16_over      1      2584 1132340 18082
    ## - pop_est2015                1      3449 1133205 18084
    ## - pct_emp_priv_coverage      1      4319 1134075 18087
    ## - pct_hs25_over              1      5224 1134980 18089
    ## - pct_private_coverage       1      5418 1135174 18089
    ## - poverty_percent            1      6116 1135873 18091
    ## - avg_ann_count              1      7999 1137755 18096
    ## - median_age_male            1      8703 1138459 18098
    ## - birth_rate                 1      8790 1138546 18098
    ## - avg_deaths_per_year        1      9153 1138909 18099
    ## - pct_hs18_24                1     12013 1141769 18107
    ## - percent_married            1     13458 1143214 18111
    ## - pct_married_households     1     17139 1146895 18121
    ## - pct_other_race             1     20952 1150708 18131
    ## - pct_bach_deg25_over        1     24517 1154273 18140
    ## - incidence_rate             1    267771 1397527 18723
    ## 
    ## Step:  AIC=18074.86
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age + 
    ##     median_age_male + avg_household_size + percent_married + 
    ##     pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_black + pct_asian + 
    ##     pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_asian                  1        61 1129828 18073
    ## - median_age                 1        77 1129844 18073
    ## - pct_black                  1       153 1129921 18073
    ## - avg_household_size         1       157 1129925 18073
    ## - pct_bach_deg18_24          1       158 1129925 18073
    ## - pct_public_coverage_alone  1       398 1130165 18074
    ## - med_income                 1       593 1130361 18075
    ## <none>                                   1129767 18075
    ## + study_per_cap              1        11 1129756 18077
    ## + pct_public_coverage        1         1 1129766 18077
    ## + median_age_female          1         0 1129767 18077
    ## - pct_white                  1      2160 1131927 18079
    ## - pct_unemployed16_over      1      2582 1132350 18080
    ## - pop_est2015                1      3444 1133212 18082
    ## - pct_emp_priv_coverage      1      4307 1134075 18085
    ## - pct_hs25_over              1      5256 1135023 18087
    ## - pct_private_coverage       1      5435 1135202 18088
    ## - poverty_percent            1      6153 1135920 18089
    ## - avg_ann_count              1      8045 1137813 18095
    ## - median_age_male            1      8709 1138476 18096
    ## - birth_rate                 1      8829 1138596 18097
    ## - avg_deaths_per_year        1      9159 1138926 18098
    ## - pct_hs18_24                1     12003 1141770 18105
    ## - percent_married            1     13480 1143247 18109
    ## - pct_married_households     1     17130 1146898 18119
    ## - pct_other_race             1     20944 1150712 18129
    ## - pct_bach_deg25_over        1     24595 1154362 18139
    ## - incidence_rate             1    268223 1397990 18722
    ## 
    ## Step:  AIC=18073.03
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age + 
    ##     median_age_male + avg_household_size + percent_married + 
    ##     pct_hs18_24 + pct_bach_deg18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_black + pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - median_age                 1        80 1129908 18071
    ## - pct_bach_deg18_24          1       144 1129972 18071
    ## - avg_household_size         1       158 1129987 18072
    ## - pct_black                  1       210 1130038 18072
    ## - pct_public_coverage_alone  1       413 1130241 18072
    ## - med_income                 1       718 1130547 18073
    ## <none>                                   1129828 18073
    ## + pct_asian                  1        61 1129767 18075
    ## + study_per_cap              1        10 1129818 18075
    ## + pct_public_coverage        1         2 1129826 18075
    ## + median_age_female          1         0 1129828 18075
    ## - pct_white                  1      2475 1132304 18078
    ## - pct_unemployed16_over      1      2609 1132437 18078
    ## - pop_est2015                1      3384 1133212 18080
    ## - pct_emp_priv_coverage      1      4319 1134147 18083
    ## - pct_hs25_over              1      5199 1135027 18085
    ## - pct_private_coverage       1      5395 1135223 18086
    ## - poverty_percent            1      6319 1136147 18088
    ## - avg_ann_count              1      8035 1137863 18093
    ## - median_age_male            1      8707 1138535 18094
    ## - birth_rate                 1      8887 1138716 18095
    ## - avg_deaths_per_year        1      9103 1138931 18096
    ## - pct_hs18_24                1     12049 1141877 18103
    ## - percent_married            1     13433 1143261 18107
    ## - pct_married_households     1     17280 1147108 18117
    ## - pct_other_race             1     20997 1150825 18127
    ## - pct_bach_deg25_over        1     24586 1154414 18137
    ## - incidence_rate             1    268199 1398027 18720
    ## 
    ## Step:  AIC=18071.24
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     avg_household_size + percent_married + pct_hs18_24 + pct_bach_deg18_24 + 
    ##     pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + 
    ##     pct_private_coverage + pct_emp_priv_coverage + pct_public_coverage_alone + 
    ##     pct_white + pct_black + pct_other_race + pct_married_households + 
    ##     birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_bach_deg18_24          1       143 1130051 18070
    ## - avg_household_size         1       156 1130063 18070
    ## - pct_black                  1       203 1130111 18070
    ## - pct_public_coverage_alone  1       422 1130330 18070
    ## - med_income                 1       726 1130634 18071
    ## <none>                                   1129908 18071
    ## + median_age                 1        80 1129828 18073
    ## + pct_asian                  1        64 1129844 18073
    ## + study_per_cap              1         9 1129899 18073
    ## + pct_public_coverage        1         2 1129906 18073
    ## + median_age_female          1         0 1129908 18073
    ## - pct_white                  1      2442 1132350 18076
    ## - pct_unemployed16_over      1      2572 1132480 18076
    ## - pop_est2015                1      3405 1133313 18078
    ## - pct_emp_priv_coverage      1      4323 1134231 18081
    ## - pct_hs25_over              1      5211 1135119 18083
    ## - pct_private_coverage       1      5389 1135297 18084
    ## - poverty_percent            1      6361 1136269 18086
    ## - avg_ann_count              1      8024 1137931 18091
    ## - birth_rate                 1      8896 1138804 18093
    ## - median_age_male            1      8931 1138838 18093
    ## - avg_deaths_per_year        1      9134 1139042 18094
    ## - pct_hs18_24                1     12023 1141931 18102
    ## - percent_married            1     13427 1143335 18105
    ## - pct_married_households     1     17252 1147160 18115
    ## - pct_other_race             1     20993 1150900 18125
    ## - pct_bach_deg25_over        1     24575 1154483 18135
    ## - incidence_rate             1    268131 1398039 18718
    ## 
    ## Step:  AIC=18069.63
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     avg_household_size + percent_married + pct_hs18_24 + pct_hs25_over + 
    ##     pct_bach_deg25_over + pct_unemployed16_over + pct_private_coverage + 
    ##     pct_emp_priv_coverage + pct_public_coverage_alone + pct_white + 
    ##     pct_black + pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - avg_household_size         1       153 1130204 18068
    ## - pct_black                  1       197 1130248 18068
    ## - pct_public_coverage_alone  1       395 1130447 18069
    ## - med_income                 1       644 1130695 18069
    ## <none>                                   1130051 18070
    ## + pct_bach_deg18_24          1       143 1129908 18071
    ## + median_age                 1        79 1129972 18071
    ## + pct_asian                  1        50 1130001 18072
    ## + study_per_cap              1         8 1130043 18072
    ## + pct_public_coverage        1         5 1130046 18072
    ## + median_age_female          1         0 1130051 18072
    ## - pct_white                  1      2433 1132485 18074
    ## - pct_unemployed16_over      1      2681 1132732 18075
    ## - pop_est2015                1      3365 1133416 18077
    ## - pct_emp_priv_coverage      1      4310 1134362 18079
    ## - pct_hs25_over              1      5103 1135154 18081
    ## - pct_private_coverage       1      5625 1135676 18083
    ## - poverty_percent            1      6251 1136303 18084
    ## - avg_ann_count              1      8015 1138066 18089
    ## - birth_rate                 1      8775 1138827 18091
    ## - median_age_male            1      9033 1139085 18092
    ## - avg_deaths_per_year        1      9043 1139094 18092
    ## - pct_hs18_24                1     13160 1143212 18103
    ## - percent_married            1     13285 1143336 18103
    ## - pct_married_households     1     17220 1147271 18114
    ## - pct_other_race             1     21121 1151173 18124
    ## - pct_bach_deg25_over        1     26501 1156553 18138
    ## - incidence_rate             1    268249 1398300 18717
    ## 
    ## Step:  AIC=18068.04
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_black + pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_black                  1       210 1130414 18067
    ## - pct_public_coverage_alone  1       390 1130594 18067
    ## - med_income                 1       709 1130914 18068
    ## <none>                                   1130204 18068
    ## + avg_household_size         1       153 1130051 18070
    ## + pct_bach_deg18_24          1       141 1130063 18070
    ## + median_age                 1        76 1130128 18070
    ## + pct_asian                  1        51 1130153 18070
    ## + study_per_cap              1         6 1130198 18070
    ## + pct_public_coverage        1         2 1130202 18070
    ## + median_age_female          1         2 1130203 18070
    ## - pct_white                  1      2460 1132664 18073
    ## - pct_unemployed16_over      1      2756 1132961 18074
    ## - pop_est2015                1      3346 1133551 18075
    ## - pct_emp_priv_coverage      1      4321 1134525 18078
    ## - pct_hs25_over              1      5102 1135307 18080
    ## - pct_private_coverage       1      5890 1136095 18082
    ## - poverty_percent            1      6260 1136465 18083
    ## - avg_ann_count              1      8004 1138209 18088
    ## - birth_rate                 1      8697 1138901 18089
    ## - avg_deaths_per_year        1      9021 1139225 18090
    ## - median_age_male            1      9689 1139894 18092
    ## - percent_married            1     13148 1143352 18101
    ## - pct_hs18_24                1     13321 1143526 18102
    ## - pct_married_households     1     17338 1147543 18112
    ## - pct_other_race             1     20996 1151200 18122
    ## - pct_bach_deg25_over        1     26451 1156655 18137
    ## - incidence_rate             1    268247 1398451 18715
    ## 
    ## Step:  AIC=18066.61
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage_alone + pct_white + pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - pct_public_coverage_alone  1       438 1130852 18066
    ## - med_income                 1       666 1131080 18066
    ## <none>                                   1130414 18067
    ## + pct_black                  1       210 1130204 18068
    ## + avg_household_size         1       166 1130248 18068
    ## + pct_bach_deg18_24          1       134 1130280 18068
    ## + pct_asian                  1       105 1130310 18068
    ## + median_age                 1        70 1130344 18068
    ## + median_age_female          1        14 1130400 18069
    ## + pct_public_coverage        1         7 1130407 18069
    ## + study_per_cap              1         5 1130409 18069
    ## - pct_unemployed16_over      1      2643 1133057 18072
    ## - pct_white                  1      2647 1133061 18072
    ## - pop_est2015                1      3292 1133706 18074
    ## - pct_emp_priv_coverage      1      4303 1134717 18076
    ## - pct_hs25_over              1      5252 1135666 18079
    ## - pct_private_coverage       1      6012 1136427 18081
    ## - poverty_percent            1      6057 1136471 18081
    ## - avg_ann_count              1      7969 1138383 18086
    ## - birth_rate                 1      8637 1139051 18088
    ## - avg_deaths_per_year        1      8942 1139356 18089
    ## - median_age_male            1      9906 1140320 18091
    ## - pct_hs18_24                1     13283 1143698 18100
    ## - percent_married            1     13657 1144071 18101
    ## - pct_married_households     1     17169 1147583 18111
    ## - pct_other_race             1     21055 1151469 18121
    ## - pct_bach_deg25_over        1     26253 1156667 18135
    ## - incidence_rate             1    268037 1398451 18713
    ## 
    ## Step:  AIC=18065.79
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     med_income + pop_est2015 + poverty_percent + median_age_male + 
    ##     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_white + pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## - med_income                 1       664 1131517 18066
    ## <none>                                   1130852 18066
    ## + pct_public_coverage_alone  1       438 1130414 18067
    ## + pct_public_coverage        1       360 1130492 18067
    ## + pct_black                  1       258 1130594 18067
    ## + avg_household_size         1       161 1130692 18067
    ## + pct_asian                  1       133 1130719 18067
    ## + pct_bach_deg18_24          1       106 1130746 18068
    ## + median_age                 1        79 1130774 18068
    ## + median_age_female          1         4 1130848 18068
    ## + study_per_cap              1         2 1130850 18068
    ## - pct_white                  1      2444 1133297 18070
    ## - pct_unemployed16_over      1      3140 1133992 18072
    ## - pop_est2015                1      3466 1134318 18073
    ## - pct_emp_priv_coverage      1      4618 1135470 18076
    ## - pct_hs25_over              1      5542 1136394 18079
    ## - poverty_percent            1      6837 1137689 18082
    ## - avg_ann_count              1      7801 1138653 18085
    ## - birth_rate                 1      8573 1139425 18087
    ## - avg_deaths_per_year        1      9162 1140015 18088
    ## - median_age_male            1      9475 1140327 18089
    ## - pct_private_coverage       1     11662 1142514 18095
    ## - pct_hs18_24                1     13138 1143991 18099
    ## - percent_married            1     13809 1144661 18101
    ## - pct_married_households     1     17754 1148606 18111
    ## - pct_other_race             1     20832 1151684 18119
    ## - pct_bach_deg25_over        1     26495 1157347 18134
    ## - incidence_rate             1    279620 1410472 18737
    ## 
    ## Step:  AIC=18065.58
    ## target_death_rate ~ avg_ann_count + avg_deaths_per_year + incidence_rate + 
    ##     pop_est2015 + poverty_percent + median_age_male + percent_married + 
    ##     pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + 
    ##     pct_private_coverage + pct_emp_priv_coverage + pct_white + 
    ##     pct_other_race + pct_married_households + birth_rate
    ## 
    ##                             Df Sum of Sq     RSS   AIC
    ## <none>                                   1131517 18066
    ## + med_income                 1       664 1130852 18066
    ## + pct_public_coverage_alone  1       437 1131080 18066
    ## + pct_asian                  1       288 1131228 18067
    ## + pct_public_coverage        1       269 1131247 18067
    ## + avg_household_size         1       224 1131293 18067
    ## + pct_black                  1       210 1131307 18067
    ## + median_age                 1        86 1131430 18067
    ## + pct_bach_deg18_24          1        37 1131479 18068
    ## + study_per_cap              1        13 1131503 18068
    ## + median_age_female          1         6 1131511 18068
    ## - pct_white                  1      3212 1134728 18072
    ## - pop_est2015                1      3300 1134817 18073
    ## - pct_unemployed16_over      1      3520 1135037 18073
    ## - pct_hs25_over              1      5015 1136531 18077
    ## - poverty_percent            1      6524 1138041 18081
    ## - pct_emp_priv_coverage      1      6952 1138469 18082
    ## - avg_ann_count              1      7648 1139165 18084
    ## - birth_rate                 1      8224 1139741 18086
    ## - avg_deaths_per_year        1      8888 1140405 18087
    ## - median_age_male            1      9234 1140751 18088
    ## - pct_private_coverage       1     12588 1144105 18097
    ## - percent_married            1     13151 1144668 18099
    ## - pct_hs18_24                1     14115 1145631 18101
    ## - pct_married_households     1     17382 1148899 18110
    ## - pct_other_race             1     20553 1152070 18118
    ## - pct_bach_deg25_over        1     26264 1157781 18134
    ## - incidence_rate             1    280814 1412331 18739

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
    ##              1.214e+02              -3.465e-03               1.877e-02  
    ##         incidence_rate             pop_est2015         poverty_percent  
    ##              1.939e-01              -1.597e-05               5.327e-01  
    ##        median_age_male         percent_married             pct_hs18_24  
    ##             -5.036e-01               8.318e-01               2.823e-01  
    ##          pct_hs25_over     pct_bach_deg25_over   pct_unemployed16_over  
    ##              3.410e-01              -1.173e+00               4.658e-01  
    ##   pct_private_coverage   pct_emp_priv_coverage               pct_white  
    ##             -5.151e-01               3.575e-01              -3.082e+00  
    ##         pct_other_race  pct_married_households              birth_rate  
    ##             -8.809e-01              -8.540e-01              -8.915e-01

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
    ## pct_white                         -0.168        -0.127              -0.175
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
    ## pct_white                      -0.022      -0.181          -0.430
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
    ## pct_white                        0.310           0.545       0.029
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
    ## pct_white                      0.118               0.067
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
    ## pct_white                             -0.399                0.353
    ## pct_other_race                         0.028               -0.176
    ## pct_married_households                -0.470                0.435
    ## birth_rate                            -0.068               -0.040
    ##                        pct_emp_priv_coverage pct_white pct_other_race
    ## target_death_rate                     -0.267    -0.168         -0.190
    ## avg_ann_count                          0.202    -0.127          0.209
    ## avg_deaths_per_year                    0.160    -0.175          0.215
    ## incidence_rate                         0.150    -0.022         -0.209
    ## pop_est2015                            0.159    -0.181          0.241
    ## poverty_percent                       -0.683    -0.430          0.047
    ## median_age_male                       -0.209     0.310         -0.267
    ## percent_married                        0.233     0.545         -0.105
    ## pct_hs18_24                           -0.244     0.029         -0.060
    ## pct_hs25_over                         -0.223     0.118         -0.286
    ## pct_bach_deg25_over                    0.539     0.067          0.039
    ## pct_unemployed16_over                 -0.475    -0.399          0.028
    ## pct_private_coverage                   0.827     0.353         -0.176
    ## pct_emp_priv_coverage                  1.000     0.237         -0.064
    ## pct_white                              0.237     1.000         -0.216
    ## pct_other_race                        -0.064    -0.216          1.000
    ## pct_married_households                 0.323     0.486         -0.027
    ## birth_rate                            -0.094    -0.001          0.060
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
    ## pct_white                               0.486     -0.001
    ## pct_other_race                         -0.027      0.060
    ## pct_married_households                  1.000      0.102
    ## birth_rate                              0.102      1.000

``` r
step1=lm(target_death_rate ~ avg_ann_count  + 
     incidence_rate + pop_est2015 + poverty_percent + median_age_male + 
     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
     pct_white + pct_other_race + pct_married_households + birth_rate, 
     data = seventeen_variable)
summary(step1)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     pop_est2015 + poverty_percent + median_age_male + percent_married + 
    ##     pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + 
    ##     pct_private_coverage + pct_emp_priv_coverage + pct_white + 
    ##     pct_other_race + pct_married_households + birth_rate, data = seventeen_variable)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -99.956 -10.943  -0.364  10.698 136.733 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.234e+02  1.067e+01  11.564  < 2e-16 ***
    ## avg_ann_count          -1.899e-03  6.978e-04  -2.722 0.006534 ** 
    ## incidence_rate          1.961e-01  7.083e-03  27.679  < 2e-16 ***
    ## pop_est2015             5.760e-06  3.016e-06   1.910 0.056242 .  
    ## poverty_percent         4.732e-01  1.274e-01   3.716 0.000206 ***
    ## median_age_male        -4.551e-01  1.012e-01  -4.498 7.12e-06 ***
    ## percent_married         7.842e-01  1.404e-01   5.586 2.53e-08 ***
    ## pct_hs18_24             2.830e-01  4.609e-02   6.140 9.34e-10 ***
    ## pct_hs25_over           3.623e-01  9.331e-02   3.883 0.000105 ***
    ## pct_bach_deg25_over    -1.150e+00  1.403e-01  -8.197 3.59e-16 ***
    ## pct_unemployed16_over   5.427e-01  1.515e-01   3.582 0.000346 ***
    ## pct_private_coverage   -5.429e-01  8.889e-02  -6.107 1.14e-09 ***
    ## pct_emp_priv_coverage   3.739e-01  8.312e-02   4.499 7.09e-06 ***
    ## pct_white              -2.847e+00  1.054e+00  -2.701 0.006951 ** 
    ## pct_other_race         -9.007e-01  1.191e-01  -7.560 5.30e-14 ***
    ## pct_married_households -8.858e-01  1.255e-01  -7.059 2.07e-12 ***
    ## birth_rate             -9.309e-01  1.905e-01  -4.886 1.08e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.4 on 3030 degrees of freedom
    ## Multiple R-squared:  0.5139, Adjusted R-squared:  0.5113 
    ## F-statistic: 200.2 on 16 and 3030 DF,  p-value: < 2.2e-16

``` r
step2= lm(target_death_rate ~ avg_deaths_per_year  + 
     incidence_rate + pop_est2015 + poverty_percent + median_age_male + 
     percent_married + pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + 
     pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage + 
     pct_white + pct_other_race + pct_married_households + birth_rate, 
     data = seventeen_variable)
summary(step2)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_deaths_per_year + incidence_rate + 
    ##     pop_est2015 + poverty_percent + median_age_male + percent_married + 
    ##     pct_hs18_24 + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + 
    ##     pct_private_coverage + pct_emp_priv_coverage + pct_white + 
    ##     pct_other_race + pct_married_households + birth_rate, data = seventeen_variable)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -96.440 -11.088  -0.457  10.743 136.074 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.231e+02  1.067e+01  11.539  < 2e-16 ***
    ## avg_deaths_per_year     1.147e-02  3.505e-03   3.273 0.001075 ** 
    ## incidence_rate          1.913e-01  7.071e-03  27.056  < 2e-16 ***
    ## pop_est2015            -1.885e-05  5.353e-06  -3.521 0.000436 ***
    ## poverty_percent         5.351e-01  1.279e-01   4.185 2.94e-05 ***
    ## median_age_male        -5.093e-01  1.016e-01  -5.013 5.67e-07 ***
    ## percent_married         7.799e-01  1.402e-01   5.564 2.87e-08 ***
    ## pct_hs18_24             2.963e-01  4.596e-02   6.447 1.32e-10 ***
    ## pct_hs25_over           3.674e-01  9.317e-02   3.943 8.23e-05 ***
    ## pct_bach_deg25_over    -1.135e+00  1.401e-01  -8.103 7.68e-16 ***
    ## pct_unemployed16_over   4.961e-01  1.521e-01   3.262 0.001117 ** 
    ## pct_private_coverage   -5.522e-01  8.864e-02  -6.229 5.34e-10 ***
    ## pct_emp_priv_coverage   3.668e-01  8.312e-02   4.413 1.06e-05 ***
    ## pct_white              -3.127e+00  1.054e+00  -2.966 0.003041 ** 
    ## pct_other_race         -9.046e-01  1.190e-01  -7.600 3.92e-14 ***
    ## pct_married_households -8.107e-01  1.252e-01  -6.474 1.11e-10 ***
    ## birth_rate             -9.646e-01  1.899e-01  -5.079 4.02e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.39 on 3030 degrees of freedom
    ## Multiple R-squared:  0.5144, Adjusted R-squared:  0.5118 
    ## F-statistic: 200.6 on 16 and 3030 DF,  p-value: < 2.2e-16

-   "avg\_deaths\_per\_year" and "not avg\_ann\_count" are highly correlated. The last two steps show that we should choose avg\_deaths\_per\_year, not avg\_ann\_count.
