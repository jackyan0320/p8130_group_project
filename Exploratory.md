Exploratory
================
12/4/2018

Data manipulation
-----------------

``` r
cancer_df <-
  read_csv("Cancer_Registry.csv") %>% 
  janitor::clean_names() %>%
  
  select(-pct_no_hs18_24, -pct_some_col18_24, -binned_inc, -geography)     # lots of same county names, so I did not remove state column #
 
  
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
    ##          median_age_female         avg_household_size 
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
  select(-pct_employed16_over, -pct_private_coverage_alone) 
  
round(cor(cancer_df),3) 
```

    ##                           avg_ann_count avg_deaths_per_year
    ## avg_ann_count                     1.000               0.939
    ## avg_deaths_per_year               0.939               1.000
    ## target_death_rate                -0.144              -0.091
    ## incidence_rate                    0.074               0.063
    ## med_income                        0.269               0.223
    ## pop_est2015                       0.927               0.978
    ## poverty_percent                  -0.136              -0.067
    ## study_per_cap                     0.082               0.063
    ## median_age                       -0.024              -0.025
    ## median_age_male                  -0.125              -0.148
    ## median_age_female                -0.123              -0.144
    ## avg_household_size                0.065               0.086
    ## percent_married                  -0.106              -0.181
    ## pct_hs18_24                      -0.182              -0.151
    ## pct_bach_deg18_24                 0.284               0.260
    ## pct_hs25_over                    -0.311              -0.296
    ## pct_bach_deg25_over               0.321               0.293
    ## pct_unemployed16_over            -0.009               0.070
    ## pct_private_coverage              0.132               0.056
    ## pct_emp_priv_coverage             0.202               0.160
    ## pct_public_coverage              -0.174              -0.132
    ## pct_public_coverage_alone        -0.094              -0.027
    ## pct_white                        -0.137              -0.187
    ## pct_black                         0.031               0.085
    ## pct_asian                         0.435               0.443
    ## pct_other_race                    0.209               0.215
    ## pct_married_households           -0.106              -0.160
    ## birth_rate                       -0.035              -0.074
    ##                           target_death_rate incidence_rate med_income
    ## avg_ann_count                        -0.144          0.074      0.269
    ## avg_deaths_per_year                  -0.091          0.063      0.223
    ## target_death_rate                     1.000          0.449     -0.429
    ## incidence_rate                        0.449          1.000     -0.001
    ## med_income                           -0.429         -0.001      1.000
    ## pop_est2015                          -0.120          0.027      0.236
    ## poverty_percent                       0.429          0.009     -0.789
    ## study_per_cap                        -0.022          0.077      0.044
    ## median_age                            0.004          0.018     -0.013
    ## median_age_male                      -0.022         -0.015     -0.092
    ## median_age_female                     0.012         -0.009     -0.153
    ## avg_household_size                   -0.037         -0.118      0.112
    ## percent_married                      -0.267         -0.120      0.355
    ## pct_hs18_24                           0.262          0.023     -0.190
    ## pct_bach_deg18_24                    -0.288          0.047      0.493
    ## pct_hs25_over                         0.405          0.122     -0.471
    ## pct_bach_deg25_over                  -0.485         -0.038      0.705
    ## pct_unemployed16_over                 0.378          0.100     -0.453
    ## pct_private_coverage                 -0.386          0.105      0.724
    ## pct_emp_priv_coverage                -0.267          0.150      0.747
    ## pct_public_coverage                   0.405          0.046     -0.755
    ## pct_public_coverage_alone             0.449          0.041     -0.720
    ## pct_white                            -0.177         -0.015      0.167
    ## pct_black                             0.257          0.113     -0.270
    ## pct_asian                            -0.186         -0.008      0.426
    ## pct_other_race                       -0.190         -0.209      0.084
    ## pct_married_households               -0.293         -0.152      0.446
    ## birth_rate                           -0.087         -0.118     -0.010
    ##                           pop_est2015 poverty_percent study_per_cap
    ## avg_ann_count                   0.927          -0.136         0.082
    ## avg_deaths_per_year             0.978          -0.067         0.063
    ## target_death_rate              -0.120           0.429        -0.022
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
    ## pct_white                      -0.190          -0.509         0.023
    ## pct_black                       0.073           0.512        -0.020
    ## pct_asian                       0.464          -0.157         0.063
    ## pct_other_race                  0.241           0.047        -0.015
    ## pct_married_households         -0.128          -0.605        -0.052
    ## birth_rate                     -0.058          -0.012         0.011
    ##                           median_age median_age_male median_age_female
    ## avg_ann_count                 -0.024          -0.125            -0.123
    ## avg_deaths_per_year           -0.025          -0.148            -0.144
    ## target_death_rate              0.004          -0.022             0.012
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
    ## pct_white                      0.035           0.398             0.340
    ## pct_black                     -0.017          -0.243            -0.157
    ## pct_asian                     -0.038          -0.238            -0.259
    ## pct_other_race                -0.030          -0.267            -0.274
    ## pct_married_households         0.015           0.222             0.162
    ## birth_rate                    -0.008          -0.104            -0.099
    ##                           avg_household_size percent_married pct_hs18_24
    ## avg_ann_count                          0.065          -0.106      -0.182
    ## avg_deaths_per_year                    0.086          -0.181      -0.151
    ## target_death_rate                     -0.037          -0.267       0.262
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
    ## pct_white                             -0.188           0.677       0.045
    ## pct_black                              0.030          -0.622      -0.025
    ## pct_asian                              0.132          -0.149      -0.200
    ## pct_other_race                         0.229          -0.105      -0.060
    ## pct_married_households                 0.091           0.870       0.120
    ## birth_rate                             0.076           0.141       0.058
    ##                           pct_bach_deg18_24 pct_hs25_over
    ## avg_ann_count                         0.284        -0.311
    ## avg_deaths_per_year                   0.260        -0.296
    ## target_death_rate                    -0.288         0.405
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
    ## pct_white                             0.069         0.188
    ## pct_black                            -0.094        -0.024
    ## pct_asian                             0.346        -0.437
    ## pct_other_race                        0.007        -0.286
    ## pct_married_households                0.000         0.062
    ## birth_rate                           -0.125         0.017
    ##                           pct_bach_deg25_over pct_unemployed16_over
    ## avg_ann_count                           0.321                -0.009
    ## avg_deaths_per_year                     0.293                 0.070
    ## target_death_rate                      -0.485                 0.378
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
    ## pct_white                               0.049                -0.502
    ## pct_black                              -0.146                 0.469
    ## pct_asian                               0.438                -0.022
    ## pct_other_race                          0.039                 0.028
    ## pct_married_households                  0.098                -0.470
    ## birth_rate                             -0.088                -0.068
    ##                           pct_private_coverage pct_emp_priv_coverage
    ## avg_ann_count                            0.132                 0.202
    ## avg_deaths_per_year                      0.056                 0.160
    ## target_death_rate                       -0.386                -0.267
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
    ## pct_white                                0.429                 0.270
    ## pct_black                               -0.345                -0.237
    ## pct_asian                                0.189                 0.282
    ## pct_other_race                          -0.176                -0.064
    ## pct_married_households                   0.435                 0.323
    ## birth_rate                              -0.040                -0.094
    ##                           pct_public_coverage pct_public_coverage_alone
    ## avg_ann_count                          -0.174                    -0.094
    ## avg_deaths_per_year                    -0.132                    -0.027
    ## target_death_rate                       0.405                     0.449
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
    ## pct_white                              -0.134                    -0.361
    ## pct_black                               0.196                     0.330
    ## pct_asian                              -0.306                    -0.181
    ## pct_other_race                         -0.079                     0.084
    ## pct_married_households                 -0.362                    -0.474
    ## birth_rate                             -0.031                    -0.005
    ##                           pct_white pct_black pct_asian pct_other_race
    ## avg_ann_count                -0.137     0.031     0.435          0.209
    ## avg_deaths_per_year          -0.187     0.085     0.443          0.215
    ## target_death_rate            -0.177     0.257    -0.186         -0.190
    ## incidence_rate               -0.015     0.113    -0.008         -0.209
    ## med_income                    0.167    -0.270     0.426          0.084
    ## pop_est2015                  -0.190     0.073     0.464          0.241
    ## poverty_percent              -0.509     0.512    -0.157          0.047
    ## study_per_cap                 0.023    -0.020     0.063         -0.015
    ## median_age                    0.035    -0.017    -0.038         -0.030
    ## median_age_male               0.398    -0.243    -0.238         -0.267
    ## median_age_female             0.340    -0.157    -0.259         -0.274
    ## avg_household_size           -0.188     0.030     0.132          0.229
    ## percent_married               0.677    -0.622    -0.149         -0.105
    ## pct_hs18_24                   0.045    -0.025    -0.200         -0.060
    ## pct_bach_deg18_24             0.069    -0.094     0.346          0.007
    ## pct_hs25_over                 0.188    -0.024    -0.437         -0.286
    ## pct_bach_deg25_over           0.049    -0.146     0.438          0.039
    ## pct_unemployed16_over        -0.502     0.469    -0.022          0.028
    ## pct_private_coverage          0.429    -0.345     0.189         -0.176
    ## pct_emp_priv_coverage         0.270    -0.237     0.282         -0.064
    ## pct_public_coverage          -0.134     0.196    -0.306         -0.079
    ## pct_public_coverage_alone    -0.361     0.330    -0.181          0.084
    ## pct_white                     1.000    -0.828    -0.266         -0.234
    ## pct_black                    -0.828     1.000     0.017         -0.023
    ## pct_asian                    -0.266     0.017     1.000          0.201
    ## pct_other_race               -0.234    -0.023     0.201          1.000
    ## pct_married_households        0.597    -0.574    -0.087         -0.027
    ## birth_rate                   -0.009    -0.068    -0.062          0.060
    ##                           pct_married_households birth_rate
    ## avg_ann_count                             -0.106     -0.035
    ## avg_deaths_per_year                       -0.160     -0.074
    ## target_death_rate                         -0.293     -0.087
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
    ## pct_white                                  0.597     -0.009
    ## pct_black                                 -0.574     -0.068
    ## pct_asian                                 -0.087     -0.062
    ## pct_other_race                            -0.027      0.060
    ## pct_married_households                     1.000      0.102
    ## birth_rate                                 0.102      1.000

``` r
a=lm(target_death_rate ~ .,data=cancer_df)
summary(a)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ ., data = cancer_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -95.080 -10.795  -0.328  10.859 134.837 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1.154e+02  1.292e+01   8.930  < 2e-16 ***
    ## avg_ann_count             -3.637e-03  7.710e-04  -4.717 2.50e-06 ***
    ## avg_deaths_per_year        1.923e-02  3.893e-03   4.941 8.21e-07 ***
    ## incidence_rate             1.933e-01  7.229e-03  26.736  < 2e-16 ***
    ## med_income                 8.165e-05  7.954e-05   1.027  0.30472    
    ## pop_est2015               -1.611e-05  5.476e-06  -2.941  0.00329 ** 
    ## poverty_percent            6.224e-01  1.537e-01   4.050 5.24e-05 ***
    ## study_per_cap             -8.589e-05  6.772e-04  -0.127  0.89909    
    ## median_age                -2.685e-03  7.832e-03  -0.343  0.73174    
    ## median_age_male           -4.886e-01  2.084e-01  -2.345  0.01909 *  
    ## median_age_female         -2.053e-02  2.138e-01  -0.096  0.92353    
    ## avg_household_size         4.865e-01  9.583e-01   0.508  0.61175    
    ## percent_married            8.897e-01  1.470e-01   6.052 1.61e-09 ***
    ## pct_hs18_24                2.680e-01  4.767e-02   5.623 2.05e-08 ***
    ## pct_bach_deg18_24         -6.077e-02  1.070e-01  -0.568  0.57017    
    ## pct_hs25_over              3.670e-01  9.626e-02   3.812  0.00014 ***
    ## pct_bach_deg25_over       -1.221e+00  1.532e-01  -7.974 2.16e-15 ***
    ## pct_unemployed16_over      3.884e-01  1.603e-01   2.423  0.01546 *  
    ## pct_private_coverage      -4.059e-01  1.272e-01  -3.192  0.00143 ** 
    ## pct_emp_priv_coverage      3.083e-01  1.030e-01   2.994  0.00278 ** 
    ## pct_public_coverage        1.401e-02  2.173e-01   0.064  0.94860    
    ## pct_public_coverage_alone  1.374e-01  2.737e-01   0.502  0.61572    
    ## pct_white                 -9.939e-02  5.696e-02  -1.745  0.08109 .  
    ## pct_black                 -3.732e-02  5.540e-02  -0.674  0.50050    
    ## pct_asian                  6.123e-02  1.877e-01   0.326  0.74423    
    ## pct_other_race            -9.087e-01  1.241e-01  -7.324 3.08e-13 ***
    ## pct_married_households    -9.170e-01  1.397e-01  -6.563 6.20e-11 ***
    ## birth_rate                -9.458e-01  1.929e-01  -4.902 9.99e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.35 on 3019 degrees of freedom
    ## Multiple R-squared:  0.518,  Adjusted R-squared:  0.5137 
    ## F-statistic: 120.2 on 27 and 3019 DF,  p-value: < 2.2e-16
