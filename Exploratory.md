

Data manipulation


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
  select(-pct_employed16_over, - pct_private_coverage_alone, - binned_inc)

  
                            

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
cor(cancer_df)
```

    ##                           avg_ann_count avg_deaths_per_year
    ## avg_ann_count               1.000000000          0.93940778
    ## avg_deaths_per_year         0.939407783          1.00000000
    ## target_death_rate          -0.143531620         -0.09071516
    ## incidence_rate              0.073553175          0.06268986
    ## med_income                  0.269144676          0.22320676
    ## pop_est2015                 0.926893538          0.97763406
    ## poverty_percent            -0.135693914         -0.06691794
    ## study_per_cap               0.082071379          0.06348833
    ## median_age                 -0.024097510         -0.02459872
    ## median_age_male            -0.124968609         -0.14848720
    ## median_age_female          -0.122844098         -0.14406921
    ## avg_household_size          0.064787793          0.08616148
    ## percent_married            -0.106107711         -0.18102911
    ## pct_no_hs18_24             -0.143326877         -0.13679416
    ## pct_hs18_24                -0.182053929         -0.15141783
    ## pct_some_col18_24                    NA                  NA
    ## pct_bach_deg18_24           0.284176205          0.25976080
    ## pct_hs25_over              -0.311375212         -0.29592941
    ## pct_bach_deg25_over         0.321020553          0.29320978
    ## pct_unemployed16_over      -0.009015804          0.06970063
    ## pct_private_coverage        0.132244379          0.05618256
    ## pct_emp_priv_coverage       0.202348916          0.16012370
    ## pct_public_coverage        -0.173548301         -0.13168651
    ## pct_public_coverage_alone  -0.093699079         -0.02733797
    ## pct_white                  -0.136501141         -0.18715902
    ## pct_black                   0.031375602          0.08460710
    ## pct_asian                   0.435071173          0.44307423
    ## pct_other_race              0.209183775          0.21514936
    ## pct_married_households     -0.106220868         -0.16026613
    ## birth_rate                 -0.034507632         -0.07442001
    ##                           target_death_rate incidence_rate   med_income
    ## avg_ann_count                  -0.143531620    0.073553175  0.269144676
    ## avg_deaths_per_year            -0.090715160    0.062689857  0.223206757
    ## target_death_rate               1.000000000    0.449431698 -0.428614927
    ## incidence_rate                  0.449431698    1.000000000 -0.001036186
    ## med_income                     -0.428614927   -0.001036186  1.000000000
    ## pop_est2015                    -0.120073096    0.026912352  0.235522860
    ## poverty_percent                 0.429388980    0.009046252 -0.788965239
    ## study_per_cap                  -0.022285011    0.077282631  0.044002767
    ## median_age                      0.004375077    0.018089172 -0.013287743
    ## median_age_male                -0.021929429   -0.014733235 -0.091662642
    ## median_age_female               0.012048386   -0.009105564 -0.153278401
    ## avg_household_size             -0.036905314   -0.118399973  0.112065314
    ## percent_married                -0.266820464   -0.119524484  0.355122865
    ## pct_no_hs18_24                  0.088462610   -0.170762076 -0.289383120
    ## pct_hs18_24                     0.261975940    0.022643795 -0.190005681
    ## pct_some_col18_24                        NA             NA           NA
    ## pct_bach_deg18_24              -0.287817410    0.046835423  0.492810246
    ## pct_hs25_over                   0.404589076    0.121724595 -0.471348267
    ## pct_bach_deg25_over            -0.485477318   -0.038177165  0.704928245
    ## pct_unemployed16_over           0.378412442    0.099979455 -0.453107661
    ## pct_private_coverage           -0.386065507    0.105174269  0.724174768
    ## pct_emp_priv_coverage          -0.267399428    0.149824526  0.747293549
    ## pct_public_coverage             0.404571656    0.046108610 -0.754821751
    ## pct_public_coverage_alone       0.449357576    0.040812287 -0.719756152
    ## pct_white                      -0.177399980   -0.014509829  0.167225441
    ## pct_black                       0.257023560    0.113488959 -0.270231619
    ## pct_asian                      -0.186331105   -0.008123427  0.425844240
    ## pct_other_race                 -0.189893571   -0.208748336  0.083634870
    ## pct_married_households         -0.293325341   -0.152176321  0.446082895
    ## birth_rate                     -0.087406970   -0.118181288 -0.010194553
    ##                           pop_est2015 poverty_percent study_per_cap
    ## avg_ann_count              0.92689354    -0.135693914   0.082071379
    ## avg_deaths_per_year        0.97763406    -0.066917939   0.063488331
    ## target_death_rate         -0.12007310     0.429388980  -0.022285011
    ## incidence_rate             0.02691235     0.009046252   0.077282631
    ## med_income                 0.23552286    -0.788965239   0.044002767
    ## pop_est2015                1.00000000    -0.065299150   0.055721518
    ## poverty_percent           -0.06529915     1.000000000  -0.055652350
    ## study_per_cap              0.05572152    -0.055652350   1.000000000
    ## median_age                -0.02521899    -0.029279996  -0.026029802
    ## median_age_male           -0.17660764    -0.214001049  -0.036647292
    ## median_age_female         -0.17793232    -0.148163541  -0.030577044
    ## avg_household_size         0.10994045     0.074307601  -0.004070887
    ## percent_married           -0.16046328    -0.642856868  -0.038143262
    ## pct_no_hs18_24            -0.12658242     0.288106366  -0.090387320
    ## pct_hs18_24               -0.15182121     0.094211082  -0.057035136
    ## pct_some_col18_24                  NA              NA            NA
    ## pct_bach_deg18_24          0.24837541    -0.387121904   0.063819117
    ## pct_hs25_over             -0.31184921     0.194361157  -0.085127983
    ## pct_bach_deg25_over        0.29746337    -0.531599691   0.108593794
    ## pct_unemployed16_over      0.05076814     0.655148122  -0.031956813
    ## pct_private_coverage       0.05267651    -0.822534292   0.092544651
    ## pct_emp_priv_coverage      0.15864952    -0.683099657   0.100063185
    ## pct_public_coverage       -0.16006562     0.651162060  -0.051496680
    ## pct_public_coverage_alone -0.04146881     0.798642030  -0.055511989
    ## pct_white                 -0.19009450    -0.509432808   0.023291042
    ## pct_black                  0.07304407     0.511529663  -0.019761153
    ## pct_asian                  0.46416779    -0.157288704   0.062543075
    ## pct_other_race             0.24146800     0.047095893  -0.015247481
    ## pct_married_households    -0.12797946    -0.604952784  -0.051735616
    ## birth_rate                -0.05774018    -0.012282511   0.010676193
    ##                             median_age median_age_male median_age_female
    ## avg_ann_count             -0.024097510    -0.124968609      -0.122844098
    ## avg_deaths_per_year       -0.024598722    -0.148487199      -0.144069211
    ## target_death_rate          0.004375077    -0.021929429       0.012048386
    ## incidence_rate             0.018089172    -0.014733235      -0.009105564
    ## med_income                -0.013287743    -0.091662642      -0.153278401
    ## pop_est2015               -0.025218994    -0.176607643      -0.177932323
    ## poverty_percent           -0.029279996    -0.214001049      -0.148163541
    ## study_per_cap             -0.026029802    -0.036647292      -0.030577044
    ## median_age                 1.000000000     0.129119478       0.124678372
    ## median_age_male            0.129119478     1.000000000       0.933696103
    ## median_age_female          0.124678372     0.933696103       1.000000000
    ## avg_household_size        -0.031944148    -0.343188659      -0.367585149
    ## percent_married            0.046371506     0.449986173       0.375207983
    ## pct_no_hs18_24             0.006178084     0.100485523       0.136361328
    ## pct_hs18_24                0.050573668     0.241309928       0.242827279
    ## pct_some_col18_24                   NA              NA                NA
    ## pct_bach_deg18_24         -0.016909407    -0.034135247      -0.070698993
    ## pct_hs25_over              0.036587378     0.318277051       0.344839719
    ## pct_bach_deg25_over       -0.020352194    -0.131599355      -0.180845331
    ## pct_unemployed16_over      0.018590443    -0.142737472      -0.111161313
    ## pct_private_coverage       0.004665111     0.082231778       0.046909158
    ## pct_emp_priv_coverage     -0.036926459    -0.208663968      -0.252221140
    ## pct_public_coverage        0.049060211     0.398967231       0.455496465
    ## pct_public_coverage_alone -0.003297872     0.002478719       0.047659145
    ## pct_white                  0.035009366     0.398044362       0.339803910
    ## pct_black                 -0.017173240    -0.242748132      -0.156728442
    ## pct_asian                 -0.038423911    -0.238322374      -0.258747912
    ## pct_other_race            -0.030276508    -0.266655447      -0.274119578
    ## pct_married_households     0.014503609     0.222277744       0.161506831
    ## birth_rate                -0.008276233    -0.104105160      -0.098812608
    ##                           avg_household_size percent_married
    ## avg_ann_count                    0.064787793     -0.10610771
    ## avg_deaths_per_year              0.086161477     -0.18102911
    ## target_death_rate               -0.036905314     -0.26682046
    ## incidence_rate                  -0.118399973     -0.11952448
    ## med_income                       0.112065314      0.35512286
    ## pop_est2015                      0.109940447     -0.16046328
    ## poverty_percent                  0.074307601     -0.64285687
    ## study_per_cap                   -0.004070887     -0.03814326
    ## median_age                      -0.031944148      0.04637151
    ## median_age_male                 -0.343188659      0.44998617
    ## median_age_female               -0.367585149      0.37520798
    ## avg_household_size               1.000000000     -0.10051170
    ## percent_married                 -0.100511698      1.00000000
    ## pct_no_hs18_24                   0.064718590     -0.01237458
    ## pct_hs18_24                      0.027228204      0.13279244
    ## pct_some_col18_24                         NA              NA
    ## pct_bach_deg18_24               -0.060960847      0.05303732
    ## pct_hs25_over                   -0.138728398      0.10243370
    ## pct_bach_deg25_over              0.013917803      0.10358519
    ## pct_unemployed16_over            0.131506325     -0.55148349
    ## pct_private_coverage            -0.144390600      0.44945161
    ## pct_emp_priv_coverage            0.011111227      0.23289907
    ## pct_public_coverage             -0.134812156     -0.24697154
    ## pct_public_coverage_alone        0.061114735     -0.45998992
    ## pct_white                       -0.188445815      0.67741994
    ## pct_black                        0.030277977     -0.62235733
    ## pct_asian                        0.131535433     -0.14869134
    ## pct_other_race                   0.229439641     -0.10466945
    ## pct_married_households           0.091450373      0.87026054
    ## birth_rate                       0.075917596      0.14140393
    ##                           pct_no_hs18_24 pct_hs18_24 pct_some_col18_24
    ## avg_ann_count               -0.143326877 -0.18205393                NA
    ## avg_deaths_per_year         -0.136794157 -0.15141783                NA
    ## target_death_rate            0.088462610  0.26197594                NA
    ## incidence_rate              -0.170762076  0.02264379                NA
    ## med_income                  -0.289383120 -0.19000568                NA
    ## pop_est2015                 -0.126582418 -0.15182121                NA
    ## poverty_percent              0.288106366  0.09421108                NA
    ## study_per_cap               -0.090387320 -0.05703514                NA
    ## median_age                   0.006178084  0.05057367                NA
    ## median_age_male              0.100485523  0.24130993                NA
    ## median_age_female            0.136361328  0.24282728                NA
    ## avg_household_size           0.064718590  0.02722820                NA
    ## percent_married             -0.012374580  0.13279244                NA
    ## pct_no_hs18_24               1.000000000  0.08462928                NA
    ## pct_hs18_24                  0.084629285  1.00000000                NA
    ## pct_some_col18_24                     NA          NA                 1
    ## pct_bach_deg18_24           -0.381422016 -0.38933391                NA
    ## pct_hs25_over                0.217069496  0.43892915                NA
    ## pct_bach_deg25_over         -0.396578614 -0.40475397                NA
    ## pct_unemployed16_over        0.181193218  0.13069406                NA
    ## pct_private_coverage        -0.454750805 -0.25385075                NA
    ## pct_emp_priv_coverage       -0.429994050 -0.24449415                NA
    ## pct_public_coverage          0.318540309  0.27822049                NA
    ## pct_public_coverage_alone    0.327269783  0.23412398                NA
    ## pct_white                   -0.157282267  0.04530637                NA
    ## pct_black                    0.116805155 -0.02486791                NA
    ## pct_asian                   -0.217534569 -0.19977046                NA
    ## pct_other_race               0.126256354 -0.06041485                NA
    ## pct_married_households       0.005339552  0.12004023                NA
    ## birth_rate                   0.125894802  0.05822688                NA
    ##                           pct_bach_deg18_24 pct_hs25_over
    ## avg_ann_count                  0.2841762046   -0.31137521
    ## avg_deaths_per_year            0.2597607979   -0.29592941
    ## target_death_rate             -0.2878174102    0.40458908
    ## incidence_rate                 0.0468354233    0.12172459
    ## med_income                     0.4928102457   -0.47134827
    ## pop_est2015                    0.2483754141   -0.31184921
    ## poverty_percent               -0.3871219044    0.19436116
    ## study_per_cap                  0.0638191165   -0.08512798
    ## median_age                    -0.0169094070    0.03658738
    ## median_age_male               -0.0341352465    0.31827705
    ## median_age_female             -0.0706989935    0.34483972
    ## avg_household_size            -0.0609608467   -0.13872840
    ## percent_married                0.0530373214    0.10243370
    ## pct_no_hs18_24                -0.3814220162    0.21706950
    ## pct_hs18_24                   -0.3893339119    0.43892915
    ## pct_some_col18_24                        NA            NA
    ## pct_bach_deg18_24              1.0000000000   -0.38404878
    ## pct_hs25_over                 -0.3840487785    1.00000000
    ## pct_bach_deg25_over            0.5998141845   -0.74061122
    ## pct_unemployed16_over         -0.3089196535    0.08230552
    ## pct_private_coverage           0.4877417395   -0.22193481
    ## pct_emp_priv_coverage          0.4509960515   -0.22280299
    ## pct_public_coverage           -0.4224703062    0.42797377
    ## pct_public_coverage_alone     -0.4218045942    0.29714338
    ## pct_white                      0.0691328203    0.18804475
    ## pct_black                     -0.0936139964   -0.02444526
    ## pct_asian                      0.3458827707   -0.43656094
    ## pct_other_race                 0.0065469377   -0.28561114
    ## pct_married_households        -0.0001044447    0.06217592
    ## birth_rate                    -0.1250734830    0.01660026
    ##                           pct_bach_deg25_over pct_unemployed16_over
    ## avg_ann_count                      0.32102055          -0.009015804
    ## avg_deaths_per_year                0.29320978           0.069700627
    ## target_death_rate                 -0.48547732           0.378412442
    ## incidence_rate                    -0.03817717           0.099979455
    ## med_income                         0.70492824          -0.453107661
    ## pop_est2015                        0.29746337           0.050768138
    ## poverty_percent                   -0.53159969           0.655148122
    ## study_per_cap                      0.10859379          -0.031956813
    ## median_age                        -0.02035219           0.018590443
    ## median_age_male                   -0.13159935          -0.142737472
    ## median_age_female                 -0.18084533          -0.111161313
    ## avg_household_size                 0.01391780           0.131506325
    ## percent_married                    0.10358519          -0.551483488
    ## pct_no_hs18_24                    -0.39657861           0.181193218
    ## pct_hs18_24                       -0.40475397           0.130694061
    ## pct_some_col18_24                          NA                    NA
    ## pct_bach_deg18_24                  0.59981418          -0.308919654
    ## pct_hs25_over                     -0.74061122           0.082305516
    ## pct_bach_deg25_over                1.00000000          -0.372980047
    ## pct_unemployed16_over             -0.37298005           1.000000000
    ## pct_private_coverage               0.60324766          -0.634317281
    ## pct_emp_priv_coverage              0.53908363          -0.474745168
    ## pct_public_coverage               -0.63609480           0.529821296
    ## pct_public_coverage_alone         -0.60575990           0.655365736
    ## pct_white                          0.04865228          -0.501755245
    ## pct_black                         -0.14640875           0.469273102
    ## pct_asian                          0.43796288          -0.022020273
    ## pct_other_race                     0.03907545           0.028463247
    ## pct_married_households             0.09813386          -0.469609014
    ## birth_rate                        -0.08794027          -0.067906273
    ##                           pct_private_coverage pct_emp_priv_coverage
    ## avg_ann_count                      0.132244379            0.20234892
    ## avg_deaths_per_year                0.056182557            0.16012370
    ## target_death_rate                 -0.386065507           -0.26739943
    ## incidence_rate                     0.105174269            0.14982453
    ## med_income                         0.724174768            0.74729355
    ## pop_est2015                        0.052676513            0.15864952
    ## poverty_percent                   -0.822534292           -0.68309966
    ## study_per_cap                      0.092544651            0.10006319
    ## median_age                         0.004665111           -0.03692646
    ## median_age_male                    0.082231778           -0.20866397
    ## median_age_female                  0.046909158           -0.25222114
    ## avg_household_size                -0.144390600            0.01111123
    ## percent_married                    0.449451608            0.23289907
    ## pct_no_hs18_24                    -0.454750805           -0.42999405
    ## pct_hs18_24                       -0.253850745           -0.24449415
    ## pct_some_col18_24                           NA                    NA
    ## pct_bach_deg18_24                  0.487741739            0.45099605
    ## pct_hs25_over                     -0.221934807           -0.22280299
    ## pct_bach_deg25_over                0.603247665            0.53908363
    ## pct_unemployed16_over             -0.634317281           -0.47474517
    ## pct_private_coverage               1.000000000            0.82745884
    ## pct_emp_priv_coverage              0.827458844            1.00000000
    ## pct_public_coverage               -0.720011521           -0.77831482
    ## pct_public_coverage_alone         -0.886233694           -0.72882303
    ## pct_white                          0.429031447            0.26981502
    ## pct_black                         -0.345172126           -0.23738803
    ## pct_asian                          0.189331755            0.28248429
    ## pct_other_race                    -0.176300307           -0.06422598
    ## pct_married_households             0.434640055            0.32256933
    ## birth_rate                        -0.040436613           -0.09387800
    ##                           pct_public_coverage pct_public_coverage_alone
    ## avg_ann_count                     -0.17354830              -0.093699079
    ## avg_deaths_per_year               -0.13168651              -0.027337969
    ## target_death_rate                  0.40457166               0.449357576
    ## incidence_rate                     0.04610861               0.040812287
    ## med_income                        -0.75482175              -0.719756152
    ## pop_est2015                       -0.16006562              -0.041468807
    ## poverty_percent                    0.65116206               0.798642030
    ## study_per_cap                     -0.05149668              -0.055511989
    ## median_age                         0.04906021              -0.003297872
    ## median_age_male                    0.39896723               0.002478719
    ## median_age_female                  0.45549646               0.047659145
    ## avg_household_size                -0.13481216               0.061114735
    ## percent_married                   -0.24697154              -0.459989923
    ## pct_no_hs18_24                     0.31854031               0.327269783
    ## pct_hs18_24                        0.27822049               0.234123984
    ## pct_some_col18_24                          NA                        NA
    ## pct_bach_deg18_24                 -0.42247031              -0.421804594
    ## pct_hs25_over                      0.42797377               0.297143381
    ## pct_bach_deg25_over               -0.63609480              -0.605759903
    ## pct_unemployed16_over              0.52982130               0.655365736
    ## pct_private_coverage              -0.72001152              -0.886233694
    ## pct_emp_priv_coverage             -0.77831482              -0.728823026
    ## pct_public_coverage                1.00000000               0.865832788
    ## pct_public_coverage_alone          0.86583279               1.000000000
    ## pct_white                         -0.13370507              -0.361026352
    ## pct_black                          0.19559747               0.330110279
    ## pct_asian                         -0.30562546              -0.181380191
    ## pct_other_race                    -0.07870778               0.083755384
    ## pct_married_households            -0.36217051              -0.473993882
    ## birth_rate                        -0.03053076              -0.004752695
    ##                              pct_white   pct_black    pct_asian
    ## avg_ann_count             -0.136501141  0.03137560  0.435071173
    ## avg_deaths_per_year       -0.187159023  0.08460710  0.443074226
    ## target_death_rate         -0.177399980  0.25702356 -0.186331105
    ## incidence_rate            -0.014509829  0.11348896 -0.008123427
    ## med_income                 0.167225441 -0.27023162  0.425844240
    ## pop_est2015               -0.190094503  0.07304407  0.464167791
    ## poverty_percent           -0.509432808  0.51152966 -0.157288704
    ## study_per_cap              0.023291042 -0.01976115  0.062543075
    ## median_age                 0.035009366 -0.01717324 -0.038423911
    ## median_age_male            0.398044362 -0.24274813 -0.238322374
    ## median_age_female          0.339803910 -0.15672844 -0.258747912
    ## avg_household_size        -0.188445815  0.03027798  0.131535433
    ## percent_married            0.677419940 -0.62235733 -0.148691337
    ## pct_no_hs18_24            -0.157282267  0.11680516 -0.217534569
    ## pct_hs18_24                0.045306371 -0.02486791 -0.199770462
    ## pct_some_col18_24                   NA          NA           NA
    ## pct_bach_deg18_24          0.069132820 -0.09361400  0.345882771
    ## pct_hs25_over              0.188044752 -0.02444526 -0.436560938
    ## pct_bach_deg25_over        0.048652281 -0.14640875  0.437962881
    ## pct_unemployed16_over     -0.501755245  0.46927310 -0.022020273
    ## pct_private_coverage       0.429031447 -0.34517213  0.189331755
    ## pct_emp_priv_coverage      0.269815023 -0.23738803  0.282484289
    ## pct_public_coverage       -0.133705071  0.19559747 -0.305625464
    ## pct_public_coverage_alone -0.361026352  0.33011028 -0.181380191
    ## pct_white                  1.000000000 -0.82845885 -0.265676411
    ## pct_black                 -0.828458852  1.00000000  0.016583407
    ## pct_asian                 -0.265676411  0.01658341  1.000000000
    ## pct_other_race            -0.233692379 -0.02300131  0.200781103
    ## pct_married_households     0.596771068 -0.57359245 -0.086602036
    ## birth_rate                -0.008958097 -0.06780483 -0.061946987
    ##                           pct_other_race pct_married_households
    ## avg_ann_count                0.209183775          -0.1062208681
    ## avg_deaths_per_year          0.215149359          -0.1602661296
    ## target_death_rate           -0.189893571          -0.2933253405
    ## incidence_rate              -0.208748336          -0.1521763205
    ## med_income                   0.083634870           0.4460828953
    ## pop_est2015                  0.241468004          -0.1279794627
    ## poverty_percent              0.047095893          -0.6049527844
    ## study_per_cap               -0.015247481          -0.0517356157
    ## median_age                  -0.030276508           0.0145036093
    ## median_age_male             -0.266655447           0.2222777445
    ## median_age_female           -0.274119578           0.1615068308
    ## avg_household_size           0.229439641           0.0914503733
    ## percent_married             -0.104669448           0.8702605365
    ## pct_no_hs18_24               0.126256354           0.0053395517
    ## pct_hs18_24                 -0.060414849           0.1200402276
    ## pct_some_col18_24                     NA                     NA
    ## pct_bach_deg18_24            0.006546938          -0.0001044447
    ## pct_hs25_over               -0.285611137           0.0621759178
    ## pct_bach_deg25_over          0.039075451           0.0981338597
    ## pct_unemployed16_over        0.028463247          -0.4696090139
    ## pct_private_coverage        -0.176300307           0.4346400550
    ## pct_emp_priv_coverage       -0.064225979           0.3225693263
    ## pct_public_coverage         -0.078707776          -0.3621705058
    ## pct_public_coverage_alone    0.083755384          -0.4739938818
    ## pct_white                   -0.233692379           0.5967710681
    ## pct_black                   -0.023001308          -0.5735924510
    ## pct_asian                    0.200781103          -0.0866020358
    ## pct_other_race               1.000000000          -0.0273522962
    ## pct_married_households      -0.027352296           1.0000000000
    ## birth_rate                   0.059829476           0.1022633001
    ##                             birth_rate
    ## avg_ann_count             -0.034507632
    ## avg_deaths_per_year       -0.074420014
    ## target_death_rate         -0.087406970
    ## incidence_rate            -0.118181288
    ## med_income                -0.010194553
    ## pop_est2015               -0.057740178
    ## poverty_percent           -0.012282511
    ## study_per_cap              0.010676193
    ## median_age                -0.008276233
    ## median_age_male           -0.104105160
    ## median_age_female         -0.098812608
    ## avg_household_size         0.075917596
    ## percent_married            0.141403930
    ## pct_no_hs18_24             0.125894802
    ## pct_hs18_24                0.058226877
    ## pct_some_col18_24                   NA
    ## pct_bach_deg18_24         -0.125073483
    ## pct_hs25_over              0.016600264
    ## pct_bach_deg25_over       -0.087940271
    ## pct_unemployed16_over     -0.067906273
    ## pct_private_coverage      -0.040436613
    ## pct_emp_priv_coverage     -0.093878001
    ## pct_public_coverage       -0.030530759
    ## pct_public_coverage_alone -0.004752695
    ## pct_white                 -0.008958097
    ## pct_black                 -0.067804827
    ## pct_asian                 -0.061946987
    ## pct_other_race             0.059829476
    ## pct_married_households     0.102263300
    ## birth_rate                 1.000000000

``` r
a=cancer_df %>%
  select(target_death_rate, median_age , median_age_male, median_age_female) 
lm(target_death_rate ~ median_age + median_age_male+ median_age_female, data=a)%>%
  summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ median_age + median_age_male + 
    ##     median_age_female, data = a)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -114.936  -17.956   -0.653   16.245  194.886 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       176.987749   4.024148  43.981  < 2e-16 ***
    ## median_age          0.003839   0.011150   0.344    0.731    
    ## median_age_male    -1.377503   0.267834  -5.143 2.87e-07 ***
    ## median_age_female   1.329004   0.264301   5.028 5.23e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 27.64 on 3043 degrees of freedom
    ## Multiple R-squared:  0.00877,    Adjusted R-squared:  0.007793 
    ## F-statistic: 8.974 on 3 and 3043 DF,  p-value: 6.456e-06

``` r
b=cancer_df %>%
  select(target_death_rate, pct_private_coverage,pct_emp_priv_coverage,pct_public_coverage,pct_public_coverage_alone)
lm(target_death_rate ~pct_private_coverage+pct_emp_priv_coverage+pct_public_coverage+pct_public_coverage_alone, data=b)  %>%
  summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_private_coverage + pct_emp_priv_coverage + 
    ##     pct_public_coverage + pct_public_coverage_alone, data = b)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -121.762  -13.692    0.574   14.226  178.854 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               134.0180     8.6254  15.538  < 2e-16 ***
    ## pct_private_coverage       -0.8224     0.1380  -5.958 2.84e-09 ***
    ## pct_emp_priv_coverage       1.0398     0.1141   9.114  < 2e-16 ***
    ## pct_public_coverage         1.1673     0.1576   7.408 1.65e-13 ***
    ## pct_public_coverage_alone   0.6454     0.2676   2.412   0.0159 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.46 on 3042 degrees of freedom
    ## Multiple R-squared:  0.2245, Adjusted R-squared:  0.2235 
    ## F-statistic: 220.1 on 4 and 3042 DF,  p-value: < 2.2e-16

``` r
c=cancer_df %>%
  select(target_death_rate, pct_white , pct_black, pct_asian , pct_other_race)
lm(target_death_rate ~ pct_white + pct_black+ pct_asian + pct_other_race,data=c) %>%
  summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_white + pct_black + pct_asian + 
    ##     pct_other_race, data = c)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -114.852  -15.281   -0.395   14.563  176.720 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    212.46569    6.04343  35.156  < 2e-16 ***
    ## pct_white       -0.35241    0.06276  -5.615 2.14e-08 ***
    ## pct_black        0.15958    0.06696   2.383   0.0172 *  
    ## pct_asian       -2.16556    0.20095 -10.777  < 2e-16 ***
    ## pct_other_race  -1.54376    0.14926 -10.343  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.85 on 3042 degrees of freedom
    ## Multiple R-squared:  0.1335, Adjusted R-squared:  0.1324 
    ## F-statistic: 117.2 on 4 and 3042 DF,  p-value: < 2.2e-16

``` r
lm(target_death_rate ~ percent_married, data = cancer_df) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ percent_married, data = cancer_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -133.752  -16.671   -0.593   15.726  167.630 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     234.24926    3.67052   63.82   <2e-16 ***
    ## percent_married  -1.07362    0.07027  -15.28   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.75 on 3045 degrees of freedom
    ## Multiple R-squared:  0.07119,    Adjusted R-squared:  0.07089 
    ## F-statistic: 233.4 on 1 and 3045 DF,  p-value: < 2.2e-16

``` r
lm(target_death_rate ~ pct_married_households, data = cancer_df) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_married_households, data = cancer_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -138.206  -16.530   -0.909   15.680  182.611 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            242.12797    3.77911   64.07   <2e-16 ***
    ## pct_married_households  -1.23847    0.07315  -16.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.54 on 3045 degrees of freedom
    ## Multiple R-squared:  0.08604,    Adjusted R-squared:  0.08574 
    ## F-statistic: 286.7 on 1 and 3045 DF,  p-value: < 2.2e-16

``` r
raw_data=cancer_df %>%
  select(-avg_deaths_per_year, -pop_est2015, -pct_no_hs18_24 , - pct_hs18_24 , - pct_bach_deg18_24 , - pct_some_col18_24 , - median_age, - pct_private_coverage, - pct_public_coverage, -pct_public_coverage_alone, -percent_married , - birth_rate)
```

-   First we remove two variables with lots of missing values "pct\_employed16\_over" and "pct\_private\_coverage\_alone".
-   Then we remove "binned\_inc" and "birth\_rate" because ?????
-   "avg\_deaths\_per\_year", "avg\_ann\_count" and "pop\_est2015" are highly correlated. The last three steps show that we should choose avg\_ann\_count, because the p-value is the smallest, showing significant relation between target\_death\_rate and avg\_ann\_count.
-   Finally we plan to choose at least one variable from each category. We fit each variables in MLR and find the variable with small p-value. Finally we get 17 variables left and get raw\_data.
