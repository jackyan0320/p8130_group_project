Model Improvement
================
Bihui Sun
December 15, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
cancer_df =
  read_csv("Cancer_Registry.csv") %>% 
  janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   avgDeathsPerYear = col_integer(),
    ##   medIncome = col_integer(),
    ##   popEst2015 = col_integer(),
    ##   binnedInc = col_character(),
    ##   Geography = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
raw_data =
  cancer_df %>%
  dplyr::select(-pct_employed16_over, - pct_private_coverage_alone, -binned_inc) %>% 
  dplyr::select(-geography, -avg_deaths_per_year, -pop_est2015, -pct_no_hs18_24 , -pct_hs18_24 , -pct_bach_deg18_24, -pct_some_col18_24, -median_age, -pct_private_coverage, -pct_public_coverage, -pct_public_coverage_alone, -percent_married , -birth_rate) %>% 
  dplyr::select(target_death_rate, everything()) 

raw_data2 = raw_data %>%
 mutate(pct_other_race_1 = ifelse(pct_other_race > 0.8262, 1, 0))
```

``` r
fit2 = lm(target_death_rate ~ avg_ann_count + incidence_rate + poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + pct_other_race + pct_married_households, data = raw_data)
summary(fit2)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_other_race + pct_married_households, 
    ##     data = raw_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -114.139  -11.354   -0.354   11.046  141.058 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             1.054e+02  9.202e+00  11.453  < 2e-16 ***
    ## avg_ann_count          -5.528e-04  2.813e-04  -1.965 0.049451 *  
    ## incidence_rate          1.960e-01  7.024e-03  27.908  < 2e-16 ***
    ## poverty_percent         6.354e-01  1.017e-01   6.245 4.83e-10 ***
    ## median_age_female      -2.851e-01  7.621e-02  -3.741 0.000187 ***
    ## pct_hs25_over           4.834e-01  9.059e-02   5.336 1.02e-07 ***
    ## pct_bach_deg25_over    -1.314e+00  1.342e-01  -9.788  < 2e-16 ***
    ## pct_unemployed16_over   7.821e-01  1.427e-01   5.479 4.62e-08 ***
    ## pct_other_race         -6.730e-01  1.174e-01  -5.731 1.10e-08 ***
    ## pct_married_households -3.349e-01  7.578e-02  -4.420 1.02e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.86 on 3037 degrees of freedom
    ## Multiple R-squared:  0.4895, Adjusted R-squared:  0.4879 
    ## F-statistic: 323.5 on 9 and 3037 DF,  p-value: < 2.2e-16

``` r
fit2_bin = lm(target_death_rate ~ avg_ann_count + incidence_rate + poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + pct_other_race_1 + pct_married_households, data = raw_data2)
summary(fit2_bin)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_other_race_1 + pct_married_households, 
    ##     data = raw_data2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -117.321  -11.484   -0.227   11.260  142.115 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            95.8434519  9.1561417  10.468  < 2e-16 ***
    ## avg_ann_count          -0.0008095  0.0002777  -2.915  0.00358 ** 
    ## incidence_rate          0.2017466  0.0069587  28.992  < 2e-16 ***
    ## poverty_percent         0.6388390  0.1024007   6.239 5.03e-10 ***
    ## median_age_female      -0.2406558  0.0764833  -3.147  0.00167 ** 
    ## pct_hs25_over           0.5682237  0.0904472   6.282 3.81e-10 ***
    ## pct_bach_deg25_over    -1.1662794  0.1315128  -8.868  < 2e-16 ***
    ## pct_unemployed16_over   0.8351130  0.1431375   5.834 5.97e-09 ***
    ## pct_other_race_1       -2.2869395  0.8107649  -2.821  0.00482 ** 
    ## pct_married_households -0.3408406  0.0761237  -4.477 7.83e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.94 on 3037 degrees of freedom
    ## Multiple R-squared:  0.4853, Adjusted R-squared:  0.4838 
    ## F-statistic: 318.2 on 9 and 3037 DF,  p-value: < 2.2e-16

-   From the previous two rmd we know that we should choose the model 2 according to parsimony. Now we want to try interaction based on model 2. We recode the pct\_other\_race to 0 and 1 based on median. 0 indicates fewer other race people in this county, and 1 indicates more other race people in this county. We try to interact the new variable pct\_other\_race\_1 to other predictors one by one and see if there is association between race and these predictors.

``` r
try1 = lm(target_death_rate ~ avg_ann_count + incidence_rate + poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + pct_married_households + pct_other_race_1 + pct_other_race_1 * avg_ann_count, data = raw_data2)
summary(try1)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_married_households + pct_other_race_1 + 
    ##     pct_other_race_1 * avg_ann_count, data = raw_data2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -117.966  -11.488   -0.336   11.342  142.025 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    96.9358352  9.1863700  10.552  < 2e-16 ***
    ## avg_ann_count                  -0.0019203  0.0008248  -2.328  0.01996 *  
    ## incidence_rate                  0.2021858  0.0069642  29.032  < 2e-16 ***
    ## poverty_percent                 0.6280222  0.1026620   6.117 1.07e-09 ***
    ## median_age_female              -0.2408542  0.0764703  -3.150  0.00165 ** 
    ## pct_hs25_over                   0.5615072  0.0905534   6.201 6.38e-10 ***
    ## pct_bach_deg25_over            -1.1701008  0.1315173  -8.897  < 2e-16 ***
    ## pct_unemployed16_over           0.8239888  0.1433240   5.749 9.86e-09 ***
    ## pct_married_households         -0.3468538  0.0762266  -4.550 5.57e-06 ***
    ## pct_other_race_1               -2.8132826  0.8902454  -3.160  0.00159 ** 
    ## avg_ann_count:pct_other_race_1  0.0012236  0.0008555   1.430  0.15274    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.94 on 3036 degrees of freedom
    ## Multiple R-squared:  0.4856, Adjusted R-squared:  0.4839 
    ## F-statistic: 286.6 on 10 and 3036 DF,  p-value: < 2.2e-16

-   First we try race and avg\_ann\_count. The p-value is 0.153.

``` r
try2 = lm(target_death_rate ~ avg_ann_count + incidence_rate + poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + pct_married_households + pct_other_race_1 + pct_other_race_1 * incidence_rate, data = raw_data2)
summary(try2)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_married_households + pct_other_race_1 + 
    ##     pct_other_race_1 * incidence_rate, data = raw_data2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -105.665  -11.539   -0.226   11.306  141.583 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      1.064e+02  9.725e+00  10.943  < 2e-16 ***
    ## avg_ann_count                   -8.377e-04  2.774e-04  -3.020 0.002551 ** 
    ## incidence_rate                   1.803e-01  9.671e-03  18.642  < 2e-16 ***
    ## poverty_percent                  6.696e-01  1.027e-01   6.520 8.20e-11 ***
    ## median_age_female               -2.554e-01  7.651e-02  -3.338 0.000853 ***
    ## pct_hs25_over                    5.428e-01  9.066e-02   5.987 2.39e-09 ***
    ## pct_bach_deg25_over             -1.191e+00  1.315e-01  -9.054  < 2e-16 ***
    ## pct_unemployed16_over            8.088e-01  1.432e-01   5.650 1.75e-08 ***
    ## pct_married_households          -3.256e-01  7.616e-02  -4.275 1.97e-05 ***
    ## pct_other_race_1                -2.179e+01  6.166e+00  -3.533 0.000417 ***
    ## incidence_rate:pct_other_race_1  4.329e-02  1.357e-02   3.190 0.001438 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.91 on 3036 degrees of freedom
    ## Multiple R-squared:  0.487,  Adjusted R-squared:  0.4853 
    ## F-statistic: 288.2 on 10 and 3036 DF,  p-value: < 2.2e-16

-   The p-value for pct\_other\_race\_1 \* incidence\_rate is 0.0014, indicating the race modifies the relationship between incidece rate and target mortality rate.

``` r
try3 = lm(target_death_rate ~ avg_ann_count + incidence_rate + poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + pct_married_households + pct_other_race_1 + pct_other_race_1 * poverty_percent, data = raw_data2)
summary(try3)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_married_households + pct_other_race_1 + 
    ##     pct_other_race_1 * poverty_percent, data = raw_data2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -115.02  -11.44   -0.05   11.26  141.07 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                      94.4494705  9.0916447  10.389  < 2e-16
    ## avg_ann_count                    -0.0008183  0.0002756  -2.969  0.00301
    ## incidence_rate                    0.1968358  0.0069459  28.339  < 2e-16
    ## poverty_percent                   1.0000739  0.1148089   8.711  < 2e-16
    ## median_age_female                -0.2207619  0.0759820  -2.905  0.00369
    ## pct_hs25_over                     0.5632461  0.0897900   6.273 4.05e-10
    ## pct_bach_deg25_over              -1.2211542  0.1308043  -9.336  < 2e-16
    ## pct_unemployed16_over             0.7261620  0.1430013   5.078 4.04e-07
    ## pct_married_households           -0.3728734  0.0757161  -4.925 8.90e-07
    ## pct_other_race_1                 10.8260327  2.0976723   5.161 2.61e-07
    ## poverty_percent:pct_other_race_1 -0.7724087  0.1141047  -6.769 1.55e-11
    ##                                     
    ## (Intercept)                      ***
    ## avg_ann_count                    ** 
    ## incidence_rate                   ***
    ## poverty_percent                  ***
    ## median_age_female                ** 
    ## pct_hs25_over                    ***
    ## pct_bach_deg25_over              ***
    ## pct_unemployed16_over            ***
    ## pct_married_households           ***
    ## pct_other_race_1                 ***
    ## poverty_percent:pct_other_race_1 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.79 on 3036 degrees of freedom
    ## Multiple R-squared:  0.4929, Adjusted R-squared:  0.4913 
    ## F-statistic: 295.1 on 10 and 3036 DF,  p-value: < 2.2e-16

-   P-value is very small.

``` r
try4 = lm(target_death_rate ~ avg_ann_count + incidence_rate + poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + pct_unemployed16_over + pct_married_households + pct_other_race_1 + pct_other_race_1 * pct_hs25_over, data = raw_data2)
summary(try4)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ avg_ann_count + incidence_rate + 
    ##     poverty_percent + median_age_female + pct_hs25_over + pct_bach_deg25_over + 
    ##     pct_unemployed16_over + pct_married_households + pct_other_race_1 + 
    ##     pct_other_race_1 * pct_hs25_over, data = raw_data2)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -118.577  -11.413   -0.096   11.237  143.179 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    98.1578767  9.3684351  10.478  < 2e-16 ***
    ## avg_ann_count                  -0.0007630  0.0002805  -2.720  0.00656 ** 
    ## incidence_rate                  0.2012442  0.0069716  28.866  < 2e-16 ***
    ## poverty_percent                 0.6366589  0.1024118   6.217 5.77e-10 ***
    ## median_age_female              -0.2454882  0.0765911  -3.205  0.00136 ** 
    ## pct_hs25_over                   0.5075886  0.1043350   4.865 1.20e-06 ***
    ## pct_bach_deg25_over            -1.1552495  0.1318450  -8.762  < 2e-16 ***
    ## pct_unemployed16_over           0.8510945  0.1437842   5.919 3.60e-09 ***
    ## pct_married_households         -0.3378887  0.0761613  -4.436 9.47e-06 ***
    ## pct_other_race_1               -6.9121788  4.0500438  -1.707  0.08798 .  
    ## pct_hs25_over:pct_other_race_1  0.1317460  0.1130272   1.166  0.24386    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.94 on 3036 degrees of freedom
    ## Multiple R-squared:  0.4855, Adjusted R-squared:  0.4838 
    ## F-statistic: 286.5 on 10 and 3036 DF,  p-value: < 2.2e-16

-   P-value is too big.
