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
cancer_df = cancer_df %>%
  select(-pct_employed16_over, -pct_private_coverage_alone) %>%
  mutate(pct_white = ifelse(pct_white > 75, 1, 0))    # 1 represents high percentage of white #
```
