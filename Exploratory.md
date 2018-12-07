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
  select(-pct_no_hs18_24, -pct_some_col18_24) %>%     # lots of same county names, so I did not remove state column #
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
    ##                 binned_inc                 median_age 
    ##                          0                          0 
    ##            median_age_male          median_age_female 
    ##                          0                          0 
    ##                     county                      state 
    ##                          0                          0 
    ##         avg_household_size            percent_married 
    ##                          0                          0 
    ##                pct_hs18_24          pct_bach_deg18_24 
    ##                          0                          0 
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
  select(-pct_employed16_over, -pct_private_coverage_alone)
```
