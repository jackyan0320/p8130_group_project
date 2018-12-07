Exploratory
================
12/4/2018

Data manipulation
-----------------

``` r
cancer_df <-
  read_csv("Cancer_Registry.csv") %>% 
  janitor::clean_names() %>%
  select(-pct_no_hs18_24, -pct_some_col18_24)
  
colSums(is.na(cancer_df))
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
    ##                  geography         avg_household_size 
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
  select(-pct_employed16_over, -pct_private_coverage_alone)
cancer_df
```

    ## # A tibble: 3,047 x 30
    ##    avg_ann_count avg_deaths_per_~ target_death_ra~ incidence_rate
    ##            <dbl>            <int>            <dbl>          <dbl>
    ##  1          1397              469             165.           490.
    ##  2           173               70             161.           412.
    ##  3           102               50             175.           350.
    ##  4           427              202             195.           430.
    ##  5            57               26             144.           350.
    ##  6           428              152             176            505.
    ##  7           250               97             176.           462.
    ##  8           146               71             184.           404 
    ##  9            88               36             190.           459.
    ## 10          4025             1380             178.           511.
    ## # ... with 3,037 more rows, and 26 more variables: med_income <int>,
    ## #   pop_est2015 <int>, poverty_percent <dbl>, study_per_cap <dbl>,
    ## #   binned_inc <chr>, median_age <dbl>, median_age_male <dbl>,
    ## #   median_age_female <dbl>, geography <chr>, avg_household_size <dbl>,
    ## #   percent_married <dbl>, pct_hs18_24 <dbl>, pct_bach_deg18_24 <dbl>,
    ## #   pct_hs25_over <dbl>, pct_bach_deg25_over <dbl>,
    ## #   pct_unemployed16_over <dbl>, pct_private_coverage <dbl>,
    ## #   pct_emp_priv_coverage <dbl>, pct_public_coverage <dbl>,
    ## #   pct_public_coverage_alone <dbl>, pct_white <dbl>, pct_black <dbl>,
    ## #   pct_asian <dbl>, pct_other_race <dbl>, pct_married_households <dbl>,
    ## #   birth_rate <dbl>
