data\_selection\_tidy\_xy2397
================
Xue Yang
12/7/2018

### Load and manipulate the data

``` r
cancer_df = 
  read_csv(file = "./Cancer_Registry.csv") %>% 
  janitor::clean_names() %>% 
  separate(geography, c("county", "state"), sep = ",") %>% 
  dplyr::select(-pct_no_hs18_24, -pct_some_col18_24, -binned_inc)   
```

### Deal with missing value

``` r
# calculate the number of missing value and the percentage of the missing value of them
missing_value = 
  cancer_df %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(term, num_na, everything()) %>% 
  mutate(percent = num_na/nrow(cancer_df)) 
```
