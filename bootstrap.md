bootstrapping
================
yz5248
2025-11-13

``` r
n_samp = 250

sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

sim_df_nonconst = sim_df_const |> 
  mutate(
  error = error * .75 * x,
  y = 2 + 3 * x + error
)
```

``` r
sim_df = 
  bind_rows(const = sim_df_const, nonconst = sim_df_nonconst, .id = "data_source") 

sim_df |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = .5) +
  stat_smooth(method = "lm") +
  facet_grid(~data_source) 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](bootstrap_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

look at this data

``` r
sim_df_nonconst |>
  ggplot(aes(x = x, y = y)) +
  geom_point()
```

![](bootstrap_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
sim_df_const |>
  lm(y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    1.977 |     0.098 |    20.157 |       0 |
| x           |    3.045 |     0.070 |    43.537 |       0 |

``` r
sim_df_nonconst |>
  lm(y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    1.934 |     0.105 |    18.456 |       0 |
| x           |    3.112 |     0.075 |    41.661 |       0 |

write a function to draw a bootstrap sample

``` r
boot_sample = function(df) {
  sample_frac(df, size = 1, replace = TRUE)
}

# does it work?
sim_df_nonconst |> 
  boot_sample() |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = .5) +
  stat_smooth(method = "lm") +
  xlim(c(-2,4)) +
  ylim(c(-5, 16))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](bootstrap_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
boot_straps = 
  tibble(
    iter = 1:10
  ) |> 
  mutate(
    bootstrap_sample = map(iter, \(i) boot_sample(df = sim_df_nonconst))
  )

boot_straps
```

    ## # A tibble: 10 × 2
    ##     iter bootstrap_sample  
    ##    <int> <list>            
    ##  1     1 <tibble [250 × 3]>
    ##  2     2 <tibble [250 × 3]>
    ##  3     3 <tibble [250 × 3]>
    ##  4     4 <tibble [250 × 3]>
    ##  5     5 <tibble [250 × 3]>
    ##  6     6 <tibble [250 × 3]>
    ##  7     7 <tibble [250 × 3]>
    ##  8     8 <tibble [250 × 3]>
    ##  9     9 <tibble [250 × 3]>
    ## 10    10 <tibble [250 × 3]>

``` r
boot_straps |> 
  pull(bootstrap_sample) |> 
  nth(2) |>
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(c(-2,4)) +
  ylim(c(-5, 16))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](bootstrap_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

actually run my analysis

``` r
bootstrap_results = 
  boot_straps |>
  mutate(
    fits = map(bootstrap_sample, \(df) lm(y ~ x, data = df)),
    results = map(fits, broom::tidy)
  )
```

look at result

``` r
bootstrap_results |>
  select(iter, results) |>
  unnest(results)
```

    ## # A tibble: 20 × 6
    ##     iter term        estimate std.error statistic   p.value
    ##    <int> <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ##  1     1 (Intercept)     1.90    0.0982      19.3 2.45e- 51
    ##  2     1 x               3.14    0.0688      45.6 1.18e-122
    ##  3     2 (Intercept)     1.89    0.118       16.0 4.78e- 40
    ##  4     2 x               3.12    0.0843      37.0 5.32e-103
    ##  5     3 (Intercept)     2.06    0.0976      21.1 3.71e- 57
    ##  6     3 x               2.97    0.0690      43.1 2.89e-117
    ##  7     4 (Intercept)     2.04    0.102       20.0 9.03e- 54
    ##  8     4 x               3.03    0.0699      43.3 1.43e-117
    ##  9     5 (Intercept)     1.90    0.113       16.8 1.01e- 42
    ## 10     5 x               3.18    0.0772      41.2 7.18e-113
    ## 11     6 (Intercept)     1.95    0.112       17.3 1.22e- 44
    ## 12     6 x               2.97    0.0787      37.8 6.61e-105
    ## 13     7 (Intercept)     1.96    0.112       17.4 5.05e- 45
    ## 14     7 x               3.23    0.0792      40.8 4.11e-112
    ## 15     8 (Intercept)     1.93    0.0910      21.3 8.65e- 58
    ## 16     8 x               3.16    0.0640      49.4 2.91e-130
    ## 17     9 (Intercept)     1.76    0.0979      18.0 7.79e- 47
    ## 18     9 x               3.22    0.0670      48.1 8.76e-128
    ## 19    10 (Intercept)     2.02    0.100       20.1 6.79e- 54
    ## 20    10 x               3.03    0.0746      40.6 1.24e-111

``` r
bootstrap_results |>
  select(iter, results) |>
  unnest(results) |>
  group_by(term) |>
  summarise(
    mean = mean(estimate),
    se = sd(estimate)
  )
```

    ## # A tibble: 2 × 3
    ##   term         mean     se
    ##   <chr>       <dbl>  <dbl>
    ## 1 (Intercept)  1.94 0.0871
    ## 2 x            3.11 0.0986

``` r
bootstrap_results |>
  select(iter, results) |>
  unnest(results) |>
  filter(term == "x") |>
  ggplot(aes(x = estimate)) +
  geom_density()
```

![](bootstrap_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
bootstrap_results |>
  select(iter, results) |>
  unnest(results) |>
  group_by(term) |>
  summarise(
    ci_lower = quantile(estimate, 0.025),
    ci_upper = quantile(estimate, 0.975)
  )
```

    ## # A tibble: 2 × 3
    ##   term        ci_lower ci_upper
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)     1.79     2.05
    ## 2 x               2.97     3.23

## do it again but faster

``` r
sim_df_nonconst |>
  bootstrap(n = 10) |>
  mutate(
    df = map(strap, as_tibble),
    fits = map(df, \(df) lm(y ~ x, data = df)),
    results = map(fits, broom::tidy)
  ) |>
  select(.id, results)|>
  unnest(results)
```

    ## # A tibble: 20 × 6
    ##    .id   term        estimate std.error statistic   p.value
    ##    <chr> <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 01    (Intercept)     2.04    0.0858      23.8 7.12e- 66
    ##  2 01    x               2.90    0.0661      43.9 5.20e-119
    ##  3 02    (Intercept)     2.02    0.107       18.9 6.40e- 50
    ##  4 02    x               3.03    0.0710      42.7 2.83e-116
    ##  5 03    (Intercept)     1.92    0.0957      20.0 8.39e- 54
    ##  6 03    x               3.14    0.0701      44.8 7.28e-121
    ##  7 04    (Intercept)     1.91    0.101       18.9 5.66e- 50
    ##  8 04    x               3.12    0.0689      45.3 6.46e-122
    ##  9 05    (Intercept)     1.94    0.0907      21.3 4.66e- 58
    ## 10 05    x               3.08    0.0680      45.4 3.99e-122
    ## 11 06    (Intercept)     1.95    0.110       17.7 7.92e- 46
    ## 12 06    x               3.13    0.0767      40.9 3.88e-112
    ## 13 07    (Intercept)     1.95    0.0908      21.5 1.22e- 58
    ## 14 07    x               3.03    0.0670      45.3 7.12e-122
    ## 15 08    (Intercept)     1.87    0.115       16.3 4.84e- 41
    ## 16 08    x               3.22    0.0795      40.4 3.54e-111
    ## 17 09    (Intercept)     1.92    0.0965      19.9 2.32e- 53
    ## 18 09    x               3.19    0.0715      44.6 1.71e-120
    ## 19 10    (Intercept)     2.00    0.100       20.0 1.22e- 53
    ## 20 10    x               3.01    0.0694      43.4 8.37e-118

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb |> 
  mutate(stars = review_scores_location / 2) |> 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) |> 
  filter(borough != "Staten Island") |> 
  drop_na(price, stars) |> 
  select(price, stars, borough, neighborhood, room_type)
```

``` r
nyc_airbnb |> 
  ggplot(aes(x = stars, y = price, color = room_type)) + 
  geom_point() 
```

![](bootstrap_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
airbnb_bootstrap_results =
  nyc_airbnb |>
  filter(borough == "Manhattan") |> 
  modelr::bootstrap(n = 10) |> 
  mutate(
    df = map(strap, as_tibble),
    fits = map(df, \(df) lm(price ~ stars + room_type, data = df)),
    results = map(fits, broom::tidy)) |> 
  select(.id, results) |> 
  unnest(results) 
```

``` r
airbnb_bootstrap_results |>
  filter(term == "stars") |> 
  ggplot(aes(x = estimate)) + 
  geom_density()
```

![](bootstrap_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
