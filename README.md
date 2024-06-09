
<!-- README.md is generated from README.Rmd. Please edit that file -->

# econgoods

<!-- badges: start -->
<!-- badges: end -->

econgoods provides utility functions and composite goods in economics.

## Installation

You can install the development version of econgoods from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/econgoods")
```

## Examples

``` r
library(econgoods)
library(tidyverse)
```

### Calibrate utility functions

- Utility functions:
  - Cobb-Douglas: `util_cobb_douglas()`
  - Leontief: `util_leontief()`
  - Constant elasticity of substitution (CES): `util_ces()`
- Calibrate: `util_calibrate()`

``` r
# Sample data
prices <- c(2, 1)
income <- 6

x <- 2
y <- util_2goods_budget(prices, income)(x)
quantities <- c(x, y)
```

``` r
cobb_douglas <- util_cobb_douglas() |> 
  util_calibrate(prices, quantities)
cobb_douglas
#> <Cobb-Douglas>
#> function (quantities, efficiency, weights) 
#> {
#>     efficiency * prod(quantities^weights, na.rm = TRUE)
#> }
#> (
#>   efficiency = 3
#>   weights    = [1] 0.6666667 0.3333333
#>   ...
#> )
```

``` r
leontief <- util_leontief() |> 
  util_calibrate(prices, quantities)
leontief
#> <Leontief>
#> function (quantities, efficiency, weights) 
#> {
#>     efficiency * min(quantities/weights, na.rm = TRUE)
#> }
#> (
#>   efficiency = 1.5
#>   weights    = [1] 0.5 0.5
#>   ...
#> )
```

``` r
ces_0_75 <- util_ces(substitution = 0.75) |> 
  util_calibrate(prices, quantities)
ces_minus_1_5 <- util_ces(substitution = -1.5) |> 
  util_calibrate(prices, quantities)
ces_minus_1_5
#> <CES(-1.5)>
#> function (quantities, substitution, homogeneity, efficiency, 
#>     weights) 
#> {
#>     efficiency * sum(weights * quantities^substitution, na.rm = TRUE)^(homogeneity/substitution)
#> }
#> (
#>   substitution = -1.5
#>   homogeneity  = 1
#>   efficiency   = 3
#>   weights      = [1] 0.6666667 0.3333333
#>   ...
#> )
```

### Indifference curve and budget line for two goods

- `util_2goods_indifference()` returns the function of indifference
  curve.
- `util_2goods_budget()` returns the function of budget line.

``` r
# You need to install dev version of geomtextpath
# - https://github.com/AllanCameron/geomtextpath/issues/114
library(geomtextpath)

theme_set(theme_bw())

limits <- c(0, max(income / prices))
stat_2goods_indifference <- function(f, ...) {
  stat_function(geom = "textpath",
                fun = \(x) util_2goods_indifference(f, quantities)(x) |> 
                  replace_na(1e6),
                label = pillar::obj_sum(f) |> 
                  # https://github.com/AllanCameron/geomtextpath/issues/75
                  str_replace_all("-", "\u2212"),
                hjust = 1,
                xlim = limits,
                ...)
}
stat_2goods_budget <- function(...) {
  stat_function(geom = "textpath",
                fun = util_2goods_budget(prices, income),
                label = "Budget line",
                hjust = 0.2,
                xlim = c(0, income / prices[1]),
                ...)
}
geom_quantities <- function(...) {
  geom_point(aes(quantities[[1]], quantities[[2]]),
             ...)
}
ggplot() +
  geom_hline(yintercept = 0,
             color = "dimgray") +
  geom_vline(xintercept = 0,
             color = "dimgray") +
  stat_2goods_indifference(cobb_douglas,
                           color = "red4") +
  stat_2goods_indifference(leontief,
                           n = 201,
                           color = "red4") +
  stat_2goods_indifference(ces_minus_1_5,
                           color = "red4") +
  stat_2goods_budget(color = "blue4") +
  geom_quantities(color = "blue4",
                  size = 2) +
  scale_x_continuous("Quantity of good X") +
  scale_y_continuous("Quantity of good Y") +
  tune::coord_obs_pred(xlim = limits,
                       ylim = limits)
```

<img src="man/figures/README-plot-indifference-curve-and-budget-line-1.png" width="100%" />

``` r
get_data_2goods_utility <- function(f) {
  f |> 
    purrr::map(
      \(f) {
        expand_grid(x = seq(0, limits[[2]], length.out = 100),
                    y = seq(0, limits[[2]], length.out = 100)) |>
          rowwise() |> 
          mutate(z = f(c(x, y))) |> 
          ungroup()
      }
    ) |> 
    set_names(map_chr(f, pillar::obj_sum)) |> 
    bind_rows(.id = "utility") |> 
    mutate(across(utility, as_factor))
}

get_data_2goods_utility(c(leontief, ces_minus_1_5, cobb_douglas, ces_0_75)) |> 
  ggplot(aes(x, y)) +
  geom_contour_filled(aes(z = z)) +
  stat_2goods_budget(color = "white") +
  geom_quantities(color = "white") +
  scale_x_continuous("Quantity of good X") +
  scale_y_continuous("Quantity of good Y") +
  facet_wrap(~ utility,
             ncol = 2) +
  tune::coord_obs_pred()
```

<img src="man/figures/README-plot-utility-level-1.png" width="100%" />
