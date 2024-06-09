
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
  - `util_cobb_douglas()`: Cobb-Douglas utility function
  - `util_leontief()`: Leontief utility function
  - `util_ces()`: Constant elasticity of substitution (CES) utility
    function
- `util_calibrate()`: Fit parameters of utility functions to the given
  prices and quantities.

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
util_2goods_indifference(cobb_douglas, quantities)(1:6)
#> [1] 8.0000000 2.0000000 0.8888889 0.5000000 0.3200000 0.2222222
```

``` r
util_2goods_budget(prices, income)(1:6)
#> [1]  4  2  0 -2 -4 -6
```

#### Advanced plotting examples

<img src="man/figures/README-plot-indifference-curve-and-budget-line-1.png" width="100%" />

<img src="man/figures/README-plot-utility-level-1.png" width="100%" />
