test_that("goods_reprice() works", {
  set.seed(1234)

  industry <- get_industry_iotable_regional()
  prices <- get_prices_industry_iotable_regional(industry)

  analytic <- industry |>
    goods_reprice(prices,
                  gradient = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select(!c("price", "quantity", "utility"))
  numerical <- goods_reprice_gradient_numerical(goods = industry,
                                                prices = prices)

  expect_equal(analytic, numerical,
               tolerance = 1e-6)
})

test_that("goods_reprice_recursive() works", {
  set.seed(1234)

  industry <- get_industry_iotable_regional()

  prices <- get_prices_industry_iotable_regional(industry) |>
    dplyr::filter(.data$input_sector %in% c("a", "b"),
                  .data$output_sector %in% c("a", "b")) |>
    dplyr::mutate(price = runif(dplyr::n()))

  f <- function(industry) {
    prices_new <- industry |>
      tibble::as_tibble() |>
      dplyr::select("output_sector", "price")

    prices |>
      dplyr::select(!"price") |>
      dplyr::left_join(prices_new,
                       by = "output_sector")
  }
  gradient <- function(industry) {
    prices_new <- industry |>
      tibble::as_tibble() |>
      dplyr::select("output_sector", "prices")

    prices |>
      dplyr::select(!"price") |>
      dplyr::left_join(prices_new,
                       by = "output_sector")
  }

  industry_new <- industry |>
    goods_reprice(prices) |>
    goods_reprice_recursive(f,
                            gradient = gradient)

  expect_equal(industry_new, industry,
               tolerance = 1e-6)
})
