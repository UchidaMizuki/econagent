test_that("goods_reprice_recursively() works", {
  set.seed(1234)

  industry <- get_industry_iotable_regional(iotable = get_iotable_dummy())

  prices <- get_prices_industry_iotable_regional(industry) |>
    dplyr::filter(.data$input_sector %in% c("industry_1", "industry_2"),
                  .data$output_sector %in% c("industry_1", "industry_2")) |>
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
  industry_new <- industry |>
    goods_reprice(prices) |>
    goods_reprice_recursively(f)

  expect_equal(industry_new, industry,
               tolerance = 1e-6)
})
