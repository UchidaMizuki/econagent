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
