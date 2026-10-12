test_that("goods_trade_update() requires a `cost` column", {
  goods <- tibble::tibble(region = c("a", "b"), quantity = c(1, 2)) |>
    goods_by(region)

  expect_snapshot(
    goods_trade_update(goods, tibble::tibble(region = "a", value = 2)),
    error = TRUE
  )
})
