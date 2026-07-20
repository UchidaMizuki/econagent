test_that("goods_compose(utility_negative=) composes mixed-sign quantities", {
  data <- tibble::tibble(
    sector = "total",
    item = c("mfg", "svc", "inventory", "subsidy"),
    price = 1,
    quantity = c(8, 2, -3, -1)
  )

  goods <- data |>
    goods_by(sector, item) |>
    goods_compose(
      util_cobb_douglas(),
      node = "total",
      utility_negative = util_leontief()
    )

  goods_tbl <- goods |> tibble::as_tibble()
  expect_equal(goods_tbl$price, 1)
  expect_equal(goods_tbl$quantity, sum(data$quantity))
})

test_that("goods_compose(utility_negative=) requires a single econ_util object", {
  data <- tibble::tibble(
    sector = "total",
    item = c("mfg", "svc"),
    price = 1,
    quantity = c(8, 2)
  )
  utility <- tibble::tibble(
    sector = "total",
    utility = list(util_cobb_douglas())
  )

  expect_snapshot(
    data |>
      goods_by(sector, item) |>
      goods_compose(
        utility,
        node = "total",
        utility_negative = util_leontief()
      ),
    error = TRUE
  )
})

test_that("goods_produce() preserves sign-split branch ratios", {
  data <- tibble::tibble(
    sector = "total",
    item = c("mfg", "svc", "inventory", "subsidy"),
    price = 1,
    quantity = c(8, 2, -3, -1)
  )

  goods <- data |>
    goods_by(sector, item) |>
    goods_compose(
      util_cobb_douglas(),
      node = "total",
      utility_negative = util_leontief()
    )

  produced <- goods_produce(
    goods,
    tibble::tibble(sector = "total", quantity = 12)
  )
  quantity_new <- produced |>
    timbr::climb("item") |>
    tibble::as_tibble() |>
    dplyr::pull("quantity")

  expect_equal(sum(quantity_new), 12)
  expect_equal(
    sum(quantity_new[1:2]) / sum(quantity_new[3:4]),
    sum(data$quantity[1:2]) / sum(data$quantity[3:4]),
    tolerance = 1e-6
  )
})

test_that("goods_reprice() updates price without reallocating across the sign split", {
  # Both branches use util_leontief() (no substitution) so the reprice
  # formula's cost-minimizing quantities are exactly the calibrated ones,
  # keeping the expected price a simple weighted average to hand-verify.
  data <- tibble::tibble(
    sector = "total",
    item = c("mfg", "svc", "inventory", "subsidy"),
    price = 1,
    quantity = c(8, 2, -3, -1)
  )

  goods <- data |>
    goods_by(sector, item) |>
    goods_compose(
      util_leontief(),
      node = "total",
      utility_negative = util_leontief()
    )

  repriced <- goods_reprice(
    goods,
    tibble::tibble(sector = "total", item = "mfg", price = 1.2)
  )

  quantity_new <- repriced |>
    timbr::climb("item") |>
    tibble::as_tibble() |>
    dplyr::pull("quantity")
  expect_equal(quantity_new, data$quantity)

  price_new <- repriced |> tibble::as_tibble() |> dplyr::pull("price")
  expect_equal(
    price_new,
    sum(c(1.2, 1, 1, 1) * data$quantity) / sum(data$quantity)
  )
})
