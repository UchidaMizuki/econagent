test_that("goods_produce() works", {
  set.seed(1234)

  industry <- get_industry_iotable_regional(iotable = get_iotable_dummy())
  quantities <- get_quantities_industry_iotable_regional(industry)

  analytic <- industry |>
    goods_produce(quantities,
                  gradient = TRUE) |>
    timbr::climb(output_sector, input_sector) |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::select(!c("price", "quantity", "utility")) |>
    dplyr::arrange(output_sector, input_sector)
  numerical <- goods_produce_gradient_numerical(goods = industry,
                                                quantities = quantities,
                                                output_sector, input_sector) |>
    dplyr::arrange(output_sector, input_sector)

  expect_equal(analytic, numerical,
               tolerance = 1e-6)
})

test_that("goods_produce_recursively() works", {
  set.seed(1234)

  industry <- get_industry_iotable_regional(iotable = get_iotable_dummy())
  final_demand <- get_final_demand_iotable_regional(iotable = get_iotable_dummy()) |>
    dplyr::rename(quantity_final_demand = "quantity")

  quantities <- get_quantities_industry_iotable_regional(industry) |>
    dplyr::filter(.data$output_sector %in% c("a", "b")) |>
    dplyr::mutate(quantity = runif(dplyr::n()))

  f <- function(industry) {
    quantity <- industry |>
      timbr::climb("output_sector", "input_sector") |>
      tibble::as_tibble() |>
      dplyr::ungroup() |>
      dplyr::summarise(quantity_industry = sum(.data$quantity),
                       .by = "input_sector") |>
      dplyr::inner_join(final_demand,
                        by = "input_sector") |>
      dplyr::mutate(quantity = .data$quantity_industry + .data$quantity_final_demand,
                    .keep = "unused") |>
      dplyr::rename(output_sector = "input_sector")

    industry |>
      tibble::as_tibble() |>
      dplyr::select("output_sector", "quantity") |>
      dplyr::rows_update(quantity,
                         by = setdiff(names(quantity), "quantity"),
                         unmatched = "ignore")
  }
  gradient <- function(industry) {
    gradient <- industry |>
      timbr::climb("output_sector", "input_sector") |>
      tibble::as_tibble() |>
      dplyr::ungroup() |>
      dplyr::summarise(gradient = sum(.data$gradient),
                       .by = "input_sector") |>
      dplyr::rename(output_sector = "input_sector")

    industry |>
      tibble::as_tibble() |>
      dplyr::select("output_sector", "gradient") |>
      dplyr::rows_update(gradient,
                         by = setdiff(names(gradient), "gradient"),
                         unmatched = "ignore")
  }

  industry_new <- industry |>
    goods_produce(quantities) |>
    goods_produce_recursively(f,
                              gradient = gradient)

  expect_equal(industry_new, industry,
               tolerance = 1e-6)
})
