get_sector <- function() {
  list(industry = factor(letters[1:3]),
       value_added = factor(letters[4:5]))
}

get_industry_iotable_regional <- function() {
  set.seed(1234)

  sector <- get_sector()
  interindustry <- vctrs::vec_expand_grid(output_sector = sector$industry,
                                          input_sector = sector$industry) |>
    dplyr::mutate(price = 1,
                  quantity = runif(dplyr::n())) |>
    goods_by(output_sector, input_sector)

  value_added <- vctrs::vec_expand_grid(output_sector = sector$industry,
                                       input_sector = sector$value_added) |>
    dplyr::mutate(price = 1,
                  quantity = runif(dplyr::n())) |>
    goods_by(output_sector, input_sector) |>
    goods_compose(util_cobb_douglas(),
                  node = factor("value_added"))

  rbind(interindustry, value_added) |>
    goods_compose(util_ces(-5))
}

get_prices_industry_iotable_regional <- function(industry_iotable_regional) {
  industry_iotable_regional |>
    timbr::climb(output_sector, input_sector) |>
    tibble::as_tibble() |>
    dplyr::ungroup() |>
    dplyr::select(output_sector, input_sector, price)
}

get_quantities_industry_iotable_regional <- function(industry_iotable_regional) {
  industry_iotable_regional |>
    tibble::as_tibble() |>
    dplyr::select(output_sector, quantity)
}
