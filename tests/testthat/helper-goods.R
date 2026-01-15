get_iotable_dummy <- function() {
  readRDS(test_path("data", "iotable_dummy.rds"))
}

get_industry_iotable_regional <- function(iotable) {
  intermediate_input <- iotable |>
    dplyr::filter(input_sector_type == "industry",
                  output_sector_type == "industry") |>
    dplyr::select(!c("input_sector_type", "output_sector_type")) |>
    dplyr::rename(input_sector = "input_sector_name",
                  output_sector = "output_sector_name") |>
    tibble::add_column(price = 1,
                       .before = "value") |>
    dplyr::rename(quantity = value) |>
    goods_by(output_sector, input_sector)

  value_added <- iotable |>
    dplyr::filter(input_sector_type %in% c("import", "value_added"),
                  output_sector_type == "industry") |>
    dplyr::select(!c("input_sector_type", "output_sector_type")) |>
    dplyr::rename(input_sector = "input_sector_name",
                  output_sector = "output_sector_name") |>
    tibble::add_column(price = 1,
                       .before = "value") |>
    dplyr::rename(quantity = value) |>
    goods_by(output_sector, input_sector) |>
    goods_compose(util_cobb_douglas(),
                  node = factor("value_added"))

  rbind(intermediate_input, value_added) |>
    goods_compose(util_leontief())
}

get_final_demand_iotable_regional <- function(iotable) {
  iotable |>
    dplyr::filter(input_sector_type == "industry",
                  output_sector_type %in% c("final_demand", "export")) |>
    dplyr::select(!c("input_sector_type", "output_sector_type")) |>
    dplyr::summarise(quantity = sum(.data$value),
                     .by = "input_sector_name") |>
    dplyr::rename(input_sector = "input_sector_name")
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
