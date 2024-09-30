goods_reprice_gradient_numerical <- function(goods, prices,
                                             h = 1e-6) {
  size <- vctrs::vec_size(prices)
  out <- vctrs::vec_init(list(), size)

  for (i in seq_len(size)) {
    prices_i <- vctrs::vec_slice(prices, i)

    prices_plus <- prices
    vctrs::vec_slice(prices_plus$price, i) <- prices_i$price + h

    prices_old <- goods |>
      goods_reprice(prices) |>
      tibble::as_tibble() |>
      dplyr::select(!c("quantity", "utility"))
    prices_new <- goods |>
      goods_reprice(prices_plus) |>
      tibble::as_tibble() |>
      dplyr::select(!c("quantity", "utility"))

    names_prices_new <- setdiff(names(prices_new), "price")
    prices_new <- vctrs::vec_slice(prices_new,
                                   vctrs::vec_match(prices_i[names_prices_new],
                                                    prices_new[names_prices_new]))

    gradient <- prices_i |>
      dplyr::mutate(price = (.env$prices_new$price - .data$price) / h)

    out[[i]] <- prices_new |>
      dplyr::select(!"price") |>
      tibble::add_column(gradient = gradient)
  }
  dplyr::bind_rows(out) |>
    tidyr::unpack("gradient",
                  names_sep = "_") |>
    tidyr::nest(gradient = dplyr::starts_with("gradient"),
                .names_sep = "_")
}

goods_produce_gradient_numerical <- function(goods, quantities, ...,
                                             h = 1e-6) {
  size <- vctrs::vec_size(quantities)
  out <- vctrs::vec_init(list(), size)

  for (i in seq_len(size)) {
    quantities_i <- vctrs::vec_slice(quantities, i)

    quantities_plus <- quantities
    vctrs::vec_slice(quantities_plus$quantity, i) <- quantities_i$quantity + h

    quantities_old <- goods |>
      goods_produce(quantities) |>
      timbr::climb(...) |>
      tibble::as_tibble() |>
      dplyr::ungroup() |>
      dplyr::select(!c("price", "utility")) |>
      dplyr::rename(quantity_old = "quantity")
    quantities_new <- goods |>
      goods_produce(quantities_plus) |>
      timbr::climb(...) |>
      tibble::as_tibble() |>
      dplyr::ungroup() |>
      dplyr::select(!c("price", "utility")) |>
      dplyr::rename(quantity_new = "quantity")

    out[[i]] <- quantities_new |>
      dplyr::left_join(quantities_old,
                       by = dplyr::join_by(...)) |>
      dplyr::inner_join(quantities_i |>
                          dplyr::select(!"quantity"),
                        by = setdiff(names(quantities_i), "quantity")) |>
      dplyr::mutate(gradient = (.data$quantity_new - .data$quantity_old) / h,
                    .keep = "unused")
  }
  dplyr::bind_rows(out)
}
