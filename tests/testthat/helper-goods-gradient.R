goods_reprice_gradient_numerical <- function(goods, prices,
                                             h = 1e-6) {
  size <- vctrs::vec_size(prices)
  out <- vctrs::vec_init(list(), size)

  for (i in seq_len(size)) {
    prices_i <- vctrs::vec_slice(prices, i)

    prices_plus <- prices
    vctrs::vec_slice(prices_plus$price, i) <- prices_i$price + h

    prices_old <- goods |>
      tibble::as_tibble() |>
      dplyr::select(!c("quantity", "utility"))
    prices_new <- goods |>
      goods_reprice(prices_plus) |>
      tibble::as_tibble() |>
      dplyr::select(!c("quantity", "utility"))

    prices_new <- vctrs::vec_slice(prices_new, prices_new$price != prices_old$price)

    prices_i <- prices_i |>
      dplyr::mutate(price = (.env$prices_new$price - .data$price) / h)

    out[[i]] <- prices_new |>
      dplyr::select(!"price") |>
      tibble::add_column(prices = prices_i)
  }
  dplyr::bind_rows(out) |>
    tidyr::unpack("prices",
                  names_sep = "_") |>
    tidyr::nest(prices = dplyr::starts_with("prices"),
                .names_sep = "_")
}
