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
