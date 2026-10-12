# goods_trade_update() requires a `cost` column

    Code
      goods_trade_update(goods, tibble::tibble(region = "a", value = 2))
    Condition
      Error in `goods_trade_update()`:
      ! `trade` must contain a `cost` column.

