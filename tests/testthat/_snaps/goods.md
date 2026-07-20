# goods_compose(utility_negative=) requires a single econ_util object

    Code
      goods_compose(goods_by(data, sector, item), utility, node = "total",
      utility_negative = util_leontief())
    Condition
      Error in `goods_compose()`:
      ! `utility_negative` requires `utility` to be a single <econ_util> object.

