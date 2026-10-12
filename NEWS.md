# econagent (development version)

* econagent now requires R >= 4.1.0, since it uses the native pipe and lambda syntax (#5).

* `util_calibrate()` now errors when `quantities` contains negative values, instead of silently returning `Inf`/`NaN` weights or a meaningless `efficiency` (#2).
