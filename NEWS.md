# econagent (development version)

* `util_calibrate()` now errors when `quantities` contains negative values, instead of silently returning `Inf`/`NaN` weights or a meaningless `efficiency` (#2).
