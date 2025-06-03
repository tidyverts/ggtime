# ggtime 0.1.0

Initial release including all plot helper functions from across the tidy time
series analysis packages. The following plot helper functions are included in
this release:

* `autoplot(<tbl_ts>)`: Time plots to show overall patterns in tsibble objects.
* `autolayer(<tbl_ts>)`: Time plot layers for tsibble objects.
* `gg_season()`: Seasonal plots to show the shape of seasonal patterns.
* `gg_subseries()`: Seasonal sub-series plots to show seasonal changes over time.
* `gg_lag()`: Lag plots to show relationships between now and the past.
* `gg_irf()`: Impulse response function plots to be used with `IRF()` results.
* `gg_arma()`: Plot the characteristic ARMA roots.
* `gg_tsdisplay()`: An ensemble graphic useful in exploring time series data.
* `gg_tsresiduals()`: An ensemble graphic useful in diagnosing model residuals.
