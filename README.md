# legdata

Utility functions for working with Shor legislative data.

## Installation

```r
install.packages("pak")
pak::pak("bshor/legdata")
```

The default installation requires only `lobstr`; data-processing, plotting,
mapping, and modeling packages are optional. A specialized function reports
any missing packages when called, so install those packages only when that
workflow is needed.

For example, `descriptives.st.yr.outcomes()` uses `ggslopegraph`. Install it
separately when needed:

```r
pak::pak("bshor/ggslopegraph")
```
