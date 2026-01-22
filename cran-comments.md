## R CMD check results

0 errors | 0 warnings | 1 notes

1. note
To use the argument output="map" in the tsvis_nfi function, administrative district sf data is required. Since the sf data is large, a separate data package called kadmin was created.
When using output="map", if the kadmin package is not available, a stop message is set to appear.

You have informed the user of the following:
"To visualize data as a map, you need to agree to install the kadmin package during the function execution or install it in advance. The kadmin package loads shapefiles for Korea's Si, Do, or Si, Gun, Gu, or Eup, Myeon, Dong.
Use drat::addRepo("SYOUNG9836") install.packages("kadmin") or remotes::install_github("SYOUNG9836/kadmin")."

```
❯ checking package dependencies ... NOTE
  Package suggested but not available for checking: 'kadmin'
```

## check_win_devel results

0 errors | 0 warnings | 0 notes


## rhub Status: Success

linux (R-devel)
macos-arm64 (R-devel)
macos (R-devel)
windows (R-devel)



