# knfi 1.0.2
## Improvements
* Replaced masked coordinate values with zeros in LAT/LON columns for spatial mapping (original values retained in LAT_MASKED/LON_MASKED). Actual locations may differ by up to 100m.

# knfi 1.0.1.9
## Bug fixes
* Fixed a bug in `biomass_nfi`

# knfi 1.0.1
## Bug fixes
* Fixed a bug in `summary_nfi` and `biomass_nfi` where functions were not working when `largetree_area = FALSE`.
* Fixed a filtering bug in `filter_nfi` where plots with the same subplot name in different cycles were not properly filtered at sublayers - resolved by implementing simultaneous filtering using both subplot and cycle.

## Improvements
* `read_nfi`: Added support for additional `list.files()` arguments (e.g., `pattern`, `recursive`) to provide more flexibility in file selection.
* `summary_nfi`, `iv_nfi`, `diversity_nfi`, `biomass_nfi`, `cwd_biomass_nfi`: Added `continuousplot` parameter to filter for plots with measurements across all NFI cycles.
* `tsvis_nfi`: Added layout control parameters (`ncol`, `nrow`) for customizing plot visualization  and `top` parameter for selecting number of top species to display.
*`nfi_col`: Renamed from `col_name` to provide a more descriptive name


# knfi 1.0.0
*  Initial CRAN submission(2024-10-10).
