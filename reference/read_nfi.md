# Read Korean National Forest Inventory

read_nfi() function reads and processes the Korean National Forest
Inventory (NFI). It loads annual NFI files from a local computer,
transforms the data into an analysis-friendly format, and performs data
integrity verification. Users can specify districts and tables to load.
When loading data from the original NFI Excel files, the function will
automatically handle the translation of Korean column names to English -
no manual translation is required. NFI data can be downloaded from
<https://kfss.forest.go.kr/stat/>.

## Usage

``` r
read_nfi(
  dir,
  district = NULL,
  tables = c("tree", "cwd"),
  pattern = "xlsx",
  ...
)
```

## Arguments

- dir:

  : A character vector; The directory containing NFI files.

- district:

  : A character vector; The district names in Korean (sido, sigungu, or
  eupmyondong levels). If `NULL`, the entire dataset is loaded. Combine
  multiple districts using [`c()`](https://rdrr.io/r/base/c.html).

- tables:

  : A character vector; tables to import. Options: 'tree', 'cwd',
  'stump', 'sapling', 'veg', 'herb', 'soil'. Combine multiple tables
  using [`c()`](https://rdrr.io/r/base/c.html). e.g.,
  `c('tree', 'cwd', 'stump', 'sapling', 'veg', 'herb', 'soil')`.

- pattern:

  : A character vector; (default "xlsx"); file pattern to match when
  loading NFI files. Use regular expressions to filter specific files
  (e.g., "NFI5.\*xlsx" for 5th NFI files only)

- ...:

  : Additional arguments to be passed to
  [`list.files()`](https://rdrr.io/r/base/list.files.html)

## Value

A `data.frame`; the processed NFI data, structured for easy analysis.

## Details

The function can load the following tables:

`plot` Base table containing subplot data including site, stand and
non-forest area, among other details (automatically included).

`tree` Tree survey table including species, DBH, and height, among
others. Data is collected from trees and large trees survey plot of
subplot.

`cwd` Coarse woody debris table including species, tree decay level, and
cause of death, among other details. Data is collected only at the
center subplot of the cluster plot.

`stump` Stumps table including species and diameter at 20 cm above the
ground, among other details. Data is collected only at the center
subplot of the cluster.

`sapling` Saplings table including species, diameter at 20 cm above the
ground, and the number of individuals, among other details. Data is
collected only at the sapling survey plot of the subplot.

`veg` Vegetation table (both woody and herbaceous plants). It records
species, number of individuals, and dominance, among others. Data is
collected from three vegetation survey plots located within each
selected center subplot. The selection includes 25% of the total number
of center subplots.

`herb` Herbaceous table focused on the herbaceous list. Data is
collected only at the sapling survey plot of the subplot.

`soil` Soil table including the thickness of the organic layer and soil
depth, among others. Data is collected from three soil survey plots
located within each selected center subplot. The selection includes 25%
of the total number of center subplots.

For more details, refer to the National Forest Inventory guidelines.

This function performs several data integrity validation.

1.  Corrects administrative region information for subplots. (col: SIDO,
    SIDO_CD, SGG, SGG_CD, EMD, EMD_CD)

2.  Adds ecoregion and catchment for subplots. (col: ECOREGION,
    CATCHMENT)

3.  Verifies and corrects coniferous/deciduous classification of tree
    species. (col: CONDEC_CLASS, CONDEC_CLASS_CD, WDY_PLNTS_TYP,
    WDY_PLNTS_TYP_CD)

4.  Adds scientific names for species. (col: SCIENTIFIC_NAME)

5.  Adds Korean and English names for plant families and genera. (col:
    FAMILY, FAMILY_KOREAN, GENUS, GENUS_KOREAN)

6.  Adds whether a plant is native or cultivated, and identifies if it
    is a food, medicinal, fiber, or ornamental resource. (col:
    NATIVE_CULTIVATED, FOOD, MEDICINAL, FIBER, ORNAMENTAL)

7.  Calculates basal area for individual tree (col: BASAL_AREA)

8.  Calculates forest type, dominant species, and dominant species
    percentage for each subplot and cluster plot. (col: FORTYP_SUB,
    DOMIN_PERCNT_SUB, DOMIN_SP_SUB, FORTYP_CLST, DOMIN_PERCNT_CLST,
    DOMIN_SP_CLST) Species classification and taxonomy follow the
    standards set by the Korean Plant Names Index Committee of the Korea
    National Arboretum <http://www.nature.go.kr/kpni/index.do>.

## Note

To manually download subsets of the annual NFI file, visit the Korea
Forest Service Forestry Statistics Platform
(<https://kfss.forest.go.kr/stat/>), download .zip files, and extract
them.

-The 5th National Forest Inventory file:
<https://kfss.forest.go.kr/stat/ptl/article/articleFileDown.do?fileSeq=2995&workSeq=2203>
-The 6th National Forest Inventory file:
<https://kfss.forest.go.kr/stat/ptl/article/articleFileDown.do?fileSeq=2996&workSeq=2204>
-The 7th National Forest Inventory file:
<https://kfss.forest.go.kr/stat/ptl/article/articleFileDown.do?fileSeq=2997&workSeq=2205>

Use `data("nfi_col")` to view the Korean and English names of the column
names.

While the National Forest Inventory undergoes rigorous quality control,
including internal reviews and field inspections, errors may still exist
due to the extensive nature of the survey (approximately 4,000 plots and
over 70 items in the 7th phase). Please use the data cautiously and
report any anomalies to help improve our algorithms.

If you want to save the results to your computer, you can save them in
Excel format. For example, you can use the following
code:`writexl::write_xlsx(data, "data.xlsx")`

If you want to read the saved data back, use the code below:
`path <-"../nfi_donghae.xlsx"`
`sheet_names <- readxl::excel_sheets(path)`
`for (sheet_name in sheet_names) {nfi[[sheet_name]] <- readxl::read_excel(path, sheet = sheet_name) }`

## Examples

``` r
# \donttest{
 # Load tree and CWD data for all districts
 nfi5_data <- read_nfi("D:/NFI/", district = NULL, tables = c("tree", "cwd"), recursive = TRUE)
#> Error in read_nfi("D:/NFI/", district = NULL, tables = c("tree", "cwd"),     recursive = TRUE): Directory  D:/NFI/  does not exist.
# }
```
