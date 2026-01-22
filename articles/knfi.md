# knfi

``` r
library(knfi)

# Load tree and CWD data for all districts
nfi5_data <- read_nfi("D:/NFI/NFI5", district = NULL, tables = c("tree", "cwd"), recursive = TRUE)

# Applying hierarchical filtering to select only privately owned forest subplots.
# Ensures all child tables' subplots match the filtered plot table's subplots.
nfi5_data <- filter_nfi(nfi5_data, c("plot$OWN_CD == '5'"), hier = TRUE)

# Switch column names from English to original Korean names
nfi5_data_kor <- switchcol_nfi(nfi5_data)
```

``` r
library(knfi)

# The Korean and English names of the column names
data("nfi_col")

# National Forest Inventory data for Donghae-si, Gangwon-do, Republic of Korea for testing the function
data("nfi_donghae")

# calculates comprehensive descriptive statistics for study area
summary_stats <- summary_nfi(nfi_donghae, continuousplot = T)

# Calculate importance values using genus
importance_genus <- iv_nfi(nfi_donghae, sp = "GENUS", continuousplot = T)
#> Warning: no DISPLAY variable so Tk is not available
#> Warning in Sys.setLanguage(substring(lang, 1, 2)): in a C locale: cannot set
#> language

# Calculate tree diversity indices using basal area
diversity_tree_ba <- diversity_nfi(nfi_donghae, sp = "SP", table = "tree", basal = TRUE, continuousplot = T)
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+AC08><U+CC38><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+AC10><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+AC1C><U+C0B4><U+AD6C><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+AC1C><U+C63B><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+ACE0><U+B85C><U+C1E0><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+ACE0><U+C6A4><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+AD74><U+CC38><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+AD74><U+D53C><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+AE4C><U+CE58><U+BC15><U+B2EC>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+B178><U+AC04><U+C8FC><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+B290><U+B985><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+B2E4><U+B985><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+B2F9><U+B2E8><U+D48D><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+B54C><U+C8FD><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+B5A1><U+AC08><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+B9AC><U+AE30><U+B2E4><U+C18C><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+B9D0><U+CC44><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+BB3C><U+C624><U+B9AC><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+BB3C><U+D478><U+B808><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+BC15><U+B2EC><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+BC24><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+BCF5><U+C790><U+AE30>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C0AC><U+C2DC><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C0B0><U+BC9A><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C0B0><U+BF55><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C0B0><U+D33D><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C0C1><U+C218><U+B9AC><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C11C><U+C5B4><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C18C><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C18C><U+D0DC><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C1E0><U+BB3C><U+D478><U+B808><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C2E0><U+AC08><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C544><U+AE4C><U+C2DC><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C655><U+D33D><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C77C><U+BCF8><U+C78E><U+AC08><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C794><U+D138><U+BC9A><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C7A3><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C804><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+C878><U+CC38><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+CABD><U+B3D9><U+BC31><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+CE35><U+CE35><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+D325><U+BC30><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+D33D><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+D53C><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+D568><U+BC15><U+AF43><U+B098><U+BB34>' to native encoding
#> Warning in do.call(cbind, lapply(x, is.na)): unable to translate
#> '<U+D669><U+BCBD><U+B098><U+BB34>' to native encoding

# Calculate biomass by administrative district
biomass_district <- biomass_nfi(nfi_donghae, plotgrp = "SGG", continuousplot = T)
#> Warning in is.data.frame(y): strings not representable in native encoding will
#> be translated to UTF-8

# Calculate CWD biomass grouped by administrative district and decay class
cwd_grpby <- cwd_biomass_nfi(nfi_donghae, plotgrp = "SGG", treegrp = "DECAY", continuousplot = T)
#> Warning in is.data.frame(y): strings not representable in native encoding will
#> be translated to UTF-8

# Create a bar plot of importance values at 5-year intervals
tsvis_iv_bar <- tsvis_nfi(nfi_donghae, y = "iv", output = "bar", isannual = FALSE, continuousplot = T)

# Generate a line plot of carbon biomass over time
tsvis_bm_line <- tsvis_nfi(nfi_donghae, y = "biomass", continuousplot = T, 
                            bm_type = "carbon", output = "line")

# # Create a map of volume at the sido level
# remotes::install_github("SYOUNG9836/kadmin")
# tsvis_bm_map <- tsvis_nfi(nfi_donghae, admin = "sido", continuousplot = T, 
#                            y = "biomass", bm_type = "volume", output = "map")
                           
```
