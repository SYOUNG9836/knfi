#' Read Korean National Forest Inventory
#'
#' @description
#' read_nfi() function reads and processes the Korean National Forest Inventory (NFI).
#' It loads annual NFI files from a local computer, transforms the data into an analysis-friendly format, and performs data integrity verification.
#' Users can specify districts and tables to load.
#' When loading data from the original NFI Excel files, the function will automatically handle the translation of Korean column names to English - no manual translation is required.
#' NFI data can be downloaded from \url{https://kfss.forest.go.kr/stat/}.
#' 
#' @details 
#' The function can load the following tables:
#' 
#' `plot` Base table containing subplot data including site, stand and non-forest area, among other details (automatically included).
#' 
#' `tree` Tree survey table including species, DBH, and height, among others. Data is collected from trees and large trees survey plot of subplot.
#' 
#' `cwd` Coarse woody debris table including species, tree decay level, and cause of death, among other details. Data is collected only at the center subplot of the cluster plot.
#' 
#' `stump` Stumps table including species and diameter at 20 cm above the ground, among other details. Data is collected only at the center subplot of the cluster.
#' 
#' `sapling` Saplings table including species, diameter at 20 cm above the ground, and the number of individuals, among other details. Data is collected only at the sapling survey plot of the subplot.
#' 
#' `veg` Vegetation table (both woody and herbaceous plants). It records species, number of individuals, and dominance, among others. Data is collected from three vegetation survey plots located within each selected center subplot. The selection includes 25% of the total number of center subplots.
#' 
#' `herb` Herbaceous table focused on the herbaceous list. Data is collected only at the sapling survey plot of the subplot.
#' 
#' `soil` Soil table including the thickness of the organic layer and soil depth, among others. Data is collected from three soil survey plots located within each selected center subplot. The selection includes 25% of the total number of center subplots.
#' 
#' For more details, refer to the National Forest Inventory guidelines.
#' 
#' This function performs several data integrity validation. 
#' 1. Corrects administrative region information for subplots. (col: SIDO, SIDO_CD, SGG, SGG_CD, EMD, EMD_CD)
#' 2. Adds ecoregion and catchment for subplots. (col: ECOREGION, CATCHMENT)
#' 3. Verifies and corrects coniferous/deciduous classification of tree species. (col: CONDEC_CLASS, CONDEC_CLASS_CD, WDY_PLNTS_TYP, WDY_PLNTS_TYP_CD)
#' 4. Adds scientific names for species. (col: SCIENTIFIC_NAME)
#' 5. Adds Korean and English names for plant families and genera. (col: FAMILY, FAMILY_KOREAN, GENUS, GENUS_KOREAN)
#' 6. Adds whether a plant is native or cultivated, and identifies if it is a food, medicinal, fiber, or ornamental resource. (col: NATIVE_CULTIVATED, FOOD, MEDICINAL, FIBER, ORNAMENTAL)
#' 7. Calculates basal area for individual tree (col: BASAL_AREA)
#' 8. Calculates forest type, dominant species, and dominant species percentage for each subplot and cluster plot. (col: FORTYP_SUB, DOMIN_PERCNT_SUB, DOMIN_SP_SUB, FORTYP_CLST, DOMIN_PERCNT_CLST, DOMIN_SP_CLST)
#' Species classification and taxonomy follow the standards set by the Korean Plant Names Index Committee of the Korea National Arboretum. 
#'  
#' @param dir : A character vector; The directory containing NFI files.
#' @param district : A character vector; The district names in Korean (sido, sigungu, or eupmyondong levels). If `NULL`, the entire dataset is loaded. Combine multiple districts using \code{c()}. 
#' @param tables : A character vector; tables to import. Options: 'tree', 'cwd', 'stump', 'sapling', 'veg', 'herb', 'soil'. Combine multiple tables using \code{c()}. e.g., `c('tree', 'cwd', 'stump', 'sapling', 'veg', 'herb', 'soil')`.  
#' @param pattern : A character vector; (default "xlsx"); file pattern to match when loading NFI files. Use regular expressions to filter specific files (e.g., "NFI5.*xlsx" for 5th NFI files only)
#' @param ... : Additional arguments to be passed to \code{list.files()}
#' 
#' @return A `data.frame`; the processed NFI data, structured for easy analysis. 
#' 
#' @examples
#' \dontrun{
#'  # Load tree and CWD data for all districts
#'  nfi5_data <- read_nfi("D:/NFI/", district = NULL, tables = c("tree", "cwd"), recursive = TRUE)
#' }
#' 
#' @note  
#' To manually download subsets of the annual NFI file, visit the Korea Forest Service Forestry Statistics Platform (\url{https://kfss.forest.go.kr/stat/}), download .zip files, and extract them.
#' 
#' -The 5th National Forest Inventory file: \url{https://kfss.forest.go.kr/stat/ptl/article/articleFileDown.do?fileSeq=2995&workSeq=2203}
#' -The 6th National Forest Inventory file: \url{https://kfss.forest.go.kr/stat/ptl/article/articleFileDown.do?fileSeq=2996&workSeq=2204}
#' -The 7th National Forest Inventory file: \url{https://kfss.forest.go.kr/stat/ptl/article/articleFileDown.do?fileSeq=2997&workSeq=2205}
#' 
#' Use \code{data("nfi_col")} to view the Korean and English names of the column names. 
#' 
#' While the National Forest Inventory undergoes rigorous quality control, including internal reviews  and field inspections, errors may still exist due to the extensive nature of the survey (approximately 4,000 plots and over 70 items in the 7th phase). 
#' Please use the data cautiously and report any anomalies to help improve our algorithms.
#' 
#' If you want to save the results to your computer, you can save them in Excel format. 
#' For example, you can use the following code:\code{writexl::write_xlsx(data, "data.xlsx")}
#' 
#' If you want to read the saved data back, use the code below:
#' \code{path <-"../nfi_donghae.xlsx"}
#' \code{sheet_names <- readxl::excel_sheets(path)}
#' \code{for (sheet_name in sheet_names) {nfi[[sheet_name]] <- readxl::read_excel(path, sheet = sheet_name) }}
#' 
#' @export


read_nfi <- function(dir, district=NULL, tables=c("tree", "cwd"), pattern = "xlsx", ...){
  
  
  ## Load a list of .xlsx files located in the path--------------------------------------------------
  if (stringr::str_sub(dir,-1) != '/'){
    dir <- paste(dir, '/', sep = "")} 
  
  
  ## error message--------------------------------------------------------------
  if(!dir.exists(dir)) {
    stop(paste('Directory ', dir, ' does not exist.'))
  }
  
  
  
  if(!is.null(tables)){
    if(any(!tables %in% c('plot', 'tree', 'cwd', 'stump', 'herb', 'veg', 'sapling', 'soil'))){
      stop("param 'tables' must be one of 'plot', 'tree', 'cwd', 'stump', 'sapling', 'veg', 'herb', 'soil'")
    }
  }
  
  if(!is.null(district)){
    
    if(!is.character(district)) {
      stop("param 'district' must be 'character'")
    }
    
    
    if(any(!district %in% district_DB$DISTRICT_NAME)) { # Check NFI_plot_DB  district_code  district_DB
      
      prefix <- substr(district, 1, 2)
      matches <- district_DB$DISTRICT_NAME[grep(prefix, district_DB$DISTRICT_NAME)]
      
      if (length(matches) > 0) {
        matches <- paste(matches, collapse = ", ")
        matches_name <- paste0("Closest match: ", matches)
      } else {
        matches_name <- paste0("No similar names found.")
      }
      
      
      stop(paste0( 'District ', district, ' does not exist. ', matches_name))
    }}

  
  filenames <- list.files(path=dir, pattern=pattern, ...)
  
  plot_list <- vector("list", length = length(filenames))
  tree_list <- vector("list", length = length(filenames))
  cwd_list <- vector("list", length = length(filenames))
  stump_list <- vector("list", length = length(filenames))
  sapling_list <- vector("list", length = length(filenames))
  veg_list <- vector("list", length = length(filenames))
  herb_list <- vector("list", length = length(filenames))
  soil_list <- vector("list", length = length(filenames))
  
  card <- 0
  

  for(i in 1:length(filenames)){
    
    ## General_info sheet --------------------------------------------------------------
    General_info <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[1,1], 
                                       col_names = TRUE, col_types = "text")
    
    colnames(General_info) <- gsub("\\s+", " ", gsub("[`]", "", colnames(General_info)))
    
    general_info_colnames <- names(General_info)
    missing_names <- general_info_colnames[!general_info_colnames %in% nfi_col$Korean_Column_Name]
    
    for (name in missing_names) {
      nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
    }
    
    new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
    General_info <- stats::setNames(General_info, new_names[names(General_info)])
    
    General_info <- General_info[(names(General_info) != c("CREATED_DATE"))]
    
    
    
    ## Non_forest sheet -------------------------------------------------------------
    Non_forest <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[2,1],
                                     col_names = TRUE, col_types = "text")
    colnames(Non_forest) <- gsub("\\s+", " ", gsub("[`]", "", colnames(Non_forest)))
    
    Non_forest_colnames <- names(Non_forest)
    missing_names <- Non_forest_colnames[!Non_forest_colnames %in% nfi_col$Korean_Column_Name]
    
    for (name in missing_names) {
      nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
    }
    
    new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
    Non_forest <- stats::setNames(Non_forest, new_names[names(Non_forest)])
    
    
    
    ## Stand_inve sheet --------------------------------------------------------------
    Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[3,1],
                                     col_names = TRUE, col_types = "text")
    colnames(Stand_inve) <- gsub("\\s+", " ", gsub("[`]", "", colnames(Stand_inve)))
    
    Stand_inve_colnames <- names(Stand_inve)
    missing_names <- Stand_inve_colnames[!Stand_inve_colnames %in% nfi_col$Korean_Column_Name]
    
    for (name in missing_names) {
      nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
    }
    
    new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
    Stand_inve <- stats::setNames(Stand_inve, new_names[names(Stand_inve)])
    
    
    ## NFI_plot_DB 
    Stand_inve <- Stand_inve[!(names(Stand_inve) %in% c("SIDO_CD", "SGG_CD", "EMD_CD", "SIDO", "SGG", "EMD"))]
    Stand_inve <- left_join(Stand_inve, NFI_plot_DB, by=c('SUB_PLOT'))
    
    
    ## district filtering --------------------------------------------------------------
    if(!is.null(district)){
      
      site_codes <- sapply(district, function(d) {
        gsub("-", "", district_code[district_code[, "district_name"] == d, "district_CD"][1])
      })
      

      Stand_inve <- bind_rows(lapply(site_codes, function(site_code) {
        if(nchar(site_code) == 10) {
          Stand_inve %>% filter(EMD_CD == substr(site_code, 1, 8))
        } else if(nchar(site_code) == 5) {
          Stand_inve %>% filter(SGG_CD == site_code)
        } else {
          Stand_inve %>% filter(SIDO_CD == site_code)
        }
      }))
      
      
      ## error: No NFI data for the district-----------------------------------------------------------
      if(nrow(Stand_inve) == 0){
        
        if (card == length(filenames)) {
          stop(paste('NFI data in', district, 'does not exist.'))
        } else {
          card <- card + 1
          next
        }}
      
    }
    
    
    
    
    ## Merge General_info and Non_forest based on Stand_inve----------------------------------------------
    Stand_inve <- left_join(x=Stand_inve, y=General_info, 
                            by=c('CLST_PLOT', 'SUB_PLOT', 'CYCLE',  'INVYR', 'FORTYPCD', 'FORTYP'))
    
    Stand_inve <- left_join(x=Stand_inve, y=Non_forest, 
                            by=c('CLST_PLOT', 'SUB_PLOT', 'CYCLE',  'INVYR'))
    
    
    ## Plot data by .xlsx (yearly)------------------------------------------
    plot_list[[i]] <- Stand_inve
    plot_all <- unique(plot_list[[i]]$SUB_PLOT)
    
    
    ## tree_list sheet --------------------------------------------------------------
    if("tree" %in% tables){
      tree_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[4,1],
                                           col_names = TRUE, col_types = "text")
      
      colnames(tree_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(tree_list[[i]])))
      
      tree_list_colnames <- names(tree_list[[i]])
      missing_names <- tree_list_colnames[!tree_list_colnames %in% nfi_col$Korean_Column_Name]
      
      for (name in missing_names) {
        nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
      tree_list[[i]] <- stats::setNames(tree_list[[i]], new_names[names(tree_list[[i]])])
      
      
      tree_list[[i]] <- tree_list[[i]][tree_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    if("cwd" %in% tables){
      ## cwd_list sheet --------------------------------------------------------------
      cwd_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[5,1], range = cellranger::cell_cols("A:M"),
                                          col_names = TRUE, col_types = "text")
      
      colnames(cwd_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(cwd_list[[i]])))
      
      cwd_list_colnames <- names(cwd_list[[i]])
      missing_names <- cwd_list_colnames[!cwd_list_colnames %in% nfi_col$Korean_Column_Name]
      
      for (name in missing_names) {
        nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
      cwd_list[[i]] <- stats::setNames(cwd_list[[i]], new_names[names(cwd_list[[i]])])
      
      cwd_list[[i]] <- cwd_list[[i]][cwd_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    if("stump" %in% tables){
      ## stump_list sheet --------------------------------------------------------------
      stump_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[6,1], 
                                            col_names = TRUE, col_types = "text")
      
      colnames(stump_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(stump_list[[i]])))
      
      stump_list_colnames <- names(stump_list[[i]])
      missing_names <- stump_list_colnames[!stump_list_colnames %in% nfi_col$Korean_Column_Name]
      
      for (name in missing_names) {
        nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
      stump_list[[i]] <- stats::setNames(stump_list[[i]], new_names[names(stump_list[[i]])])
      
      stump_list[[i]] <- stump_list[[i]][stump_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    if("sapling" %in% tables){
      ## sapling_list sheet--------------------------------------------------------------
      sapling_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[7,1], 
                                              col_names = TRUE, col_types = "text")
      
      colnames(sapling_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(sapling_list[[i]])))
      
      sapling_list_colnames <- names(sapling_list[[i]])
      missing_names <- sapling_list_colnames[!sapling_list_colnames %in% nfi_col$Korean_Column_Name]
      
      for (name in missing_names) {
        nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
      sapling_list[[i]] <- stats::setNames(sapling_list[[i]], new_names[names(sapling_list[[i]])])
      
      sapling_list[[i]] <- sapling_list[[i]][sapling_list[[i]]$SUB_PLOT %in% plot_all,]
      
    }
    
    
    if("veg" %in% tables){
      ## veg_list sheet --------------------------------------------------------------
      veg_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[8,1], 
                                          col_names = TRUE, col_types = "text")
      
      colnames(veg_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(veg_list[[i]])))
      
      veg_list_colnames <- names(veg_list[[i]])
      missing_names <- veg_list_colnames[!veg_list_colnames %in% nfi_col$Korean_Column_Name]
      
      for (name in missing_names) {
        nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
      veg_list[[i]] <- stats::setNames(veg_list[[i]], new_names[names(veg_list[[i]])])
      
      veg_list[[i]] <- veg_list[[i]][veg_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    
    if("herb" %in% tables){
      ## herb_list sheet --------------------------------------------------------------
      herb_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[9,1], 
                                           col_names = TRUE, col_types = "text")
      
      colnames(herb_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(herb_list[[i]])))
      
      herb_list_colnames <- names(herb_list[[i]])
      missing_names <- herb_list_colnames[!herb_list_colnames %in% nfi_col$Korean_Column_Name]
      
      for (name in missing_names) {
        nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
      herb_list[[i]] <- stats::setNames(herb_list[[i]], new_names[names(herb_list[[i]])])
      
      herb_list[[i]] <- herb_list[[i]][herb_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    if("soil" %in% tables){
      ## soil_list sheet --------------------------------------------------------------
      soil_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[10,1], 
                                           col_names = TRUE, col_types = "text")
      
      colnames(soil_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(soil_list[[i]])))
      
      soil_list_colnames <- names(soil_list[[i]])
      missing_names <- soil_list_colnames[!soil_list_colnames %in% nfi_col$Korean_Column_Name]
      
      for (name in missing_names) {
        nfi_col <- bind_rows(nfi_col, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(nfi_col$Column_Name, nfi_col$Korean_Column_Name)
      soil_list[[i]] <- stats::setNames(soil_list[[i]], new_names[names(soil_list[[i]])])
      
      soil_list[[i]] <- soil_list[[i]][soil_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
  }  

  ## Merge data by .xlsx (yearly)--------------------------------------------------------------
  plot_df <- data.table::rbindlist(plot_list, fill=TRUE, use.names=TRUE)
  plot_df <- as.data.frame(plot_df)
  
  
  NFI <- list(plot = plot_df)
  plot_subset <- NFI$plot[,c('CLST_PLOT', 'SUB_PLOT', 'CYCLE', 'INVYR'), drop = FALSE]
  
  if("tree" %in% tables){
    tree_df <- data.table::rbindlist(tree_list, fill=TRUE, use.names=TRUE)
    tree_df <- as.data.frame(tree_df)
    
    NFI$tree <- tree_df
    NFI$tree <- left_join(NFI$tree, plot_subset,
                          by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  
  if("cwd" %in% tables){
    cwd_df <- data.table::rbindlist(cwd_list, fill=TRUE, use.names=TRUE)
    cwd_df <- as.data.frame(cwd_df)
    
    NFI$cwd <- cwd_df
    NFI$cwd <- left_join(NFI$cwd, plot_subset, 
                         by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  
  if("stump" %in% tables){
    stump_df <- data.table::rbindlist(stump_list, fill=TRUE, use.names=TRUE)
    stump_df <- as.data.frame(stump_df)
    
    NFI$stump <- stump_df
    NFI$stump <- left_join(NFI$stump, plot_subset, 
                           by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  
  if("sapling" %in% tables){
    sapling_df <- data.table::rbindlist(sapling_list, fill=TRUE, use.names=TRUE)
    sapling_df <- as.data.frame(sapling_df)
    
    NFI$sapling <- sapling_df
    NFI$sapling <- left_join(NFI$sapling, plot_subset, 
                             by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  
  if("veg" %in% tables){
    veg_df <- data.table::rbindlist(veg_list, fill=TRUE, use.names=TRUE)
    veg_df <- as.data.frame(veg_df)
    
    NFI$veg <- veg_df
    NFI$veg <- left_join(NFI$veg, plot_subset, 
                         by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  if("herb" %in% tables){
    herb_df <- data.table::rbindlist(herb_list, fill=TRUE, use.names=TRUE)
    herb_df <- as.data.frame(herb_df)
    
    NFI$herb <- herb_df
    NFI$herb <- left_join(NFI$herb, plot_subset, 
                          by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  if("soil" %in% tables){
    soil_df <- data.table::rbindlist(soil_list, fill=TRUE, use.names=TRUE)
    soil_df <- as.data.frame(soil_df)
    
    NFI$soil <- soil_df
    NFI$soil <- left_join(NFI$soil, plot_subset, 
                          by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }


  ## Assign column attributes --------------------------------------------------------------
  # log_col <- c("FORCD", "SVYCD")
  # NFI$plot[ , colnames(NFI$plot) %in% log_col ] <- lapply(lapply(NFI$plot[ , colnames(NFI$plot) %in% log_col ], as.numeric), as.logical)
  # 
  # 
  # fac_col <- c("LAND_USECD", "LAND_USE","FORTYPCD","FORTYP", "DECAYCD")
  # NFI$plot[ , colnames(NFI$plot) %in% fac_col ] <- lapply(NFI$plot[ , colnames(NFI$plot) %in% fac_col ], as.factor)
  # 
  

  num_col <- c("NONFR_INCL_AREA_SUBP", "NONFR_INCL_AREA_LARGEP","RDDIST", "ELEV","SLOPE",
               "ASPCT",
               "DBH", "BOLE_HT", "HT", "DIST", "AZIMUTH", "TOTAGE",	"PITH_BARK_LEN",	"TRG_5YRS",
               "BARK_THICK",	"STD_DIAM_PROP", "ACTUALVOL",	"HT_EST",	"VOL_EST", "INVYR", "VOL", "HT", "DIA", "LEN",
               "TREECOUNT", "NUMINDI")
  
  char_col <- c("SUB_PLOT", "CLST_PLOT", "CREATED_DATE", "LARGEP_TREE",
                "FORCD", "SVYCD", "LAND_USECD", "FORTYPCD", "TRTCD", "SIDO_CD", "SGG_CD", "EMD_CD", 
                "TOPO_POSITION_CD", "SLP_POS_CD", "TERIN_CD", "8_ASPCT_CD", "FOR_TYP_CD", 
                "CAN_CVER_CD", "DIAM_CLS_CD", "AGE_CLS_CD", "OWN_CD", "TREE_SP_CD", 
                "FOR_MANAGE_STATUS_CD", "REG_STATUS_CD", "SOIL_TYP_CD", "SOIL_TX_A_CD", 
                "SOIL_TX_B_CD", "ROCK_EXP_CD", "ERO_COND_CD", "SPCD", "TREECLCD", "CCLCD", 
                "DECAYCD", "STANDING_DEAD_CD", "DOMINCD", "DRCCD", "CONDEC_CLASS_CD", "DECEVER_CD", "WDY_PLNTS_TYP_CD",
                "SOILPLOT", "VEGPLOT",
                "FOOD_CD",	"MEDICINAL_CD",	"FIBER_CD",	"ORNAMENTAL_CD"  )
  
  for(i in 1:length(NFI)){
    
    if(names(NFI)[i] == "tree"){
      NFI[[i]]$CONDEC_CLASS <- NULL
      NFI[[i]]$WDY_PLNTS_TYP <- NULL
    }
    
    if(!names(NFI)[i] %in% c("plot", "soil")){
      NFI[[i]] <- left_join(NFI[[i]], Species_DB, by= c("SP", "SPCD") )
    }
    
    NFI[[i]][ , colnames(NFI[[i]]) %in% num_col ] <- lapply(NFI[[i]][ , colnames(NFI[[i]]) %in% num_col ], function(x) as.numeric(as.character(x)))
    NFI[[i]][ , colnames(NFI[[i]]) %in% char_col ] <- lapply(NFI[[i]][ , colnames(NFI[[i]]) %in% char_col ], function(x) as.character(x))
    
    
  }
  
  
  if("tree" %in% tables){
    
    
    # FORTYP based on basal area (subplot)  --------------------------------------------------------------
    NFI$tree$BASAL_AREA <- (pi*(NFI$tree$DBH/2)^2)/10000
    
    large_tree_sub <- NFI$tree %>%
      group_by(CLST_PLOT, SUB_PLOT, CYCLE) %>%
      filter(!any(LARGEP_TREE == "0")) %>%
      distinct(CLST_PLOT, SUB_PLOT, CYCLE)
    
    stand_sub_data <- NFI$tree %>% filter(LARGEP_TREE == "0")
    stand_sub_data_2 <- left_join(large_tree_sub, NFI$tree, by= c("CLST_PLOT", "SUB_PLOT","CYCLE"))
    
    stand_sub_data <- bind_rows(stand_sub_data, stand_sub_data_2)
    
    stand_sub <- stand_sub_data %>%  
      mutate(deciduous_ba = ifelse(CONDEC_CLASS_CD == "1",  BASAL_AREA, 0)) %>% # deciduous
      group_by(SUB_PLOT, CYCLE) %>% 
      summarise(all_ba = sum(BASAL_AREA), 
                deciduous_ba = sum(deciduous_ba),
                .groups = 'drop')
    
    stand_sub$percent <- (stand_sub$deciduous_ba/stand_sub$all_ba) *100
    stand_sub$FORTYP_SUB <- ifelse(stand_sub$percent>=75, "Deciduous", 
                                   ifelse(stand_sub$percent>25, "Mixed", "Coniferous"))
    

    domin <- stand_sub_data %>%
      group_by(SUB_PLOT, CYCLE,  SP) %>%
      summarise(domin_ba = sum(BASAL_AREA), .groups = 'drop') %>%
      group_by(SUB_PLOT, CYCLE) %>%
      arrange(desc(domin_ba)) %>%
      slice(1) %>%
      ungroup()
    
    
    stand_sub <- left_join(stand_sub, domin, by= c("SUB_PLOT","CYCLE"))
    stand_sub$DOMIN_PERCNT_SUB <- (stand_sub$domin_ba/stand_sub$all_ba) *100
    stand_sub$DOMIN_SP_SUB <- stand_sub$SP
    
    condition <- (names(stand_sub) %in% c("SUB_PLOT","CYCLE", "FORTYP_SUB", "DOMIN_SP_SUB", "DOMIN_PERCNT_SUB"))
    NFI$plot <- left_join(NFI$plot, stand_sub[condition], by= c("SUB_PLOT","CYCLE"))
    
    
    # FORTYP based on basal area (clusterplot)
    stand_clust <- NFI$tree %>% filter(LARGEP_TREE == "0") 
    stand_clust <- stand_clust %>%
      mutate(deciduous_ba = ifelse(CONDEC_CLASS_CD == "1",  BASAL_AREA, 0)) %>%
      group_by(CLST_PLOT, CYCLE) %>% 
      summarise(all_ba = sum(BASAL_AREA), 
                deciduous_ba = sum(deciduous_ba),
                .groups = 'drop')
    
    stand_clust$percent <- (stand_clust$deciduous_ba/stand_clust$all_ba) *100
    stand_clust$FORTYP_CLST <- ifelse(stand_clust$percent>=75, "Deciduous", 
                                      ifelse(stand_clust$percent>25, "Mixed", "Coniferous"))
    
    
    domin <- NFI$tree %>% filter(LARGEP_TREE == "0") 
    domin <- domin %>%
      group_by(CLST_PLOT, CYCLE,  SP) %>%
      summarise(domin_ba = sum(BASAL_AREA), .groups = 'drop') %>%
      group_by(CLST_PLOT, CYCLE) %>%
      arrange(desc(domin_ba)) %>%
      slice(1) %>%
      ungroup()
    
    stand_clust <- left_join(stand_clust, domin, by= c("CLST_PLOT","CYCLE"))
    stand_clust$DOMIN_PERCNT_CLST <- (stand_clust$domin_ba/stand_clust$all_ba) *100
    stand_clust$DOMIN_SP_CLST <- stand_clust$SP
    
    condition <- (names(stand_clust) %in% c("CLST_PLOT","CYCLE", "FORTYP_CLST", "DOMIN_SP_CLST", "DOMIN_PERCNT_CLST"))
    
    
    
    NFI$plot <- left_join(NFI$plot, stand_clust[condition], by= c("CLST_PLOT","CYCLE"))
    
  }else{
    NFI$plot <- left_join(NFI$plot, NFI_FORTYP_DB, by= c("CLST_PLOT", "SUB_PLOT", "CYCLE", "INVYR"))
  }
  
  
  return(NFI) 
  
  
}


