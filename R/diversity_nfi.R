#' Calculate species diversity indices for National Forest Inventory Data
#' 
#' @description
#' diversity_nfi() function calculates species richness, evenness and the Shannon and Simpson diversity indices for National Forest Inventory (NFI) data.
#' It can provide diversity measures for individual plots, the entire study area, or specific groups within the study area using parameters `byplot` or `plotgrp`.
#' The function can calculate diversity at the species or genus level for different vegetation components (trees, herbs, vegetation, saplings).
#' It uses the \code{\link[vegan]{diversity}} function from the \pkg{vegan} package for core calculations.
#' Users have flexibility in specifying data inclusion criteria and analysis levels using parameters `clusterplot`, `largetreearea`, `stockedland`, and `talltree`.
#' These parameters determine whether to treat cluster plots as single plots, to include large tree survey plots, and to focus only on Stocked land and tall trees.
#' 
#' @details
#' The function calculates the following diversity indices (mean and standard error):
#' - Species richness: The total number of species surveyed.
#' - Shannon-Wiener index: Calculated as the sum of the proportions of individuals or basal area of each species relative to the total.
#' - Gini-Simpson index: Calculated as 1 minus Simpson's index. Ranges from 0 to 1, with higher values indicating greater diversity.
#' - Species evenness: Calculated by dividing Shannon diversity by the natural logarithm of species richness. 
#'   Ranges from 0 to 1, with 1 indicating that all species are evenly distributed.
#'  
#' @param data : A `list` generated by \code{\link{read_nfi}} that contains 'plot' and one of ('tree', 'herb', 'veg', 'sapling') data frames.
#' @param sp : A character vector; the column name of species information (e.g., "SP" for species, "GENUS" for genus-level analysis).
#' @param table : A character vector; Specifies which vegetation table to use for diversity analysis. Must be one of 'tree', 'herb', 'veg', 'sapling'.
#' @param basal : A logical flag (default FALSE); if TRUE, calculates tree diversity using basal area. If FALSE, uses number of individuals. Only applicable when `table = "tree"`.
#' @param byplot : A logical flag (default FALSE); if TRUE, calculates statistics for each plot separately. If FALSE, calculates for the entire dataset.
#' @param plotgrp : A character vector; specifies variables from 'plot' table to use for grouping. Use \code{c()} to combine multiple variables.
#' @param clusterplot : A logical flag (default FALSE); if TRUE, treats each cluster plot as a single unit. If FALSE, calculates for each subplot separately.
#' @param largetreearea : A logical flag (default FALSE); if TRUE, includes large tree survey plots in the analysis. If FALSE, only uses standard tree plots.
#' @param stockedland : A logical flag (default TRUE); if TRUE, includes only stocked land. If FALSE, includes all land types.
#' @param talltree : A logical flag (default TRUE); if TRUE, includes only tall trees. If FALSE, includes both trees and shrubs.
#' 
#' @return A `data.frame` that includes diversity indices.
#' The structure depends on the input parameters:
#' - If `byplot = TRUE`, each row represents a plot.
#' - If `byplot = FALSE`, each row represents the entire dataset or a group specified by `plotgrp`
#' 
#' @note 
#' The 'herb', 'veg', and 'sapling' tables may contain a lot of errors, so use caution when interpreting results from these tables.
#' 
#' @examples
#' 
#' data("nfi_donghae")
#' 
#' # Calculate tree diversity indices using basal area
#' tree_ba_diversity <- diversity_nfi(nfi_donghae, sp = "SP", table = "tree", basal = TRUE)
#' 
#' # Calculate tree diversity indices using number of individuals
#' tree_indi_diversity <- diversity_nfi(nfi_donghae, sp = "SP", table = "tree", basal = FALSE)
#' 
#' @seealso
#' \code{\link[vegan]{diversity}} for calculating the Shannon and Simpson diversity indices.
#' 
#' @references
#' Shannon, C. E. (1948). A mathematical theory of communication. The Bell System Technical Journal, 27(3), 379–423.
#' Simpson, E. H. (1949). Measurement of diversity. Nature, 163(4148), 688–688.
#' Pielou, E. C. (1966). The measurement of diversity in different types of biological collections. Journal of Theoretical Biology, 13, 131–144.
#' 
#' @export 


##  

diversity_nfi <- function(data, sp="SP", table="tree", basal=FALSE, plotgrp=NULL, byplot=FALSE, clusterplot=FALSE, largetreearea=FALSE, stockedland=TRUE, talltree=TRUE){
  
  ## error message-------------------------------------------------------------- 
  if(!table %in%  c('tree', 'herb', 'veg', 'sapling')){
    stop("param 'table' must be one of 'tree', 'herb', 'veg', 'sapling'")
  }
  
  required_names <- c("plot", table)
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  
  if (!is.null(plotgrp)){
    if(!is.character(plotgrp)) {
      stop("param 'plotgrp' must be 'character'")
    }
    if(any(!plotgrp %in% names(data$plot))){
      stop(paste0("param 'plotgrp': ", plotgrp," is not a column name in the 'plot' data frame."))
    }
  }
  
  if(!sp %in% names(data[[table]])){
    stop(paste0("param 'sp': ", sp," is not a column name in the '", table,"' data frame."))
  } 
  
  
  if (table != "tree"){
    if(basal) {
      warning("param 'basal' must be 'FALSE' if param 'table' is ", table)
    }}
  
  
  
  
  ## Preprocessing-------------------------------------------------------------- 
  if(clusterplot){
    plot_id <- c('CLST_PLOT')
  }else{
    plot_id <- c('SUB_PLOT')
  }
  
  
  if(table=="tree"){
    
    if (stockedland){ 
      data <- filter_nfi(data, c("plot$LAND_USECD == '1'"))
    }
    
    if(talltree){
      data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == "1")
    }
    
    if(!largetreearea){ 
      data$tree <- data$tree %>% filter(LARGEP_TREE == "0")
    }
    
    
    df <- left_join(data$tree[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'WDY_PLNTS_TYP_CD', 
                                  'BASAL_AREA', 'LARGEP_TREE', sp)], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", plotgrp)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
    

  }else if(table=="herb"){
    df <- left_join(data$herb[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", sp)], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", plotgrp)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else if(table=="veg"){
    df <- left_join(data$veg[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'VEGPLOT', 'NUMINDI', sp)], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", plotgrp)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else if(table=="sapling"){
    
    df <- left_join(data$sapling[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'TREECOUNT', sp)], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", plotgrp)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else(
    
    stop("param 'table' must be one of c('tree', 'herb', 'veg', 'sapling')")
  )
  
  
  plot_id  <- rlang::sym(plot_id)
  plotgrp  <- rlang::syms(plotgrp)
  sp<- rlang::sym(sp)
  

  
  
  ## Calculating Species Diversity-------------------------------------------------------------- 
  
  if(basal & table=="tree"){ # Based on Basal Area at Breast Height 
    
    indices_temp <- df %>%
      group_by(CYCLE, !!plot_id, !!!plotgrp, !!sp) %>%
      summarise(value = sum(BASAL_AREA), .groups = 'drop')
    
  }else{ # Based on Number of Individuals
    
    if(table=="tree"||table== "herb"){
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!plotgrp, !!sp) %>%
        summarise(value = n(), .groups = 'drop')
      
    }else if(table=="veg"){
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!plotgrp, !!sp) %>%
        summarise(value = sum(NUMINDI), .groups = 'drop')
      
    }else{ #sapling
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!plotgrp, !!sp) %>%
        summarise(value = sum(TREECOUNT), .groups = 'drop')
    }
  }
  
  
  indices_temp <- indices_temp %>% tidyr::spread(key = !!sp, value = value )
  
  
  indices <- indices_temp[,1:(length(plotgrp)+2)]
  abundance.matrix <- indices_temp[,-c(1:(length(plotgrp)+2))]
  abundance.matrix[is.na(abundance.matrix)] <- 0
  
  indices$Richness <- rowSums(abundance.matrix>0)
  indices$Shannon <- vegan::diversity(abundance.matrix) # shannon is default
  indices$Simpson <- vegan::diversity(abundance.matrix, "simpson")
  indices$Evenness  <- indices$Shannon/log(indices$Richness)
  
  
  if(!byplot){
    
    indices <- indices %>% 
      group_by(CYCLE, !!!plotgrp) %>% 
      summarise(mean_Richness = mean(Richness , na.rm=TRUE),
                se_Richness =  plotrix::std.error(Richness, na.rm=TRUE),
                mean_Shannon = mean(Shannon, na.rm=TRUE),
                se_Shannon =  plotrix::std.error(Shannon, na.rm=TRUE),
                mean_Simpson = mean(Simpson, na.rm=TRUE),
                se_Simpson =  plotrix::std.error(Simpson, na.rm=TRUE),
                mean_Evenness = mean(Evenness, na.rm=TRUE),
                se_Evenness =  plotrix::std.error(Evenness, na.rm=TRUE),.groups = 'drop')
    
    
  }
  
  
  return(indices)
  
  
}
