
#' bm_df()
#' 
#' @description
#' bm_df() function calculates the biomass of individual trees using species-specific national emission factors.
#' It is an internal function used within the biomass_nfi() function.
#'
#' @param data : data
#' @noRd
 
## Calculates biomass --------------------------------------------------------------
## species ~ Volume*(Wood density)*(Biomass expansion factor)*(1+Root to shoot ratio)*(Carbon fraction)*(44/12)-----------------
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)*(1+뿌리함량비)*(탄소전환계수)*(44/12)-----------------
bm_df <- function(data){
  
  
  output <- data  %>% mutate(species_bm = case_when(
    
    
    ##species-specific
    ## Pinus densiflora in Gangwondo(Yeongju-si, Bonghwa-gun, Uljin-gun, Yeongyang-gun, Gangwon-do)
    ## 강원지방소나무(영주시, 봉화군, 울진군, 영양군, 강원도) --------------------------------------------------------------
    (SPCD =="14994" & ( SGG_CD == 47210 |  SGG_CD == 47920 |  SGG_CD == 47930 | SGG_CD == 47760 | SIDO_CD == 42 )) 
    ~ "14994_GW" , 
    
    SPCD =="14994" ~ "14994", # Pinus densiflora (중부지방소나무)
    SPCD =="14964" ~ "14964" , # Larix kaempferi (일본잎갈나무)
    SPCD =="14987" ~ "14987", # Pinus rigida (리기다소나무)
    SPCD =="15003" ~ "15003", # Pinus thunbergii (곰솔)
    SPCD =="14973" ~ "14973", # Pinus koraiensis (잣나무)
    SPCD =="15014" ~ "15014" , # Cryptomeria japonica (삼나무)
    SPCD =="14973" ~ "14973" , # Chamaecyparis obtusa (편백)
    SPCD =="6617" ~ "6617" , # Quercus variabilis (굴참나무)
    SPCD =="6556" ~ "6556" , # Quercus mongolica (신갈나무)
    SPCD =="6512" ~ "6512" , # Quercus acutissima (상수리나무)
    SPCD =="6591" ~ "6591" , # Quercus serrata (졸참나무)
    SPCD =="6505" ~ "6505" , # Quercus acuta (붉가시나무)
    SPCD =="1959" ~ "1959" , # Robinia pseudoacacia (아까시나무)
    SPCD =="895" ~ "895" , # Betula pendula (자작나무)
    SPCD =="11588" ~ "11588" , # Liriodendron tulipifera (백합나무)
    SPCD =="19592" ~ "19592" , # Populus × tomentiglandulosa (은사시나무)
    SPCD =="6476" ~ "6476" , # Castanea crenata (밤나무)
    
    (DECEVER_CD == 1) ~ "EVERDEC" , # Other evergreen broad-leaved species
    (CONDEC_CLASS_CD ==1) ~ "OTHER_DEC" , # Other deciduous species
    (CONDEC_CLASS_CD ==0) ~ "OTHER_CON", # Other conifer species
    TRUE ~ as.character(NA)
    
  ))
  

  output <- left_join(output, bio_coeff[,-1], by= c("species_bm" ="SPCD") )
  
  ## Calculating aboveground biomass--------------------------------------------------------------
  ## species ~ Volume*(Wood density)*(Biomass expansion factor)-------------------------------
  output$AG_biomass <- (output$VOL_EST)*(output$wood_density)*(output$biomass_expan)
  ## Calculating biomass--------------------------------------------------------------
  ## species ~ Volume*(Wood density)*(Biomass expansion factor)*(1+Root to shoot ratio)-----------------
  output$T_biomass <- output$AG_biomass*(1+output$root_shoot_ratio)
  ## Calculating carbon storage-----------------------------------------------
  ## species ~ Volume*(Wood density)*(Biomass expansion factor)*(1+Root to shoot ratio)*(Carbon fraction)-----------------------
  output$CF <- ifelse(output$CONDEC_CLASS_CD ==1, 0.48, 0.51 ) # Carbon fraction 0.51 (coniferous) or 0.48 (broadleaf)
  output$carbon_stock <- output$T_biomass*output$CF
  ## Calculating carbon dioxide storage--------------------------------------------------------------
  ## species ~ Volume*(Wood density)*(Biomass expansion factor)*(1+Root to shoot ratio)*(Carbon fraction)*(44/12)--------------------
  output$co2_stock <- output$carbon_stock*(44/12)
  
  
  return(output)
  
}







#' Calculate Tree Biomass for National Forest Inventory Data
#' 
#' @description
#' biomass_nfi() function estimates volume, aboveground biomass, biomass, carbon storage and carbon dioxide storage.
#' It can provide summaries for individual plots, the entire study area, or specific groups within the study area using parameters `byplot`, `plotgrp` and `treegrp`.
#' When calculating biomass at the individual trees and plots level, users have flexibility in specifying data inclusion criteria and analysis levels using parameters `clusterplot`, `largetreearea`, `stockedland`, and `talltree`.
#' These parameters determine whether to treat cluster plots as single plots, to include large tree survey plots, or to focus only on Stocked land and tall trees.
#' Users can also choose the criteria for post-stratification using the `strat` parameter.
#' 
#' @details
#' This function calculates biomass using methodologies employed for national statistics (mean, variance, standard error, relative standard error):
#' - Applies national carbon emission factors to calculate biomass at the individual tree level.
#' - Estimates biomass per hectare at the cluster or subplot level, with options to include only basic survey trees or both basic and large tree survey trees.
#' - Uses the Double Sampling for Post-stratification (DSS) method to derive annual statistics.
#' - Applies the Weighted Moving Average (WMA) method to integrate annual statistics from the 20% of plots surveyed each year into a single time point.
#'
#' @param data : A `list` generated by \code{\link{read_nfi}} that contains 'plot' and 'tree' data frames.
#' @param byplot : A logical flag (default FALSE); if TRUE, calculates statistics for each plot separately. If FALSE, calculates for the entire dataset or groups specified by `plotgrp` and `treegrp`.
#' @param plotgrp : A character vector; variables from 'plot' tables for grouping. Use \code{c()} to combine multiple variables.
#' @param treegrp : A character vector; variables from 'tree' tables for grouping. Use \code{c()} to combine multiple variables.
#' @param continuousplot : A logical flag (default TRUE); if TRUE, includes only plots that have been continuously measured in all NFI cycles (5th, 6th, etc.). If FALSE, includes plots regardless of missing cycle measurements.
#' @param strat : A character vector; the variable used for post-stratification. In the National Forest Inventory of Korea, it is typically used by forest type.
#' @param clusterplot : A logical flag (default FALSE); if TRUE, treats each cluster plot as a single unit. If FALSE, calculates for each subplot separately.
#' @param largetreearea : A logical flag (default TRUE); if TRUE, includes large tree survey plots in the analysis. If FALSE, only uses standard tree plots.
#' @param stockedland : A logical flag (default TRUE); if TRUE, includes only stocked land. If FALSE, includes all land types.
#' @param talltree : A logical flag (default TRUE); if TRUE, includes only tall trees. If FALSE, includes both trees and shrubs.

#' @return A `data.frame` that includes biomass estimates.
#' The structure depends on the input parameters:
#' - If `byplot = TRUE`, each row represents a plot.
#' - If `byplot = FALSE`, each row represents the entire dataset or a group specified by `plotgrp` and `treegrp`
#' 
#' @note
#' Biomass calculation involves dividing the data into groups based on `plotgrp` and then applying post-stratification to each group. 
#' As a result, if the data for each group is not sufficiently large, the relative standard error (RSE) may be high. 
#' It is important to check the RSE and other statistical measures in the biomass results.
#' 
#' @examples
#' 
#' data("nfi_donghae")
#' 
#' # Basic usage
#' biomass <- biomass_nfi(nfi_donghae, continuousplot = TRUE)
#' 
#' # Calculate biomass by administrative district
#' district_biomass <- biomass_nfi(nfi_donghae, plotgrp = "SGG", continuousplot=TRUE)
#' 
#' # Calculate biomass for each plot
#' plot_biomass <- biomass_nfi(nfi_donghae, byplot = TRUE)
#' 
#' @references 
#' Son, Y., Kim, R., Lee, K., Pyo, J., Kim, S., Hwang, J., Lee, S., & Park, H. (2014). Carbon emission factors and biomass allometric equations by species in Korea. Korea Forest Research Institute.
#' Yim, J., Moon, G., Lee, M., Kang, J., Won, M., Ahn, E., & Jeon, J. (2021). 2020 Forest inventory of Korea. Korea Forest Research Institute.
#' 
#' @export 


biomass_nfi <- function(data, byplot= FALSE, plotgrp=NULL, treegrp= NULL, continuousplot=FALSE, strat="FORTYP_SUB", clusterplot=FALSE, largetreearea=TRUE, stockedland=TRUE, talltree=TRUE){
  
  
  ## error message-------------------------------------------------------------- 
  required_names <- c("plot", "tree")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  
  if (clusterplot){
    if(!is.null(strat) && strat=="FORTYP_SUB"){
      stop("When the param 'clusterplot' is set to TRUE, param 'strat' uses FORTYP_CLST (the forest type for the cluster plot) instead of FORTYP_SUB (the forest type for each subplot).")
    }
    
    if(!is.null(plotgrp) && plotgrp=="FORTYP_SUB"){
      stop("When the param 'clusterplot' is set to TRUE, param 'plotgrp' uses FORTYP_CLST (the forest type for the cluster plot) instead of FORTYP_SUB (the forest type for each subplot).")
    }
  }
  
  
  
  if (!is.null(plotgrp)){
    if(any(plotgrp %in% strat)){
      warning("param 'plotgrp' is the same as param 'strat'")
      strat <- "strat"
      data$plot$strat <- "same"
    }
    if(!is.character(plotgrp)) {
      stop("param 'plotgrp' must be 'character'")
    }
    if(any(!plotgrp %in% names(data$plot))){
      stop(paste0("param 'plotgrp': ", plotgrp," is not a column name in the 'plot' data frame."))
    }
    
  }
  
  if (!is.null(treegrp)){
    
    if(!is.character(treegrp)) {
      stop("param 'treegrp' must be 'character'")
    }
    if(any(!treegrp %in% names(data$tree))){
      stop(paste0("param 'treegrp': ",treegrp," is not a column name in the 'tree' data frame."))
    }
  }
  
  if (!is.null(strat)){
    if(!is.character(strat)) {
      stop("param 'strat' must be 'character'")
    }
    if(byplot){
      warning("param 'byplot' has priority over param 'strat'")
    }
    if(!strat %in% names(data$plot)){
      stop(paste0("param 'strat': ", strat," is not a column name in the 'plot' data frame."))
    }
  }
  
  
  ## Preprocessing-------------------------------------------------------------- 
  if (stockedland){ 
    data <- filter_nfi(data, c("plot$LAND_USECD == '1'"))
  }
  
  if(talltree){
    data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == "1")
  }
  
  if ("FORTYP_SUB" %in% names(data$plot)) {
    data$plot <- data$plot %>% filter(!is.na(FORTYP_SUB))
  }
  
  if(continuousplot){
    
    all_cycle <- unique(data$plot$CYCLE)
    samples_with_all_cycle <- data$plot %>%
      filter(!is.na(FORTYP_SUB)) %>%
      group_by(SUB_PLOT) %>%
      filter(all(all_cycle %in% CYCLE)) %>%
      distinct(SUB_PLOT) %>%
      pull(SUB_PLOT)
    
    data <- filter_nfi(data, c("plot$SUB_PLOT %in% samples_with_all_cycle"))
    
  }

  df <- left_join(data$tree[,c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'WDY_PLNTS_TYP_CD','SP', 'SPCD',
                                'CONDEC_CLASS_CD', 'DECEVER_CD', 'DBH', 'VOL_EST',  'LARGEP_TREE', treegrp)], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD",
                               'NONFR_INCL_AREA_SUBP', 'NONFR_INCL_AREA_LARGEP', "SGG_CD", 'SIDO_CD', strat, plotgrp)],
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))

  if (!is.numeric(df$VOL_EST)){
    df$VOL_EST <- as.numeric(df$VOL_EST)
  } 
  
  
  
  if(!largetreearea){ 
    df <- df %>% filter(df$LARGEP_TREE == "0")
    df$largetree <- 0
  }else{
    df$largetree <- ifelse(df$DBH>=30, 1, 0)
  }
  
  df$largetree_area <- 0.08 - ((df$NONFR_INCL_AREA_LARGEP*10)/10000) # unit m2/10
  df$tree_area <- 0.04 - ((df$NONFR_INCL_AREA_SUBP*10)/10000)
  
  df <- bm_df(df)
  
  if(clusterplot){
    plot_id <- c('CLST_PLOT')
  }else{
    plot_id <- c('SUB_PLOT')
  }
  
  plot_id  <- rlang::sym(plot_id)
  plotgrp  <- rlang::syms(plotgrp)
  strat<- rlang::sym(strat)
  treegrp  <- rlang::syms(treegrp)
  
  if(!largetreearea){
    largetree <- NULL
  }
  if(byplot){
    strat <- NULL
  }
  
  
  # 1.Biomass calculation by cluster or subplot
  if(clusterplot){ #  1.1 Biomass calculation by cluster plots
    
    plot_area <- df[-which(duplicated(df[c('SUB_PLOT', 'CYCLE')])),c('CYCLE', 'INVYR', 'CLST_PLOT', 'SUB_PLOT', 'largetree_area', 'tree_area')]
    
    plot_area <- plot_area %>%
      group_by(CYCLE, !!plot_id, INVYR) %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')

    bm_temp <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, largetree, !!!plotgrp, !!!treegrp, !!strat) %>% 
      summarise(volume_m3 = sum(VOL_EST, na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')

    bm_temp <- full_join(bm_temp, plot_area, by=c('CYCLE', 'INVYR', quo_name(plot_id)))
    
    
    if(!largetreearea){ # 1.1.1 Biomass calculation by cluster plots excluding large tree survey plots 
      
      condition_ha <- c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha")
      bm_temp[condition_ha] <-  NA
      bm_temp <- as.data.frame(bm_temp)
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      
      bm_temp[condition_ha] <- 
        lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      
      bm_temp[condition] <- NULL
      bm_temp$tree_area <- NULL
      bm_temp$largetree_area <- NULL
      bm_temp$largetree <- NULL
      
      
    }else{ # 1.1.2 Biomass calculation by cluster plots including large tree survey plots 
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!plotgrp, !!!treegrp, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
    
    
    
    
    
    
  }else{ # 1.2 Biomass calculation by subplots 
    
    
    bm_temp <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, !!strat, largetree, !!!plotgrp, !!!treegrp, largetree_area, tree_area) %>% 
      summarise(volume_m3 = sum(VOL_EST, na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    if(!largetreearea){ # 1.2.1 Biomass calculation by subplots excluding large tree survey plots  
      
      condition_ha <- c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha")
      bm_temp[condition_ha] <-  NA
      bm_temp <- as.data.frame(bm_temp)
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      
      bm_temp[condition_ha] <- 
        lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      
      bm_temp[condition] <- NULL
      bm_temp$tree_area <- NULL
      bm_temp$largetree_area <- NULL
      bm_temp$largetree <- NULL
      
      
      
    }else{ # 1.2.2 Biomass calculation by subplots including large tree survey plots  
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!plotgrp, !!!treegrp, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
  }
  
  if(!byplot){ # 2.1 Biomass calculation using post-stratified double sampling and weighted moving average methods
    
    # 2.1.1 Double sampling for post-strat(by forest stand)
    weight_plotgrp <- data$plot %>% 
      group_by(CYCLE, !!!plotgrp) %>% 
      summarise(plot_num_all = n_distinct(!!plot_id),.groups = 'drop')
    
    
    weight_year <- data$plot %>% 
      group_by(CYCLE, INVYR, !!!plotgrp) %>% 
      summarise(plot_num_year = n_distinct(!!plot_id),.groups = 'drop')
    
    
    weight_stand <- data$plot %>% 
      group_by(CYCLE, INVYR, !!strat, !!!plotgrp) %>% 
      summarise(plot_num_stand = n_distinct(!!plot_id),.groups = 'drop')
    
    
    weight_DSS <- full_join(weight_stand, weight_year, by =c("CYCLE", "INVYR", as.character(unlist(lapply(plotgrp, quo_name)))))
    weight_DSS$weight_DSS <- weight_DSS$plot_num_stand/weight_DSS$plot_num_year
    

    # 2.1.2 Aggregating calculated biomass per plot by forest type.
    bm_temp_DSS <- bm_temp %>% 
      group_by(CYCLE, INVYR, !!strat, !!!plotgrp, !!!treegrp) %>% 
      summarise(var_volume_m3_ha =  var(volume_m3_ha, na.rm=TRUE),
                volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                var_biomass_ton_ha =  var(biomass_ton_ha, na.rm=TRUE),
                biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                var_AG_biomass_ton_ha =  var(AG_biomass_ton_ha, na.rm=TRUE),
                AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                var_carbon_stock_tC_ha =  var(carbon_stock_tC_ha, na.rm=TRUE),
                carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                var_co2_stock_tCO2_ha =  var(co2_stock_tCO2_ha, na.rm=TRUE),
                co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
    
    
    bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("CYCLE", "INVYR", quo_name(strat), as.character(unlist(lapply(plotgrp, quo_name)))))
    
    
    condition_DSS <- c("w_volume_m3_ha","w_biomass_ton_ha","w_AG_biomass_ton_ha","w_carbon_stock_tC_ha","w_co2_stock_tCO2_ha")
    bm_temp_DSS[condition_DSS] <-  NA
    bm_temp_DSS <- as.data.frame(bm_temp_DSS)
    
    condition <- (names(bm_temp_DSS) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
    
    bm_temp_DSS[condition] <- 
      lapply(bm_temp_DSS[condition], function(x) ((x/bm_temp_DSS$plot_num_stand)))
    
    condition_DSS <- (names(bm_temp_DSS) %in% c("w_volume_m3_ha","w_biomass_ton_ha","w_AG_biomass_ton_ha","w_carbon_stock_tC_ha","w_co2_stock_tCO2_ha"))
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    
    condition_var <- (names(bm_temp_DSS) %in% c("var_volume_m3_ha","var_biomass_ton_ha","var_AG_biomass_ton_ha","var_carbon_stock_tC_ha","var_co2_stock_tCO2_ha"))
    bm_temp_DSS[condition_var] <- 
      lapply(bm_temp_DSS[condition_var], function(x) ((bm_temp_DSS$weight_DSS)^2*(x/bm_temp_DSS$plot_num_stand)))
    
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(CYCLE, INVYR, !!!plotgrp, !!!treegrp) %>% 
      summarise(w_volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
                w_biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
                w_AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
                w_carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
                w_co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
    
    
    # 2.1.3 Aggregating calculated biomass per forest type by study area.
    bm_temp_DSS[condition_DSS] <-  NULL
    bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("CYCLE", "INVYR", as.character(unlist(lapply(plotgrp, quo_name))),
                                                             as.character(unlist(lapply(treegrp, quo_name)))))
    
    bm_temp_DSS$var_volume_m3_ha <- bm_temp_DSS$var_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$volume_m3_ha-bm_temp_DSS$w_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_biomass_ton_ha <- bm_temp_DSS$var_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$biomass_ton_ha-bm_temp_DSS$w_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_AG_biomass_ton_ha <- bm_temp_DSS$var_AG_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$AG_biomass_ton_ha-bm_temp_DSS$w_AG_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_carbon_stock_tC_ha <- bm_temp_DSS$var_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$carbon_stock_tC_ha-bm_temp_DSS$w_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_co2_stock_tCO2_ha <- bm_temp_DSS$var_co2_stock_tCO2_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$co2_stock_tCO2_ha-bm_temp_DSS$w_co2_stock_tCO2_ha)^2/bm_temp_DSS$plot_num_year)
    
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(CYCLE, INVYR, !!!plotgrp, !!!treegrp) %>% 
      summarise(volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
                var_volume_m3_ha = sum(var_volume_m3_ha, na.rm=TRUE),
                biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
                var_biomass_ton_ha = sum(var_biomass_ton_ha, na.rm=TRUE),
                AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
                var_AG_biomass_ton_ha = sum(var_AG_biomass_ton_ha, na.rm=TRUE),
                carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
                var_carbon_stock_tC_ha = sum(var_carbon_stock_tC_ha, na.rm=TRUE),
                co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE),
                var_co2_stock_tCO2_ha = sum(var_co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
    
    
    # 2.1.4 Weighted Moving Average(to combine annual inventory field data)
    weight_WMA <- full_join(weight_year, weight_plotgrp, by =c("CYCLE", as.character(unlist(lapply(plotgrp, quo_name)))))
    weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
    
    
    bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("CYCLE","INVYR", as.character(unlist(lapply(plotgrp, quo_name)))))
    
    condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
    
    
    bm_temp_WMA[condition] <- 
      lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
    
    
    bm <- bm_temp_WMA %>% 
      group_by(CYCLE, !!!plotgrp, !!!treegrp) %>% 
      summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                var_volume_m3_ha = sum(weight_WMA^2*var_volume_m3_ha, na.rm=TRUE),
                se_volume_m3_ha = sqrt(var_volume_m3_ha),
                rse_volume_m3_ha = se_volume_m3_ha/volume_m3_ha*100,
                biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                var_biomass_ton_ha = sum(weight_WMA^2*var_biomass_ton_ha, na.rm=TRUE),
                se_biomass_ton_ha = sqrt(var_biomass_ton_ha),
                rse_biomass_ton_ha = se_biomass_ton_ha/biomass_ton_ha*100,
                AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                var_AG_biomass_ton_ha = sum(weight_WMA^2*var_AG_biomass_ton_ha, na.rm=TRUE),
                se_AG_biomass_ton_ha = sqrt(var_AG_biomass_ton_ha),
                rse_AG_biomass_ton_ha = se_AG_biomass_ton_ha/AG_biomass_ton_ha*100,
                carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                var_carbon_stock_tC_ha = sum(weight_WMA^2*var_carbon_stock_tC_ha, na.rm=TRUE),
                se_carbon_stock_tC_ha = sqrt(var_carbon_stock_tC_ha),
                rse_carbon_stock_tC_ha = se_carbon_stock_tC_ha/carbon_stock_tC_ha*100,
                co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),
                var_co2_stock_tCO2_ha = sum(weight_WMA^2*var_co2_stock_tCO2_ha, na.rm=TRUE),
                se_co2_stock_tCO2_ha = sqrt(var_co2_stock_tCO2_ha),
                rse_co2_stock_tCO2_ha = se_co2_stock_tCO2_ha/co2_stock_tCO2_ha*100,.groups = 'drop')
    
    
    
    
    
  }else{ # 2.2 Biomass calculation by plot
    
    bm <- bm_temp
    
  }
  
  
  
  
  
  
  
  return(bm)
  
}



#' biomass_tsvis()
#' 
#' @description
#' biomass_tsvis() function estimates volume, aboveground biomass, biomass, carbon storage and carbon dioxide storage.
#' It is an internal function used within the tsvis_nfi() function.
#' biomass_nfi() calculates biomass by cycle, while this function calculates biomass by 5 year.
#'
#' @param data : A `list` generated by \code{\link{read_nfi}} that contains 'plot' and 'tree' data frames.
#' @param plotgrp : A character vector; variables from 'plot' tables for grouping. Use \code{c()} to combine multiple variables.
#' @param treegrp : A character vector; variables from 'tree' tables for grouping. Use \code{c()} to combine multiple variables.
#' @param strat : A character vector; the variable used for post-stratification. In the National Forest Inventory of Korea, it is typically used by forest type.
#' @param clusterplot : A logical flag (default FALSE); if TRUE, treats each cluster plot as a single unit. If FALSE, calculates for each subplot separately.
#' @param largetreearea : A logical flag (default TRUE); if TRUE, includes large tree survey plots in the analysis. If FALSE, only uses standard tree plots.
#' @param stockedland : A logical flag (default TRUE); if TRUE, includes only stocked land. If FALSE, includes all land types.
#' @param talltree : A logical flag (default TRUE); if TRUE, includes only tall trees. If FALSE, includes both trees and shrubs.
#' 
#' @noRd



biomass_tsvis <- function(data, plotgrp=NULL, treegrp=NULL, strat="FORTYP_SUB", clusterplot=FALSE, largetreearea=TRUE, stockedland=TRUE, talltree=TRUE){
  
  ## error message--------------------------------------------------------------  
  required_names <- c("plot", "tree")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  if (!is.null(plotgrp)){
    if(any(plotgrp %in% strat)){
      warning("param 'plotgrp' is the same as param 'strat'")
      strat <- "strat"
      data$plot$strat <- "same"
    }
    if(!is.character(plotgrp)) {
      stop("param 'plotgrp' must be 'character'")
    }
    if(any(!plotgrp %in% names(data$plot))){
      stop(paste0("param 'plotgrp': ", plotgrp," is not a column name in the 'plot' data frame."))
    }
  }
  
  if (clusterplot){
    if(!is.null(strat) && strat=="FORTYP_SUB"){
      stop("When the param 'clusterplot' is set to TRUE, param 'strat' uses FORTYP_CLST (the forest type for the cluster plot) instead of FORTYP_SUB (the forest type for each subplot).")
    }
    
    if(!is.null(plotgrp) && plotgrp=="FORTYP_SUB"){
      stop("When the param 'clusterplot' is set to TRUE, param 'plotgrp' uses FORTYP_CLST (the forest type for the cluster plot) instead of FORTYP_SUB (the forest type for each subplot).")
    }
  }
  
  
  if (!is.null(treegrp)){
    
    if(!is.character(treegrp)) {
      stop("param 'treegrp' must be 'character'")
    }
    if(any(!treegrp %in% names(data$tree))){
      stop(paste0("param 'treegrp': ",treegrp," is not a column name in the 'tree' data frame."))
    }
  }
  
  if (!is.null(strat)){
    if(!is.character(strat)) {
      stop("param 'strat' must be 'character'")
    }
    if(!strat %in% names(data$plot)){
      stop(paste0("param 'strat': ", strat," is not a column name in the 'plot' data frame."))
    }
    
  }
  
  
  
  ## Preprocessing-------------------------------------------------------------- 
  if (stockedland){ 
    data <- filter_nfi(data, c("plot$LAND_USECD == '1'"))
  }
  
  if(talltree){
    data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == "1")
  }
  
  if ("FORTYP_SUB" %in% names(data$plot)) {
    data$plot <- data$plot %>% filter(!is.na(FORTYP_SUB))
  }
  
  df <- left_join(data$tree[,c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'WDY_PLNTS_TYP_CD','SP', 'SPCD',
                               'CONDEC_CLASS_CD', 'DECEVER_CD', 'DBH', 'VOL_EST',  'LARGEP_TREE', treegrp )], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD",
                               'NONFR_INCL_AREA_SUBP', 'NONFR_INCL_AREA_LARGEP', "SGG_CD", 'SIDO_CD', strat, plotgrp)],
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
  

  if (!is.numeric(df$VOL_EST)){
    df$VOL_EST <- as.numeric(df$VOL_EST)
  } 
  
  if(!largetreearea){ 
    df <- df %>% filter(df$LARGEP_TREE == "0")
    df$largetree <- 0
  }else{
    df$largetree <- ifelse(df$DBH>=30, 1, 0)
  }
  
  df$largetree_area <- 0.08 - ((df$NONFR_INCL_AREA_LARGEP*10)/10000) # unit m2/10
  df$tree_area <- 0.04 - ((df$NONFR_INCL_AREA_SUBP*10)/10000)
  
  df <- bm_df(df)
  
  if(clusterplot){
    plot_id <- c('CLST_PLOT')
  }else{
    plot_id <- c('SUB_PLOT')
  }
  
  plot_id  <- rlang::sym(plot_id)
  plotgrp  <- rlang::syms(plotgrp)
  treegrp  <- rlang::syms(treegrp)
  strat<- rlang::sym(strat)
  
  if(!largetreearea){
    largetree <- NULL
  }
  
  
  # 1.Biomass calculation by cluster or subplot
  if(clusterplot){ # 1.1 Biomass calculation by cluster plots
    
    plot_area <- df[-which(duplicated(df[c('SUB_PLOT', 'CYCLE')])),c('CYCLE', 'INVYR', 'CLST_PLOT', 'SUB_PLOT', 'largetree_area', 'tree_area')]

    
    plot_area <- plot_area %>%
      group_by(CYCLE, !!plot_id,  INVYR) %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')
    
 
    bm_temp <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, largetree, !!!plotgrp, !!!treegrp, !!strat) %>% 
      summarise(volume_m3 = sum(VOL_EST, na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    
    bm_temp <- full_join(bm_temp, plot_area, by=c('CYCLE', 'INVYR', quo_name(plot_id)))
    
    
    if(!largetreearea){ # 1.1.1 Biomass calculation by cluster plots excluding large tree survey plots
      
      condition_ha <- c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha")
      bm_temp[condition_ha] <-  NA
      bm_temp <- as.data.frame(bm_temp)
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      
      bm_temp[condition_ha] <- 
        lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      
      bm_temp[condition] <- NULL
      bm_temp$tree_area <- NULL
      bm_temp$largetree_area <- NULL
      
      
    }else{ # 1.1.2 Biomass calculation by cluster plots including large tree survey plots
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!plotgrp, !!!treegrp, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
    
    
    
    
    
    
  }else{ # 1.2 Biomass calculation by subplots 
    
    bm_temp <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, !!strat, largetree, !!!plotgrp, !!!treegrp, largetree_area, tree_area) %>% 
      summarise(volume_m3 = sum(VOL_EST, na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    if(!largetreearea){ # 1.2.1 Biomass calculation by subplots excluding large tree survey plots
      
      condition_ha <- c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha")
      bm_temp[condition_ha] <-  NA
      bm_temp <- as.data.frame(bm_temp)
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      
      bm_temp[condition_ha] <- 
        lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
      
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      
      bm_temp[condition] <- NULL
      bm_temp$tree_area <- NULL
      bm_temp$largetree_area <- NULL
      
      
    }else{ # 1.2.2 Biomass calculation by subplots including large tree survey plots
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!plotgrp, !!!treegrp, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
  }
  
  # 2.1 Biomass calculation using post-stratified double sampling and weighted moving average methods
  # 2.1.1 Double sampling for post-strat(forest stand)
  weight_plotgrp <- data$plot %>%  # not CYCLE
    group_by(!!!plotgrp) %>% 
    summarise(plot_num_all = n_distinct(!!plot_id),.groups = 'drop')
  
  
  weight_year <- data$plot %>% 
    group_by(CYCLE, INVYR, !!!plotgrp) %>% 
    summarise(plot_num_year = n_distinct(!!plot_id),.groups = 'drop')
  
  
  weight_stand <- data$plot %>% 
    group_by(CYCLE, INVYR, !!strat, !!!plotgrp) %>% 
    summarise(plot_num_stand = n_distinct(!!plot_id),.groups = 'drop')
  
  
  weight_DSS <- full_join(weight_stand, weight_year, by =c("CYCLE", "INVYR", as.character(unlist(lapply(plotgrp, quo_name)))))
  weight_DSS$weight_DSS <- weight_DSS$plot_num_stand/weight_DSS$plot_num_year
  
  
  # 2.1.2 Aggregating calculated biomass per plot by forest type.
  bm_temp_DSS <- bm_temp %>% 
    group_by(CYCLE, INVYR, !!strat, !!!plotgrp, !!!treegrp) %>% 
    summarise(var_volume_m3_ha =  var(volume_m3_ha, na.rm=TRUE),
              volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
              var_biomass_ton_ha =  var(biomass_ton_ha, na.rm=TRUE),
              biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
              var_AG_biomass_ton_ha =  var(AG_biomass_ton_ha, na.rm=TRUE),
              AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
              var_carbon_stock_tC_ha =  var(carbon_stock_tC_ha, na.rm=TRUE),
              carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
              var_co2_stock_tCO2_ha =  var(co2_stock_tCO2_ha, na.rm=TRUE),
              co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
  
  
  bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("CYCLE", "INVYR", quo_name(strat), as.character(unlist(lapply(plotgrp, quo_name)))))
  
  
  condition_DSS <- c("w_volume_m3_ha","w_biomass_ton_ha","w_AG_biomass_ton_ha","w_carbon_stock_tC_ha","w_co2_stock_tCO2_ha")
  bm_temp_DSS[condition_DSS] <-  NA
  bm_temp_DSS <- as.data.frame(bm_temp_DSS)
  
  condition <- (names(bm_temp_DSS) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
  
  bm_temp_DSS[condition] <- 
    lapply(bm_temp_DSS[condition], function(x) ((x/bm_temp_DSS$plot_num_stand)))
  
  condition_DSS <- (names(bm_temp_DSS) %in% c("w_volume_m3_ha","w_biomass_ton_ha","w_AG_biomass_ton_ha","w_carbon_stock_tC_ha","w_co2_stock_tCO2_ha"))
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  condition_var <- (names(bm_temp_DSS) %in% c("var_volume_m3_ha","var_biomass_ton_ha","var_AG_biomass_ton_ha","var_carbon_stock_tC_ha","var_co2_stock_tCO2_ha"))
  bm_temp_DSS[condition_var] <- 
    lapply(bm_temp_DSS[condition_var], function(x) ((bm_temp_DSS$weight_DSS)^2*(x/bm_temp_DSS$plot_num_stand)))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(CYCLE, INVYR, !!!plotgrp, !!!treegrp) %>% 
    summarise(w_volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
              w_biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
              w_AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
              w_carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
              w_co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
  
  
  # 2.1.3 Aggregating calculated biomass per forest type by study area.
  bm_temp_DSS[condition_DSS] <-  NULL
  bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("CYCLE", "INVYR", as.character(unlist(lapply(plotgrp, quo_name))),
                                                           as.character(unlist(lapply(treegrp, quo_name)))))
  
  bm_temp_DSS$var_volume_m3_ha <- bm_temp_DSS$var_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$volume_m3_ha-bm_temp_DSS$w_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_biomass_ton_ha <- bm_temp_DSS$var_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$biomass_ton_ha-bm_temp_DSS$w_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_AG_biomass_ton_ha <- bm_temp_DSS$var_AG_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$AG_biomass_ton_ha-bm_temp_DSS$w_AG_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_carbon_stock_tC_ha <- bm_temp_DSS$var_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$carbon_stock_tC_ha-bm_temp_DSS$w_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_co2_stock_tCO2_ha <- bm_temp_DSS$var_co2_stock_tCO2_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$co2_stock_tCO2_ha-bm_temp_DSS$w_co2_stock_tCO2_ha)^2/bm_temp_DSS$plot_num_year)
  
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(CYCLE, INVYR, !!!plotgrp, !!!treegrp) %>% 
    summarise(volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
              var_volume_m3_ha = sum(var_volume_m3_ha, na.rm=TRUE),
              biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
              var_biomass_ton_ha = sum(var_biomass_ton_ha, na.rm=TRUE),
              AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
              var_AG_biomass_ton_ha = sum(var_AG_biomass_ton_ha, na.rm=TRUE),
              carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
              var_carbon_stock_tC_ha = sum(var_carbon_stock_tC_ha, na.rm=TRUE),
              co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE),
              var_co2_stock_tCO2_ha = sum(var_co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
  
  
  # var2_volume_m3_ha = sum(weight_DSS*(volume_m3_ha-w_volume_m3_ha)^2/plot_num_year),
  
  
  # 2.1.4 Weighted Moving Average(to combine annual inventory field data)
  weight_WMA <- full_join(weight_year, weight_plotgrp, by =c(as.character(unlist(lapply(plotgrp, quo_name)))))
  weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
  
  
  bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("CYCLE","INVYR", as.character(unlist(lapply(plotgrp, quo_name)))))
  
  condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
  
  
  bm_temp_WMA[condition] <- 
    lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
  
  
  bm <- bm_temp_WMA %>% 
    group_by(!!!plotgrp, !!!treegrp) %>% 
    summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
              var_volume_m3_ha = sum(weight_WMA^2*var_volume_m3_ha, na.rm=TRUE),
              se_volume_m3_ha = sqrt(var_volume_m3_ha),
              rse_volume_m3_ha = se_volume_m3_ha/volume_m3_ha*100,
              biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
              var_biomass_ton_ha = sum(weight_WMA^2*var_biomass_ton_ha, na.rm=TRUE),
              se_biomass_ton_ha = sqrt(var_biomass_ton_ha),
              rse_biomass_ton_ha = se_biomass_ton_ha/biomass_ton_ha*100,
              AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
              var_AG_biomass_ton_ha = sum(weight_WMA^2*var_AG_biomass_ton_ha, na.rm=TRUE),
              se_AG_biomass_ton_ha = sqrt(var_AG_biomass_ton_ha),
              rse_AG_biomass_ton_ha = se_AG_biomass_ton_ha/AG_biomass_ton_ha*100,
              carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
              var_carbon_stock_tC_ha = sum(weight_WMA^2*var_carbon_stock_tC_ha, na.rm=TRUE),
              se_carbon_stock_tC_ha = sqrt(var_carbon_stock_tC_ha),
              rse_carbon_stock_tC_ha = se_carbon_stock_tC_ha/carbon_stock_tC_ha*100,
              co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),
              var_co2_stock_tCO2_ha = sum(weight_WMA^2*var_co2_stock_tCO2_ha, na.rm=TRUE),
              se_co2_stock_tCO2_ha = sqrt(var_co2_stock_tCO2_ha),
              rse_co2_stock_tCO2_ha = se_co2_stock_tCO2_ha/co2_stock_tCO2_ha*100,.groups = 'drop')
  
  
  
  
  return(bm)
  
}


