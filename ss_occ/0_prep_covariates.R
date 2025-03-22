# Single-season occupancy models ##THIS WAS FOR HEX-LEVEL DATA. SEE SCRIPT IN 'SPATIAL' FOLDER NOW INSTEAD

## Step 1:
## - prep covariates
## - test for correlations

library(data.table)
library(tidyverse)
library(RPresence)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)


## CREATE FOLDERS TO SAVE OUTPUTS ----------------------------------------------

  ifelse(!dir.exists(paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')), 
         dir.create(paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')), 'Folder exists already')
    print(paste('created folder', paste('ss_occ/outputs/dynamic', Sys.Date(), sep = '/')))
  
  ifelse(!dir.exists(paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')), 
         dir.create(paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')), 'Folder exists already')
    print(paste('created folder', paste('ss_occ/figures/dynamic', Sys.Date(), sep = '/')))

        
## IMPORT COVARIATES -----------------------------------------------------------  
    
#Read in geospatial data from Ray Davis
  hex_data <- fread('data/raw/covariates/from_Ray_2024/Covariates for Cara.csv')
  hex_data$hex <- gsub('-', '', hex_data$GRID_ID, fixed = TRUE)
  hex_data <- hex_data[order(hex_data$hex)] #order alphabetically by hexagon

#Import and merge fence covariate (unless Ray adds it to above)
  buaCovar <- fread('data/raw/covariates/hexagon_tph_summaries_monitored.dbf.csv')
  hex_data$BuaRiver <- buaCovar$BuaRiver[match(hex_data$GRID_ID, buaCovar$GRID_ID)]
    table(hex_data$BuaRiver, useNA = 'a', hex_data$Sample) #all NAs were hexes not monitored
    
  ## Calculate proportion for landcover cov (Ray calculated M1p etc. but those all have 500 as denominator)
  hex_data$M1_prop <- hex_data$M1 / hex_data$TOTlc #M1: miombo woodland (open)
  hex_data$M2_prop <- hex_data$M2 / hex_data$TOTlc #M2: miombo woodland (moderate)
  hex_data$M3_prop <- hex_data$M3 / hex_data$TOTlc #M3: miombo woodland (dense)
    
  ## Keep only monitored hexagons
  hex_data <- hex_data[hex_data$Sample == 1,]
    
  ## Convert Bua River to indicator
  hex_data$BuaRiver_indicator <- ifelse(hex_data$BuaRiver == 'North', 0, 1) #0:North, 1:South
    
    
## SUMMARIZE COVARIATES (TO REPORT) --------------------------------------------
      
  #Miombo woodland cover type
    round(summary(hex_data$M1_prop),2); sd(hex_data$M1_prop)
    round(summary(hex_data$M2_prop),2); sd(hex_data$M2_prop)
    round(summary(hex_data$M3_prop),2); sd(hex_data$M3_prop)
    
  #Tree volume
    # summary(hex_data$TREE_VOLm) #'no data' cells are 0
    round(summary(hex_data$TREEVOLmnd),2); sd(hex_data$TREEVOLmnd) #even incomplete cells have data
    
  #Slope mean
    # summary(hex_data$SLP_PCTm) #'no data' cells are 0
    round(summary(hex_data$SLPPCTmnd),2); sd(hex_data$SLPPCTmnd) #even incomplete cells have data
    
  #Slope SD
    # summary(hex_data$SLP_PCTsd) #'no data' cells are 0
    round(summary(hex_data$SLPPCTsdnd),2); sd(hex_data$SLPPCTsdnd) #even incomplete cells have data
    
  #Distance to nearest dambo
    round(summary(hex_data$DISTDAMBOm),2); sd(hex_data$DISTDAMBOm) 
    
  #Mean years of fire from 2017-2021
    round(summary(hex_data$FIRE_YRSm),2); sd(hex_data$FIRE_YRSm)
    
  #Elevation mean
    round(summary(hex_data$ELEVm),2); sd(hex_data$ELEVm) #full coverage DEM so no prob with 'no data'
    
  #Elevation SD
    round(summary(hex_data$ELEVsd),2); sd(hex_data$ELEVsd)
    
  #Human disturbance model (cost distance w/ elevation AND distance from nearest settlement)
    round(summary(hex_data$COST_ELEVm),2); sd(hex_data$COST_ELEVm)
    
  #Bua River (north/south)
    table(hex_data$BuaRiver)
    table(hex_data$BuaRiver_indicator)
    
    
## REPLACE NAS IF NECESSARY ----------------------------------------------------
    
  #Any NAs to replace (e.g., with means)?
  nrow(hex_data[is.na(hex_data$M1_prop),]) #none
  nrow(hex_data[is.na(hex_data$M2_prop),]) #none
  nrow(hex_data[is.na(hex_data$M3_prop),]) #none
  nrow(hex_data[is.na(hex_data$BuaRiver),]) #none
  nrow(hex_data[is.na(hex_data$TREEVOLmnd),]) #none
  nrow(hex_data[is.na(hex_data$SLPPCTmnd),]) #none
  nrow(hex_data[is.na(hex_data$SLPPCTsdnd),]) #none
  nrow(hex_data[is.na(hex_data$DISTDAMBOm),]) #none
  nrow(hex_data[is.na(hex_data$FIRE_YRSm),]) #none
  nrow(hex_data[is.na(hex_data$ELEVm),]) #none
  nrow(hex_data[is.na(hex_data$ELEVsd),]) #none
  nrow(hex_data[is.na(hex_data$COST_ELEVm),]) #none
  
  
## TEST FOR CORRELATIONS -------------------------------------------------------
  
  #create function to format correlation dataframe
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  
  #are they normally distributed? #if NOT normally distrib., use spearman or kendall
  shapiro.test(hex_data$M1_prop); ggpubr::ggqqplot(hex_data$M1_prop) #p<0.05 (not normal)
  shapiro.test(hex_data$M2_prop); ggpubr::ggqqplot(hex_data$M2_prop) #p>0.05 (yes, normal)
  shapiro.test(hex_data$M3_prop); ggpubr::ggqqplot(hex_data$M3_prop) #p<0.05 (not normal)
  shapiro.test(hex_data$TREEVOLmnd); ggpubr::ggqqplot(hex_data$TREEVOLmnd) #p<0.05 (not normal)
  shapiro.test(hex_data$SLPPCTmnd); ggpubr::ggqqplot(hex_data$SLPPCTmnd) #p<0.05 (not normal)
  shapiro.test(hex_data$SLPPCTsdnd); ggpubr::ggqqplot(hex_data$SLPPCTsdnd) #p<0.05 (not normal)
  shapiro.test(hex_data$DISTDAMBOm); ggpubr::ggqqplot(hex_data$DISTDAMBOm) #p<0.05 (not normal)
  shapiro.test(hex_data$FIRE_YRSm); ggpubr::ggqqplot(hex_data$FIRE_YRSm) #p<0.05 (not normal)
  shapiro.test(hex_data$ELEVm); ggpubr::ggqqplot(hex_data$ELEVm) #p<0.05 (not normal)
  shapiro.test(hex_data$ELEVsd); ggpubr::ggqqplot(hex_data$ELEVsd) #p<0.05 (not normal)
  shapiro.test(hex_data$COST_ELEVm); ggpubr::ggqqplot(hex_data$COST_ELEVm) #p<0.05 (not normal)
  
  #correlations
  (cors1 <- round(cor(hex_data[,c('M1_prop','M2_prop','M3_prop','TREEVOLmnd','SLPPCTmnd','SLPPCTsdnd','DISTDAMBOm',
                                 'BuaRiver_indicator','FIRE_YRSm','ELEVm','ELEVsd','COST_ELEVm')], 
                      use = 'complete.obs', method = 'spearman'),2))
  (cors2 <- rcorr(as.matrix(hex_data[,c('M1_prop','M2_prop','M3_prop','TREEVOLmnd','SLPPCTmnd','SLPPCTsdnd','DISTDAMBOm',
                                       'BuaRiver_indicator','FIRE_YRSm','ELEVm','ELEVsd','COST_ELEVm')]), type = 'spearman'))
  
  #flatten and save
  (corrMatrix <- flattenCorrMatrix(cors2$r, cors2$P))
  write.csv(corrMatrix, 'data/cleaned/covar_correlations.csv')
  
  #symbolic correlation matrix
  symnum(cors1, abbr.colnames = FALSE)  
  
  #correlograms
  corrplot(cors2$r, type = 'upper', order = 'hclust', tl.col = 'black', tl.srt = 45,  
           p.mat = cors2$P, sig.level = 0.01, insig = 'pch') #or insig = 'blank'

  #scatter plots
  chart.Correlation(hex_data[,c('M1_prop','M2_prop','M3_prop','TREEVOLmnd','SLPPCTmnd','SLPPCTsdnd','DISTDAMBOm',
                               'BuaRiver_indicator','FIRE_YRSm','ELEVm','ELEVsd','COST_ELEVm')], histogram = TRUE, pch = 19)
  
  #heatmap
  col<- colorRampPalette(c("blue", "white", "red"))(20)
  heatmap(x = cors1, col = col, symm = TRUE, margins = c(12,10))
  dev.off()
  
  ## OK, using threshold of 0.6, it looks like we have correlations between:
  # M1 and M3
  # M1 and TREE_VOL
  # M3 and TREE_VOL
  # Bua and M1
  # Bua and M3
  # Bua and TREE_VOL
  # SLOPE_MEAN and SLOPE_SD
  # SLOPE_MEAN and ELEV_MEAN
  # SLOPE_MEAN and ELEV_SD
  # SLOPE_SD and ELEV_SD
  # ELEV_MEAN and ELEV_SD
  
  # M1 ~ M3
  ggscatter(hex_data, x = 'M1_prop', y = 'M3_prop',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman',
            xlab = 'M1_prop', ylab = 'M3_prop')
  
  # M1 ~ TREE_VOL
  ggscatter(hex_data, x = 'M1_prop', y = 'TREEVOLmnd',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman',
            xlab = 'M1_prop', ylab = 'TREE_VOL')
  
  # M3 ~ TREE_VOL
  ggscatter(hex_data, x = 'M3_prop', y = 'TREEVOLmnd',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman',
            xlab = 'M3_prop', ylab = 'TREE_VOL')
  
  # Bua ~ M1
  boxplot(hex_data$M1_prop ~ hex_data$BuaRiver, xlab = 'BuaRiver', ylab = 'M1_prop')
  
  # Bua ~ M3
  boxplot(hex_data$M3_prop ~ hex_data$BuaRiver, xlab = 'BuaRiver', ylab = 'M3_prop')
  
  # Bua ~ TREE_VOL
  boxplot(hex_data$TREEVOLmnd ~ hex_data$BuaRiver, xlab = 'BuaRiver', ylab = 'TREE_VOL')
    
  # SLOPE_MEAN and SLOPE_SD
  ggscatter(hex_data, x = 'SLPPCTmnd', y = 'SLPPCTsdnd',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman',
            xlab = 'SLOPE_MEAN', ylab = 'SLOPE_SD') 
  
  # SLOPE_MEAN and ELEV_MEAN
  ggscatter(hex_data, x = 'SLPPCTmnd', y = 'ELEVm',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman',
            xlab = 'SLOPE_MEAN', ylab = 'ELEV_MEAN')
  
  # SLOPE_MEAN and ELEV_SD
  ggscatter(hex_data, x = 'SLPPCTmnd', y = 'ELEVsd',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman',
            xlab = 'SLOPE_MEAN', ylab = 'ELEV_SD')
  
  # SLOPE_SD and ELEV_SD
  ggscatter(hex_data, x = 'SLPPCTsdnd', y = 'ELEVsd',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman',
            xlab = 'SLOPE_SD', ylab = 'ELEV_SD')
  
  # ELEV_MEAN and ELEV_SD
  ggscatter(hex_data, x = 'ELEVm', y = 'ELEVsd',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman',
            xlab = 'ELEV_MEAN', ylab = 'ELEV_SD')
  
    
## STANDARDIZE -----------------------------------------------------------------
  hex_data$M1_prop_z <- scale(hex_data$M1_prop) 
  hex_data$M2_prop_z <- scale(hex_data$M2_prop) 
  hex_data$M3_prop_z <- scale(hex_data$M3_prop) 
  hex_data$TREEVOL_z <- scale(hex_data$TREEVOLmnd) 
  hex_data$SLOPE_MEAN_z <- scale(hex_data$SLPPCTmnd)
  hex_data$SLOPE_SD_z <- scale(hex_data$SLPPCTsdnd)
  hex_data$DAMBO_DIST_z <- scale(hex_data$DISTDAMBOm)
  hex_data$FIRE_YRS_z <- scale(hex_data$FIRE_YRSm) #should this be standardized?
  hex_data$ELEV_MEAN_z <- scale(hex_data$ELEVm)
  hex_data$ELEV_SD_z <- scale(hex_data$ELEVsd)
  hex_data$COST_ELEV_z <- scale(hex_data$COST_ELEVm)
  
## SAVE COVARIATES -------------------------------------------------------------
  
  write.csv(hex_data, 'data/cleaned/hex_covariates.csv')
      

  
  