# Standardize covariates and examine them for correlations

library(data.table)
library(purrr)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)
library(ggpubr)
 

## Load summarized and extracted covariates ------------------------------------

  #list all files
  csv_list <- list.files('spatial/summarized_covariates/', pattern = '.csv', full.names = TRUE)

  #read and combine into a list
  file_list <- lapply(csv_list, fread)
  
  #full merge
  covars <- reduce(file_list, full_join) #will find matching columns for merging
    names(covars)
    
    
## Summaries to report ---------------------------------------------------------

  #include SD      
  sapply(covars, function(x) if(is.numeric(x)) round(c(summary(x), 'SD' = sd(x, na.rm = TRUE)), 4))
  
  
## Replace NAs if necessary ----------------------------------------------------
  
  nrow(covars[is.na(covars$prop_M1_3x3)]) 
    covars[is.na(covars$prop_M1_3x3)]$Site
    
  (hex_to_fix <- c('K26','M26'))
  #K26-C and M26-C are outside the modeled area for everything except elevation
    #Replace with avg values at sites A and B for these hexagons
    #They do have values for distances to dambo and human, but there is no land cover mapped 
    # in those areas so we shouldn't use them.

  covars[covars$HexIDcorre %in% hex_to_fix,]
  
  #get names of covar to replace
  (cov_to_replace <- names(covars)[grepl('slope|tree|dist|prop', names(covars))])
    
  #create new object
  covars_no_na <- covars
  
  #loop thru...
  for (hh in hex_to_fix){
    
    #get rows
    covars_hh <- covars[covars$HexIDcorre == hh,]
    
    #loop thru covar to replace
    for (cc in cov_to_replace){
      
      #get avg from stations A and B for this covariate
      value_a <- covars_hh[covars_hh$PlotID == 'A', ..cc]
      value_b <- covars_hh[covars_hh$PlotID == 'B', ..cc]
      avg_a_b <- mean(c(value_a[[1]], value_b[[1]]))
      
      #replace current value for station C with this avg
      set(covars_no_na, which(covars_no_na$Site == paste0(hh, '-C')), cc, avg_a_b)
      
    }
  }
  
  #check
  covars_no_na[covars_no_na$HexIDcorre %in% hex_to_fix,] #great
  
  #any more NAs?
  colSums(is.na(covars_no_na)) #good, no NAs in the covar fields. proceed
  colSums(is.na(covars)) #why did this one get fixed too? so confused
  
    
## Standardize continuous var --------------------------------------------------
    
  #get names for continuous covariates and land cover (proportion) covariate columns separately  
  (covar_names <- names(covars)[grepl('3x3|5x5|dist', names(covars))])
  (covar_names_cont <- covar_names[!grepl('prop', covar_names)])    
  (covar_names_lc <- covar_names[grepl('prop', covar_names)])    
    
  #I guess all of them should be standardized? even proportions?
  
  #scale
  covars_scaled <- setDF(covars_no_na) #convert from data.table to dataframe
  covars_scaled[covar_names] <- scale(covars_scaled[covar_names])    
    
  #check for mean~0, sd~1
  sapply(covars_scaled, function(x) if(is.numeric(x)) round(c(summary(x), 'SD' = sd(x, na.rm = TRUE)), 4))
    #NAs are ok for Afromontane, Water, Rock -- we're not using them
    
    
## Test for correlations -------------------------------------------------------
  
  #use non-standardized values here
  
  #let's just use 3x3 for correlation tests. should be the same for 5x5
  #also remove Afromontane, Dambo, Water classes bc we're not using them and they are all 0s
  covar_names_3x3 <- covar_names[!grepl('5x5|Afromontane|Water|Rock|Dambo', covar_names, ignore.case = FALSE)]
  covar_names_3x3 #make sure it kept 'dist_dambo' but not 'prop_Dambo"
  
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
  
  #are they normally distributed? shapiro test and qqplots
  for (nn in covar_names_3x3){
    shap <- shapiro.test(covars[,nn])
    if(shap$p.value < 0.05) cat(nn, ': Not normally distributed\n') else cat(nn, ': Normally distributed\n')
    qqplot <- ggpubr::ggqqplot(covars[,nn]) + ggtitle(nn)
    print(qqplot)
  }
  
  #all are NOT normally distributed. use spearman or kendall test
  (cors1 <- round(cor(covars[,covar_names_3x3], use = 'complete.obs', method = 'spearman'),2))
  (cors2 <- rcorr(as.matrix(covars[,covar_names_3x3], type = 'spearman')))
  
  #flatten and save
  (corrMatrix <- flattenCorrMatrix(cors2$r, cors2$P))
  write.csv(corrMatrix, 'data/cleaned/covariates/covar_correlations.csv')
  write.csv(cors2$r, 'data/cleaned/covariates/covar_correlations_matrix_r.csv')
  
  #symbolic correlation matrix
  symnum(cors1, abbr.colnames = FALSE)  
  
  #correlograms
  corrplot(cors2$r, type = 'upper', order = 'hclust', tl.col = 'black', tl.srt = 45,  
           p.mat = cors2$P, sig.level = 0.01, insig = 'blank') #or insig = 'pch'
  
  #scatter plots
  chart.Correlation(covars[,covar_names_3x3], method = 'spearman', histogram = TRUE, pch = '+')
  
  #heatmap
  col<- colorRampPalette(c("blue", "white", "red"))(20)
  heatmap(x = cors1, col = col, symm = TRUE, margins = c(12,10))
  dev.off()
  
  #Correlations above 0.6 threshold:
  corrMatrix[abs(corrMatrix$cor) > 0.6,]
    
    #ELEV_SD and PCT_SLOPE_MEAN
    #TREE_VOL_MEAN and PROP_M1
    #TREE_VOL_MEAN and PROP_M3
    #PROP_M1 and PROP_M3 (makes sense. more open miombo = less dense miombo)
    #DIST_HUMAN and PROP_HUMAN (make sense. wouldn't use both anyway)
    
  ## ALSO SHOULD TEST FENCE NORTH/SOUTH
  
  #plot
  
  # ELEV_SD ~ PCT_SLOPE_MEAN
  ggscatter(covars, x = 'elev_sd_3x3', y = 'pct_slope_mean_3x3',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman')
  
  # TREE_VOL_MEAN ~ PROP_M1
  ggscatter(covars, x = 'tree_vol_mean_3x3', y = 'prop_M1_3x3',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman')
  
  # TREE_VOL_MEAN ~ PROP_M3
  ggscatter(covars, x = 'tree_vol_mean_3x3', y = 'prop_M3_3x3',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman')
  
  # PROP_M1 ~ PROP_M3
  ggscatter(covars, x = 'prop_M1_3x3', y = 'prop_M3_3x3',
            add = 'reg.line', conf.int = TRUE,
            cor.coef = TRUE, cor.method = 'spearman')
  
    
## Save ------------------------------------------------------------------------
  
  write.csv(covars_scaled, 'data/cleaned/covariates/cleaned_covariates.csv')
  
  