#====================#
# ==== IO Pset 3 ====
#====================#



#=============================#
# ==== load packages/data ====
#=============================#


  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")
  
  library(data.table)
  library(xtable)
  
  # set option for who is running this 
  opt_nate <- TRUE
  
  # load data and set directories 
  if(opt_nate){
    
    # laod data 
    gmdt <- fread("c:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps3/GMdata.csv")
    f_out <- "c:/Users/Nmath_000/Documents/Code/Econ_631/ps3/"
  
    # if running on tyler's computer 
    }else{
      
      # load data from tylers locaiton 
      gmdt <- fread("c:/")
    
  }



#========================#
# ==== summary stats ====
#========================#

  #==============================#
  # ==== Make balanced panel ====
  #==============================#

  
    # make the balanced panel version of data 
    # first get number of years by firm 
    gmdt[, num_years := .N, index]
    
    # check how many are in each group 
    gmdt[, .N, num_years]
    
    # make balanced panel 
    gmdt_b <- gmdt[num_years == max(gmdt$num_years)]
    
    # drop variable 
    gmdt[, num_years := NULL]
    gmdt_b[, num_years := NULL]
    
  #========================#
  # ==== do comparison ====
  #========================#
    
    # make list of variables 
    vars <- grep("l", colnames(gmdt_b), value = TRUE)
    
    #  get each column of summart stats 
    sum_stats_li <- list()
    sum_stats_li[[1]] <- data.table(Variable = vars)
    # means 
    sum_stats_li[[2]] <- gmdt[, list("Full Mean" = lapply(.SD, mean)), .SDcols = vars]
    sum_stats_li[[3]] <- gmdt_b[, list("Balanced Mean" = lapply(.SD, mean)), .SDcols = vars]
    # varianvce 
    sum_stats_li[[4]] <- gmdt[, list("Full Variance" = lapply(.SD, var)), .SDcols = vars]
    sum_stats_li[[5]] <- gmdt_b[, list("Balanced Variance" = lapply(.SD, var)), .SDcols = vars]
    #min
    sum_stats_li[[6]] <- gmdt[, list("Full Min" = lapply(.SD, min)), .SDcols = vars]
    sum_stats_li[[7]] <- gmdt_b[, list("Balanced Min" = lapply(.SD, min)), .SDcols = vars]
    #max 
    sum_stats_li[[8]] <- gmdt[, list("Full Min" = lapply(.SD, max)), .SDcols = vars]
    sum_stats_li[[9]] <- gmdt_b[, list("Balanced Min" = lapply(.SD, max)), .SDcols = vars]

    # get variance 
    sum_stats <- do.call(cbind, sum_stats_li)
    #check it out! 
    sum_stats
    
    
  
  
  #============================#
  # ==== save table to tex ====
  #============================#

  
  
    # save summary stats 
    print(xtable(sum_stats, type = "latex"), 
          file = paste0(f_out, "sum_stats.tex"),
          include.rownames = FALSE,
          floating = FALSE)
  
  
  
  

