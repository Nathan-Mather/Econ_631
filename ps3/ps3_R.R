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
  
  # set option for who is running this 
  opt_nate <- TRUE
  
  # load data based on who is running 
  if(opt_nate){
    
    # laod data 
    gmdt <- fread("c:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps3/GMdata.csv")
  
    # if running on tyler's computer 
    }else{
      
      # load data from tylers locaiton 
      gmdt <- fread("c:/")
    
  }

  
#===========================#
# ==== useful functions ====
#===========================#

  ea_table <- function (in_data = NULL, in_vars_table = NULL, opt_percent = 0, 
                        opt_var_per_by_group = NULL){
    temp_data <- data.table::copy(data.table::data.table(in_data))
    temp_data <- temp_data[, list(count = .N), by = in_vars_table]
    setkeyv(temp_data, in_vars_table)
    if (opt_percent == 1) {
      temp_data[, `:=`(percent, count/sum(count) * 100), 
                by = opt_var_per_by_group]
    }
    out_data <- data.table::copy(temp_data)
    return(out_data[])
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
    sum_stats_li[[6]] <- gmdt[, list("Full Min" = lapply(.SD, min)), .SDcols = vars]
    sum_stats_li[[7]] <- gmdt_b[, list("Balanced Min" = lapply(.SD, min)), .SDcols = vars]
    
    # get variance 
    sum_stats <- do.call(cbind, sum_stats_li)
  
  
  
  
  
  
  
  
  
  
  
  

