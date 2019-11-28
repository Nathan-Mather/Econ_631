#====================#
# ==== IO Pset 3 ====
#====================#



#=============================#
# ==== load packages/data ====
#=============================#


  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
  options(scipen = 999)
  cat("\f")
  
  library(package)
  library(data.table)
  library(xtable)
  
  # set option for who is running this 
  opt_nate <- FALSE
  
  # load data and set directories 
  if(opt_nate){
    
    # load data 
    gmdt <- fread("c:/Users/Nmath_000/Documents/MI_school/Third Year/Econ 631/ps3/GMdata.csv")
    f_out <- "c:/Users/Nmath_000/Documents/Code/Econ_631/ps3/"
  
    # if running on tyler's computer 
    }else{
      
      # load data from tylers locaiton 
      gmdt <- fread("C:/Users/tyler/Box/coursework/Econ_631/ps3/GMdata.csv")
      f_out <- "C:/Users/tyler/Box/coursework/Econ_631/ps3"

    
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
    
    # t.test for mean equlity 
    # function to do it 
    tstat_fun <- function(var_i){
    res <- t.test(gmdt[, get(var_i)], gmdt_b[, get(var_i)])
    
    return(res$statistic)
    }
    
    # apply over variales 
    sum_stats_li[[4]] <- data.table( "Tstat" = unlist(lapply(vars, tstat_fun)))
    
    
    # varianvce 
    sum_stats_li[[5]] <- gmdt[, list("Full Variance" = lapply(.SD, var)), .SDcols = vars]
    sum_stats_li[[6]] <- gmdt_b[, list("Balanced Variance" = lapply(.SD, var)), .SDcols = vars]
    
    
    # fstat for equalit of variance 
    ftest_fun <- function(var_i){
      res <- var.test(gmdt[, get(var_i)], gmdt_b[, get(var_i)])
      
      return(res$statistic)
    }
    
    #apply over variables 
    sum_stats_li[[7]] <- data.table( "Fstat" = unlist(lapply(vars, ftest_fun)))
    
    
    
    #min
    sum_stats_li[[8]] <- gmdt[, list("Full Min" = lapply(.SD, min)), .SDcols = vars]
    sum_stats_li[[9]] <- gmdt_b[, list("Balanced Min" = lapply(.SD, min)), .SDcols = vars]
    #max 
    sum_stats_li[[10]] <- gmdt[, list("Full max" = lapply(.SD, max)), .SDcols = vars]
    sum_stats_li[[11]] <- gmdt_b[, list("Balanced max" = lapply(.SD, max)), .SDcols = vars]

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
  
  
#========================================#
# ==== Estimate Production Function ==== #
#========================================#

    # Step 1: Get measurement error from second-order polynomial 
    # (including interactions) of emp, dnpt, drst and investment
    setnames(gmdt, c("ldsal", "ldnpt", "ldrst"), c("lsales","lcap", "lrdcap"))

    gmdt[,dummy:=1]

    # I feel like "attaching" is bad right?
    attach(gmdt)    
    X <- cbind(dummy, lemp,lcap,lrdcap,ldinv, lemp*lemp,lemp*lcap, lemp*lrdcap,
    lemp*ldinv,lcap*lcap,lcap*lrdcap,lcap*ldinv,lrdcap*lrdcap,lrdcap*ldinv,ldinv*ldinv)
    
    # Get the coefficients of all these interactions
    beta1 <- solve(t(X)%*%X)%*%t(X)%*%lsales

    # Get the fitted values
    theta <- X%*%beta1

    # Then detach, see I think this is dumb but idk what the better thing to do would be...
    detach(gmdt)
    
    # Add the theta to the datatable
    gmdt[,theta:= theta]

    # get lags of emp, cap, rdcap and theta, remove observations without lag values
    cols = c("lemp","lcap","lrdcap", "theta")
    anscols = paste("lag", cols, sep="_")
    gmdt[, (anscols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
    gmdt <- gmdt[!is.na(lag_lemp),]
    
    # function to run GMM 
    # a lot of inputs here but this is how you get around using global objects 
    # THis is supposed to be better practice but it doe sget a bit wild with all these 
    gmm_obj_f <- function(parm_vector.in, Y.in, X.in, lX.in, ltheta.in, Z.in, W.in){
      
      beta <- as.matrix(parm_vector.in[1:4])
      rho <- parm_vector.in[5]
    
      current.resid <- Y.in - X.in%*%beta
      
      lag.w <- as.matrix(ltheta.in) - lX.in%*%beta
      
      m <-  current.resid - rho*lag.w
      
      m <- as.matrix(m)
      
      distance <- t(Z.in)%*%m
      
      result <- t(distance/length(m))%*% W.in %*%(distance/length(m))

      # get function value 
      return(result)
    }
    
    parm_vector <- c(1, .6, .2, .2, .8)
    
    # attach again, boo
    attach(gmdt)
    
    # Get X's and lag X's and Zs for GMM estimation
    X <- as.matrix(cbind(dummy, lemp, lcap, lrdcap))
    lX <- as.matrix(cbind(dummy, lag_lemp, lag_lcap, lag_lrdcap))
    Z <- as.matrix(cbind(dummy, lag_lemp, lcap, lrdcap))
    W <- diag(1, 4, 4)
    
    
    # test it out 
    f <- gmm_obj_f(parm_vector.in = parm_vector,
                   Y.in = lsales,
                   X.in = X,
                   lX.in = lX,
                   ltheta.in = lag_theta,
                   Z.in = Z, 
                   W.in = W)
    
    # Run the initial GMM using the identity matrix as the weighting matrix
    Results.step1 <-  optim(par         = parm_vector,
                      fn          =  gmm_obj_f,
                      Y.in = lsales,
                      X.in = X,
                      lX.in = lX,
                      ltheta.in = lag_theta,
                      Z.in = Z, 
                      W.in = W)
    
    
    # Function to calculate optimal weighting matrix
    find.optimal.W <- function(results.in, Y.in, X.in, lX.in, ltheta.in, Z.in){
        beta <- as.matrix(results.in$par[1:4])
        rho  <- results.in$par[5]
        
        current.resid <- Y.in - X.in%*%beta
        
        lag.w <- as.matrix(ltheta.in) - lX.in%*%beta
        
        m <-  current.resid - rho*lag.w
        
        m <- as.matrix(m)
        
        distance <- t(Z.in*cbind(m, m, m, m))%*%(Z.in*cbind(m, m, m, m))
        W <- distance/length(m)
        
        W.inv <- solve(W)
    return(W.inv)
    }

    # Get optimal weighting matrix
    W.opt <- find.optimal.W(results.in = Results.step1,
                            Y.in = lsales,
                            X.in = X,
                            lX.in = lX,
                            ltheta.in = lag_theta,
                            Z.in = Z)

    # Run again with optimal weighting matrix
    Results.final <-  optim(par         = parm_vector,
                      fn          =  gmm_obj_f,
                      Y.in = lsales,
                      X.in = X,
                      lX.in = lX,
                      ltheta.in = lag_theta,
                      Z.in = Z, 
                      W.in = W.opt)
    
# To Do:
# Put lines 140-261 in function, after cleaning up. Run once to get the coefficients, use the boot command to 
# run a bunch of times to get a bunch of coefficient estimates that we get the SEs from. 