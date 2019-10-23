#=================#
# ==== pset 2 ====
#=================#

library(data.table)
library(Matrix)

# clear objects
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

#=====================#
# ==== Question 1 ====
#=====================#

# set parameters 
alpha = 1 
beta  = 1
sigma = 1 
x1    = 1 
x2    = 2
x3    = 3
n.sim = 100

# create data.tabe of xs 
xi <- as.matrix(c(x1,x2,x3))

# make simulation matrix 
v = matrix(rnorm(1 * n.sim), nrow = 1, ncol = n.sim)


# Now we will guess the price to start and calcualte everything 
# fill in an initial price guess to work through functions 
# price in iteration k 
p_k <- matrix(c(2.17, 3.27, 4.84))

# will eventually write this intro an interative While loop 

  #==========================#
  # ==== Inside the loop ====
  #==========================#
  
    # using the guess, calualte deltas 
   delta  <- xi*beta - alpha*p_k
  
    # calculate x times sigma times v 
    mu <- xi%*%v*sigma
    
    # using the delta calculate shares 
    # below are hardcoded parameters for debug
    delta.in = delta
    mu.in = mu
  
  
    share_f <- function(delta.in, mu.in){
     
      # get the numerator by exp(delta + xi*vi*sigma)
      numer <- exp(mu.in) * matrix(rep(exp(delta.in), n.sim), ncol = n.sim)
      
      # get the denominator by summing over all numerators and adding one 
      denom_i <- matrix(rep(1 + colSums(numer),3), nrow = 1, ncol = n.sim)
      
      # then replicated this three times so we can divide (probs better way to do this )
      denom <- rbind(denom_i, denom_i, denom_i)

      # the shares are the mean of numerator/denominator accross simulations 
      shares <- rowMeans(numer / denom)

      return(shares)
    }
    
    shares <- share_f(delta, mu)
      
    # using shares get derivative of shares s_j wrt delta_i 
    # write a function to do this for a given i that we can then apply accross the 3 i's 
    ds_ddi_f <- function(delta.in, mu.in, i, phi = .01){
      
      # alter delta_i plus phi  
      delta_plus <- delta.in
      delta_plus[i, ] <-  delta.in[i, ] + phi
      share_plus <- share_f(delta_plus, mu.in)
      
      # alter delta_i minus phi 
      delta_minus <- delta.in
      delta_minus[i, ] <-  delta.in[i, ] - phi
      share_minus <- share_f(delta_minus, mu.in)
      
      # take average to get estimated derivative 
      ds_ddi <- (share_plus - share_minus)/(2*phi)
      
      return(ds_ddi)
    }
    
    # now apply that for all three deltas and stak it into a matrix 
    ds_dd <- do.call("cbind", lapply(1:3, ds_ddi_f, delta.in = delta, mu.in = mu, phi = .01))
    
    # using that derivative et derivative of shares wrt price 
    
    # using the shares and derivative, calculate the equilibrium price 
    
    # check how close we are to the original 

