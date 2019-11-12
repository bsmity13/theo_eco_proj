###############################################X
#-----Theoretical Ecology Research Project-----X
#----------------Brian J. Smith----------------X
#------------------Fall 2019-------------------X
#==============================================X
#------------------Scenarios-------------------X
#==============================================X
#------------Last update 2019-11-06------------X
###############################################X

#Set options----
options(stringsAsFactors = FALSE)

#Load functions----
source("proj_fun.R")

#Load packages----
library(dplyr)
library(tidyr)
library(ggplot2)

#Base Scenario (both habitats equal)----
basic <- sim_system(T = 700, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
           r = 0.1, e = 0.75, phi_H = c(0.95, 0.95), 
           psi_HH = c(0.1, 0.1), alpha_H = c(0.015, 0.015), 
           K_H = c(200, 200), kappa_H = c(25, 25))

plot_sim(basic)

#No predators----
no_pred <- sim_system(T = 250, N_Ht = c(100, 100), rho_Ht = c(0, 0), 
                      r = 0.1, e = 0.5, phi_H = c(0.95, 0.95), 
                      psi_HH = c(0.1, 0.1), alpha_H = c(0.02, 0.02), 
                      K_H = c(200, 200), kappa_H = c(20, 20))
plot_sim(no_pred)

#No predators, different K----
no_pred2 <- sim_system(T = 250, N_Ht = c(100, 100), rho_Ht = c(0, 0), 
                      r = 0.1, e = 0.5, phi_H = c(0.95, 0.95), 
                      psi_HH = c(0.1, 0.1), alpha_H = c(0.02, 0.02), 
                      K_H = c(100, 200), kappa_H = c(20, 20))
plot_sim(no_pred2)
