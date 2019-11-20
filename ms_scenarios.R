###############################################X
#-----Theoretical Ecology Research Project-----X
#----------------Brian J. Smith----------------X
#------------------Fall 2019-------------------X
#==============================================X
#-------------Manuscript Scenarios-------------X
#==============================================X
#------------Last update 2019-11-20------------X
###############################################X

#Set options----
options(stringsAsFactors = FALSE)

#Load functions----
source("proj_fun.R")

#Load packages----
library(dplyr)
library(tidyr)
library(ggplot2)

#Base Scenario----
basic_pred <- sim_system(T = 750, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
           r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
           T_HH = c(0.25, 0.25), alpha_H = c(0.03, 0.03), 
           K_H = c(200, 200), kappa_H = c(25, 25))

plot_sim(basic_pred) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  ggtitle("Base Scenario, with Predators")
  
#Zoom
plot_sim(basic_pred) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  coord_cartesian(ylim=c(0, 80)) +
  ggtitle("Base Scenario, with Predators (zoomed)")

