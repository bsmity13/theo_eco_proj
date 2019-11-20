###############################################X
#-----Theoretical Ecology Research Project-----X
#----------------Brian J. Smith----------------X
#------------------Fall 2019-------------------X
#==============================================X
#------------------Scenarios-------------------X
#==============================================X
#------------Last update 2019-11-13------------X
###############################################X

#Set options----
options(stringsAsFactors = FALSE)

#Load functions----
source("proj_fun.R")

#Load packages----
library(dplyr)
library(tidyr)
library(ggplot2)

#Base scenario, no predators----
basic_nopred <- sim_system(T = 200, N_Ht = c(100, 100), rho_Ht = c(0, 0), 
                      r = 0.1, epsilon = c(0, 0), phi_H = c(0, 0), 
                      T_HH = c(0.25, 0.25), alpha_H = c(0, 0), 
                      K_H = c(200, 200), kappa_H = c(20, 20))
plot_sim(basic_nopred) +
  annotate(geom = "text", x = 100, y = 191, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  ggtitle("Base Scenario, No Predators")

#Base Scenario, with predators----
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

#Different K, no predators----
preyK_nopred <- sim_system(T = 250, N_Ht = c(100, 100), rho_Ht = c(0, 0), 
                      r = 0.1, epsilon = c(0, 0), phi_H = c(0, 0), 
                      T_HH = c(0.25, 0.25), alpha_H = c(0, 0), 
                      K_H = c(50, 200), kappa_H = c(20, 20))

plot_sim(preyK_nopred) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 250/2, y = 40, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 250/2, y = 190, label = expression(K[B] * " = " * 200)) +
  ggtitle("Movement between Habitats")

#Different K, no predators, no movement between habitats----
preyK_nopred_nohab <- sim_system(T = 200, N_Ht = c(100, 100), rho_Ht = c(0, 0), 
                                 r = 0.1, epsilon = c(0, 0), phi_H = c(0, 0), 
                                 T_HH = c(0, 0), alpha_H = c(0, 0), 
                                 K_H = c(50, 200), kappa_H = c(20, 20))

plot_sim(preyK_nopred_nohab) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 40, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 100, y = 190, label = expression(K[B] * " = " * 200)) +
  ggtitle("No Movement Between Habitats")

#Different K, no predators, habitat selection prop to K----
#This seems like the ideal free distribution
preyK_nopred_prophab <- sim_system(T = 200, N_Ht = c(100, 100), rho_Ht = c(0, 0), 
                                 r = 0.1, epsilon = c(0, 0), phi_H = c(0, 0), 
                                 T_HH = c(0.4, 0.1), alpha_H = c(0, 0), 
                                 K_H = c(50, 200), kappa_H = c(20, 20))

plot_sim(preyK_nopred_prophab) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 40, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 100, y = 190, label = expression(K[B] * " = " * 200)) +
  ggtitle("Habitat Selection Proportional to K")


#Different K, with predators----
preyK_pred <- sim_system(T = 400, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                           r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                           T_HH = c(0.25, 0.25), alpha_H = c(0.03, 0.03), 
                           K_H = c(50, 200), kappa_H = c(20, 20))
plot_sim(preyK_pred) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 200, y = 40, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 200, y = 190, label = expression(K[B] * " = " * 200))
#Zoom
plot_sim(preyK_pred) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 200, y = 48, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 200, y = 190, label = expression(K[B] * " = " * 200)) +
  coord_cartesian(ylim=c(0, 55)) +
  ggtitle("Equal Movement between Habitats")

#Different K, with predators, no movement between habitats----
preyK_pred_nohab <- sim_system(T = 500, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                         r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                         T_HH = c(0, 0), alpha_H = c(0.03, 0.03), 
                         K_H = c(50, 200), kappa_H = c(20, 20))
plot_sim(preyK_pred_nohab) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 200, y = 40, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 200, y = 190, label = expression(K[B] * " = " * 200))
#Zoom
plot_sim(preyK_pred_nohab) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 200, y = 48, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 200, y = 190, label = expression(K[B] * " = " * 200)) +
  coord_cartesian(ylim=c(0, 70)) +
  ggtitle("No Movement Between Habitats")

#Different K, with predators, proportional habitat selection----
preyK_pred_prophab <- sim_system(T = 500, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                               r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                               T_HH = c(0.4, 0.1), alpha_H = c(0.02, 0.02), 
                               K_H = c(50, 200), kappa_H = c(20, 20))
plot_sim(preyK_pred_prophab) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 250, y = 40, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 250, y = 190, label = expression(K[B] * " = " * 200))
#Zoom
plot_sim(preyK_pred_prophab) +
  geom_hline(yintercept = c(50, 200), linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 250, y = 48, label = expression(K[A] * " = " * 50)) +
  annotate(geom = "text", x = 250, y = 190, label = expression(K[B] * " = " * 200)) +
  coord_cartesian(ylim=c(0, 60)) +
  ggtitle("Habitat Selection Proportional to K")

#Different attack rates, high movement----
attack_himove <- sim_system(T = 750, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                          r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                          T_HH = c(0.25, 0.25), alpha_H = c(0.03, 0.06), 
                          K_H = c(200, 200), kappa_H = c(25, 25))

plot_sim(attack_himove) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  ggtitle("Different Attack Rates, with High Movement")

#Zoom
plot_sim(attack_himove) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  coord_cartesian(ylim=c(0, 80)) +
  ggtitle("Different Attack Rates, with High Movement (zoomed)")

#Different attack rates, low movement----
attack_lomove <- sim_system(T = 750, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                          r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                          T_HH = c(0.05, 0.05), alpha_H = c(0.03, 0.06), 
                          K_H = c(200, 200), kappa_H = c(25, 25))

plot_sim(attack_lomove) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  ggtitle("Different Attack Rates, with Low Movement")

#Zoom
plot_sim(attack_lomove) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  coord_cartesian(ylim=c(0, 80)) +
  ggtitle("Different Attack Rates, with Low Movement (zoomed)")

#Different attack rates, no movement----
attack_nomove <- sim_system(T = 750, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                          r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                          T_HH = c(0, 0), alpha_H = c(0.03, 0.06), 
                          K_H = c(200, 200), kappa_H = c(25, 25))

plot_sim(attack_nomove) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  ggtitle("Different Attack Rates, without Movement")

#Zoom
plot_sim(attack_nomove) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[A] * "=" * K[B] * "=" * 200)) +
  coord_cartesian(ylim=c(0, 80)) +
  ggtitle("Different Attack Rates, without Movement (zoomed)")
