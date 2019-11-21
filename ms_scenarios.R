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
library(cowplot)

#Base Scenario----
sc1 <- sim_system(T = 600, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
           r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
           T_HH = c(0.25, 0.25), alpha_H = c(0.03, 0.03), 
           K_H = c(50, 200), kappa_H = c(25, 25))

p1 <- plot_sim(sc1) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[B] * "=" * 200)) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 40, label = expression(K[A] * "=" * 50))
  
#Zoom
p1z <- plot_sim(sc1) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 48, label = expression(K[A] * "=" * 50)) +
  coord_cartesian(ylim=c(0, 55))

#Legend
leg <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

#Combine
row1 <- plot_grid(
  p1 + theme(legend.position = "none"), 
  p1z + 
    theme(legend.position = "none") +
    ylab(NULL),
  labels = "auto")

final1 <- plot_grid(
  row1, leg,
  ncol = 1, rel_heights = c(1, .1)
)

#Habitat Selection Scenarios----
#Low preference
sc2 <- sim_system(T = 600, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                  r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                  T_HH = c(0.2, 0.3), alpha_H = c(0.03, 0.03), 
                  K_H = c(50, 200), kappa_H = c(25, 25))
p2 <- plot_sim(sc2) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[B] * "=" * 200)) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 40, label = expression(K[A] * "=" * 50))

#Zoom
p2z <- plot_sim(sc2) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 48, label = expression(K[A] * "=" * 50)) +
  coord_cartesian(ylim=c(0, 55))

#High preference
sc3 <- sim_system(T = 600, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                  r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                  T_HH = c(0.1, 0.5), alpha_H = c(0.03, 0.03), 
                  K_H = c(50, 200), kappa_H = c(25, 25))
p3 <- plot_sim(sc3) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 190, label = expression(K[B] * "=" * 200)) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 40, label = expression(K[A] * "=" * 50))

#Zoom
p3z <- plot_sim(sc3) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 48, label = expression(K[A] * "=" * 50)) +
  coord_cartesian(ylim=c(0, 55))

#Combine
row23 <- plot_grid(
  p2z + theme(legend.position = "none"), 
  p3z + 
    theme(legend.position = "none") +
    ylab(NULL),
  labels = "auto")

final23 <- plot_grid(
  row23, leg,
  ncol = 1, rel_heights = c(1, .1)
)

#Carrying Capacity Scenarios----
#Proportional K
sc4 <- sim_system(T = 600, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                  r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                  T_HH = c(0.25, 0.25), alpha_H = c(0.03, 0.03), 
                  K_H = c(50, 200), kappa_H = c(25, 100))
#Zoom
p4z <- plot_sim(sc4) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 48, label = expression(K[A] * "=" * 50)) +
  coord_cartesian(ylim=c(0, 70))

#Inverse K
sc5 <- sim_system(T = 600, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                  r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                  T_HH = c(0.25, 0.25), alpha_H = c(0.03, 0.03), 
                  K_H = c(50, 200), kappa_H = c(100, 25))
#Zoom
p5z <- plot_sim(sc5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 48, label = expression(K[A] * "=" * 50)) +
  coord_cartesian(ylim=c(0, 70))

#Combine
row45 <- plot_grid(
  p4z + theme(legend.position = "none"), 
  p5z + 
    theme(legend.position = "none") +
    ylab(NULL),
  labels = "auto")

final45 <- plot_grid(
  row45, leg,
  ncol = 1, rel_heights = c(1, .1)
)

#Attack Rate Scenarios----
#Low Attack Difference
sc6 <- sim_system(T = 600, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                  r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                  T_HH = c(0.25, 0.25), alpha_H = c(0.025, 0.035), 
                  K_H = c(50, 200), kappa_H = c(25, 25))
#Zoom
p6z <- plot_sim(sc6) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 48, label = expression(K[A] * "=" * 50)) +
  coord_cartesian(ylim=c(0, 70))

#High Attack Difference
sc7 <- sim_system(T = 600, N_Ht = c(100, 100), rho_Ht = c(10, 10), 
                  r = 0.1, epsilon = c(0.1, 0.1), phi_H = c(0.95, 0.95), 
                  T_HH = c(0.25, 0.25), alpha_H = c(0.01, 0.05), 
                  K_H = c(50, 200), kappa_H = c(25, 25))
#Zoom
p7z <- plot_sim(sc7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 500, y = 48, label = expression(K[A] * "=" * 50)) +
  coord_cartesian(ylim=c(0, 70))

#Combine
row67 <- plot_grid(
  p6z + theme(legend.position = "none"), 
  p7z + 
    theme(legend.position = "none") +
    ylab(NULL),
  labels = "auto")

final67 <- plot_grid(
  row67, leg,
  ncol = 1, rel_heights = c(1, .1)
)
