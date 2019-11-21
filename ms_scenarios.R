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

#Base Parameters----
base_N_Ht = c(250, 250)
base_rho_Ht = c(25, 25)
base_r = 0.1
base_epsilon = c(0.4, 0.4)
base_phi_H = c(0.9, 0.9)
base_T_HH = c(0, 0)
base_alpha_H = c(0.01, 0.02)
base_K_H = c(250, 500)
base_kappa_H = c(25, 50)

#Base Scenario----
sc1 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = base_T_HH, alpha_H = base_alpha_H, 
                  K_H = base_K_H, base_kappa_H)

p1 <- plot_sim(sc1) +
  geom_hline(yintercept = 250, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 230, label = expression(K[A] * "=" * 250)) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 480, label = expression(K[B] * "=" * 500))

#Zoom
p1z <- plot_sim(sc1) +
  coord_cartesian(xlim = c(0, 200), ylim=c(0, 55))

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

ggsave(plot = final1, filename = "fig/sc1.tiff", width = 6.5, height = 3.5, units = "in",
       dpi = 200, compression = "lzw")

#Habitat Selection Scenarios----
#No preference
sc2 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = c(0.5, 0.5), alpha_H = base_alpha_H, 
                  K_H = base_K_H, base_kappa_H)
p2 <- plot_sim(sc2) +
  geom_hline(yintercept = 250, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 220, label = expression(K[A] * "=" * 250)) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 470, label = expression(K[B] * "=" * 500))

#Zoom
p2z <- plot_sim(sc2) +
  coord_cartesian(ylim=c(0, 55))

#Low preference
sc3 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = c(0.6, 0.4), alpha_H = base_alpha_H, 
                  K_H = base_K_H, base_kappa_H)
p3 <- plot_sim(sc3) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 470, label = expression(K[B] * "=" * 500)) +
  geom_hline(yintercept = 250, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 170, y = 220, label = expression(K[A] * "=" * 250))

#High preference for B
sc4 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = c(0.1, 0.9), alpha_H = base_alpha_H, 
                  K_H = base_K_H, base_kappa_H)
p4 <- plot_sim(sc4) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 480, label = expression(K[B] * "=" * 500)) +
  geom_hline(yintercept = 250, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 170, y = 230, label = expression(K[A] * "=" * 250))

p4z <- plot_sim(sc4) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 50))

#High preference for A
sc5 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = c(0.9, 0.1), alpha_H = base_alpha_H, 
                  K_H = base_K_H, base_kappa_H)

p5 <- plot_sim(sc5) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 480, label = expression(K[B] * "=" * 500)) +
  geom_hline(yintercept = 250, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 230, label = expression(K[A] * "=" * 250))

p5z <- plot_sim(sc5) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 50))

#Combine
row23 <- plot_grid(
  p2 + theme(legend.position = "none"), 
  p3 + 
    theme(legend.position = "none",
          axis.text.y = element_blank()) +
    ylab(NULL),
  labels = c("a", "b"),
  label_x = c(0, -0.04))
row45 <- plot_grid(
  p4z + theme(legend.position = "none"), 
  p5z + 
    theme(legend.position = "none",
          axis.text.y = element_blank()) +
    ylab(NULL),
  labels = c("c", "d"),
  label_x = c(0, -0.04))

final2345 <- plot_grid(
  row23, row45, leg,
  ncol = 1, rel_heights = c(1, 1, .1)
)

ggsave(plot = final2345, filename = "fig/sc2-5.tiff", width = 6.5, height = 5.5, units = "in",
       dpi = 200, compression = "lzw")

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
