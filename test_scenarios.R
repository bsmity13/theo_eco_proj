###############################################X
#-----Theoretical Ecology Research Project-----X
#----------------Brian J. Smith----------------X
#------------------Fall 2019-------------------X
#==============================================X
#----------------Test Scenarios----------------X
#==============================================X
#------------Last update 2019-11-21------------X
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

#No predator test----
#Base scenario, no predators, no movement
basic_nopred <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = c(0, 0), 
                           r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                           T_HH = base_T_HH, alpha_H = base_alpha_H, 
                           K_H = base_K_H, base_kappa_H)
supp1a <- plot_sim(basic_nopred) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 480, label = expression(K[B] * "=" * 500)) +
  geom_hline(yintercept = 250, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 230, label = expression(K[A] * "=" * 250)) +
  coord_cartesian(ylim = c(0, 500)) +
  annotate(geom = "label", x = 100, y=100, 
           label = "No Predators\nNo Movement", 
           fontface = "italic", size = 3)



#Base scenario, no predators, movement
basic_nopred_move <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = c(0, 0), 
                           r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                           T_HH = c(0.25, 0.25), alpha_H = base_alpha_H, 
                           K_H = base_K_H, base_kappa_H)

supp1b <- plot_sim(basic_nopred_move) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 480, label = expression(K[B] * "=" * 500)) +
  geom_hline(yintercept = 250, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 230, label = expression(K[A] * "=" * 250)) +
  coord_cartesian(ylim = c(0, 500)) +
  annotate(geom = "label", x = 100, y=100, 
           label = "No Predators\nEqual Movement", 
           fontface = "italic", size = 3)

#Legend
leg <- get_legend(
  supp1a + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

#Combine
row1 <- plot_grid(
  supp1a + theme(legend.position = "none"), 
  supp1b + 
    theme(legend.position = "none") +
    ylab(" "),
  labels = "auto")

supp1 <- plot_grid(
  row1, leg,
  ncol = 1, rel_heights = c(1, .1)
)

ggsave(plot = supp1, filename = "fig/supp1.tiff", width = 6.5, height = 3.5, units = "in",
       dpi = 200, compression = "lzw")

#Stable predator attack rate test----
attack_nomove <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                            r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                            T_HH = base_T_HH, alpha_H = c(0.01, 0.01), 
                            K_H = base_K_H, base_kappa_H)
supp2a <- plot_sim(attack_nomove) +
  coord_cartesian(ylim=c(0, 100)) +
  annotate(geom = "label", x = 150, y=90, 
           label = "Equal Stable Attack Rate\nNo Movement", 
           fontface = "italic", size = 3)

attack_move <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                          r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                          T_HH = c(0.25, 0.25), alpha_H = c(0.01, 0.01), 
                          K_H = base_K_H, base_kappa_H)
supp2b <- plot_sim(attack_move) +
  coord_cartesian(ylim=c(0, 100)) +
  annotate(geom = "label", x = 150, y=90, 
           label = "Equal Stable Attack Rate\nEqual Movement", 
           fontface = "italic", size = 3)

#Combine
row2 <- plot_grid(
  supp2a + theme(legend.position = "none"), 
  supp2b + 
    theme(legend.position = "none") +
    ylab(" "),
  labels = "auto")

supp2 <- plot_grid(
  row2, leg,
  ncol = 1, rel_heights = c(1, .1)
)

ggsave(plot = supp2, filename = "fig/supp2.tiff", width = 6.5, height = 3.5, units = "in",
       dpi = 200, compression = "lzw")
