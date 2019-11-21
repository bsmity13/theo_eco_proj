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
  annotate(geom = "text", x = 100, y = 480, label = expression(K[B] * "=" * 500))+
  annotate(geom = "label", x = 165, y=450, 
           label = "Base Scenario", 
           fontface = "italic", size = 3)

#Zoom
p1z <- plot_sim(sc1) +
  coord_cartesian(xlim = c(0, 200), ylim=c(0, 55))+
  annotate(geom = "label", x = 165, y=50, 
           label = "Base Scenario\n(zoomed)", 
           fontface = "italic", size = 3)

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
  annotate(geom = "text", x = 100, y = 470, label = expression(K[B] * "=" * 500)) +
  annotate(geom = "label", x = 165, y=475, 
           label = "No Habitat\nPreference", 
           fontface = "italic", size = 3)

#Zoom
p2z <- plot_sim(sc2) +
  coord_cartesian(ylim=c(0, 55))

#Weak preference
sc3 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = c(0.6, 0.4), alpha_H = base_alpha_H, 
                  K_H = base_K_H, base_kappa_H)
p3 <- plot_sim(sc3) +
  geom_hline(yintercept = 500, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 100, y = 470, label = expression(K[B] * "=" * 500)) +
  geom_hline(yintercept = 250, linetype = "dashed", color = "gray40", size = 1) +
  annotate(geom = "text", x = 170, y = 220, label = expression(K[A] * "=" * 250)) +
  annotate(geom = "label", x = 165, y=380, 
           label = "Weak Habitat\nPreference", 
           fontface = "italic", size = 3)

#Strong prop preference for B
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
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 50)) +
  annotate(geom = "label", x = 155, y=47, 
           label = "Strong Proportional\nHabitat Preference", 
           fontface = "italic", size = 3)

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
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 50))+
  annotate(geom = "label", x = 165, y=45, 
           label = "Strong Inverse\nHabitat Preference", 
           fontface = "italic", size = 3)

#Combine
row23 <- plot_grid(
  p2 + theme(legend.position = "none"), 
  p3 + 
    theme(legend.position = "none",
          axis.text.y = element_blank()) +
    ylab(" "),
  labels = c("a", "b"))
row45 <- plot_grid(
  p4z + theme(legend.position = "none"), 
  p5z + 
    theme(legend.position = "none",
          axis.text.y = element_blank()) +
    ylab(" "),
  labels = c("c", "d"))

final2345 <- plot_grid(
  row23, row45, leg,
  ncol = 1, rel_heights = c(1, 1, .1)
)

ggsave(plot = final2345, filename = "fig/sc2-5.tiff", width = 6.5, height = 5.5, units = "in",
       dpi = 200, compression = "lzw")

#Attack Rate Scenarios----
#Low Inverse Attack Difference
sc6 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = base_T_HH, alpha_H = c(0.02, 0.01), 
                  K_H = base_K_H, base_kappa_H)
#Zoom
p6 <- plot_sim(sc6) +
  annotate(geom = "label", x = 165, y=225, 
           label = "Low Inverse\nAttack Difference", 
           fontface = "italic", size = 3)

#High Proportional Attack Difference
sc7 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = base_T_HH, alpha_H = c(0.005, 0.025), 
                  K_H = base_K_H, kappa_H = base_kappa_H)
#Zoom
p7 <- plot_sim(sc7) +
  annotate(geom = "label", x = 165, y=225, 
           label = "High Proportional\nAttack Difference", 
           fontface = "italic", size = 3)

#High Inverse Attack Difference
sc8 <- sim_system(T = 200, N_Ht = base_N_Ht, rho_Ht = base_rho_Ht, 
                  r = base_r, epsilon = base_epsilon, phi_H = base_phi_H, 
                  T_HH = base_T_HH, alpha_H = c(0.025, 0.005), 
                  K_H = base_K_H, base_kappa_H)

#Zoom
p8 <- plot_sim(sc8) +
  annotate(geom = "label", x = 165, y=200, 
           label = "High Inverse\nAttack Difference", 
           fontface = "italic", size = 3)

#Combine
final678 <- plot_grid(
  p6 + 
    theme(legend.position = "none"), 
  p7 + 
    theme(legend.position = "none",
          axis.text.y = element_blank()) +
    ylab(" "),
  p8 + 
    theme(legend.position = "none"),
  leg,
  labels = c("a", "b", "c", NA))

ggsave(plot = final678, filename = "fig/sc6-8.tiff", width = 6.5, height = 5.5, units = "in",
       dpi = 200, compression = "lzw")
