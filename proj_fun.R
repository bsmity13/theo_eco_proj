###############################################X
#-----Theoretical Ecology Research Project-----X
#----------------Brian J. Smith----------------X
#------------------Fall 2019-------------------X
#==============================================X
#------------------Functions-------------------X
#==============================================X
#------------Last update 2019-11-06------------X
###############################################X


# #Test params----
# #N_Ht -- number of prey in habitat H at time t (vector of 2)
# N_Ht <- c(100, 100)
# #psi_HH -- transition probability from H to H', i.e., from A to B
# #and from B to A (vector of 2)
# psi_HH <- c(0.5, 0.5)
# #alpha_H -- attack rate of the predator in habitat H (vector of 2)
# alpha_H <- c(0.01, 0.01)
# #rho_Ht -- number of predators in habitat H at time t (vector of 2)
# rho_Ht <- c(10, 10)
# #r -- maximum growth rate of prey (vector of 1)
# r <- 0.1
# #K_H -- carrying capacity of prey in habitat H (vector of 2)
# K_H <- c(200, 200)
# #phi_H -- survival of predators in habitat H (vector of 2)
# phi_H <- c(0.9, 0.9)
# #epsilon -- conversion efficiency of predator in habitat H (vector of 2)
# epsilon <- c(0.1, 0.1)
# #kappa_H -- carrying capacity of predators in habitat H (vector of 2)
# kappa_H <- c(20, 20)

#Prey growth----
N_Htp1 <- function(N_Ht, psi_HH, alpha_H, rho_Ht, r, K_H){
  #Breaking the equation into chunks for clarity
  #Step 1. Transition between habitats
  s1A <- N_Ht[1] * (1 - psi_HH[1]) + N_Ht[2] * psi_HH[2] 
  s1B <- N_Ht[2] * (1 - psi_HH[2]) + N_Ht[1] * psi_HH[1] 
  #Step 2. Predation
  s2A <- -1 * (alpha_H[1] * rho_Ht[1] * N_Ht[1])
  s2B <- -1 * (alpha_H[2] * rho_Ht[2] * N_Ht[2])
  #Step 3. Growth
  s3A <- r * N_Ht[1] * (1 - (N_Ht[1]/K_H[1]))
  s3B <- r * N_Ht[2] * (1 - (N_Ht[2]/K_H[2]))
  #Combine
  N_tp1A <- s1A + s2A + s3A
  N_tp1B <- s1B + s2B + s3B
  #Return
  return(c(N_tp1A, N_tp1B))
}

#N_Htp1(N_Ht, psi_HH, alpha_H, rho_Ht, r, K_H)

#Predator growth----
rho_Htp1 <- function(phi_H, rho_Ht, epsilon, alpha_H, N_Ht, kappa_H){
  #Breaking into chunks for clarity
  #Step 1. Survival
  s1A <- phi_H[1] * rho_Ht[1]
  s1B <- phi_H[2] * rho_Ht[2]
  #Step 2. Growth due to predation
  s2A <- epsilon[1] * alpha_H[1] * rho_Ht[1] * N_Ht[1] * (1 - rho_Ht[1]/kappa_H[1])
  s2B <- epsilon[2] * alpha_H[2] * rho_Ht[2] * N_Ht[2] * (1 - rho_Ht[2]/kappa_H[2])
  #Combine
  rho_Htp1A <- s1A + s2A
  rho_Htp1B <- s1B + s2B
  #Return
  return(c(rho_Htp1A, rho_Htp1B))
}

#rho_Htp1(phi_H, rho_Ht, epsilon, alpha_H, N_Ht, kappa_H)

#Simulate the system----
sim_system <- function(T = 100, N_Ht, rho_Ht, r, epsilon, phi_H, psi_HH, alpha_H, K_H, kappa_H){
  #data.frame to hold results
  Res <- data.frame(t = 1, N_A = N_Ht[1], N_B = N_Ht[2], rho_A = rho_Ht[1], rho_B = rho_Ht[2])
  #Loop through times
  for(t in 2:T){
    #Calculate next prey population
    N_H_next <- N_Htp1(N_Ht = Res[(t-1), c("N_A", "N_B")], psi_HH, alpha_H, rho_Ht = Res[(t-1), c("rho_A", "rho_B")], r, K_H)
    #Calculate next predator population
    rho_H_next <- rho_Htp1(phi_H, rho_Ht = Res[(t-1), c("rho_A", "rho_B")], epsilon, alpha_H, N_Ht = Res[(t-1), c("N_A", "N_B")], kappa_H)
    #Create results row
    res <- data.frame(t = t, N_A = N_H_next[1], N_B = N_H_next[2], rho_A = rho_H_next[1], rho_B = rho_H_next[2])
    #Combine results
    Res <- dplyr::bind_rows(Res, res)
  }
  #Return
  return(Res)
}

#basic <- sim_system(T = 100, N_Ht, rho_Ht, r, e, phi_H, psi_HH, alpha_H, K_H, kappa_H)

#Plot the simulation----

plot_sim <- function(sim){
  #Tidy data
  tidyd <- pivot_longer(sim, -t, names_to = "pop", values_to = "size")
  #ggplot
  ggp <- ggplot(data = tidyd, aes(x = t, y = size, color = pop, linetype = pop)) +
    geom_line(size = 2) +
    theme_bw() +
    xlab("Time (t)") +
    ylab("Population Sizes") +
    scale_color_brewer(type = "qual", palette = 2,
      breaks = c("N_A", "N_B", "rho_A", "rho_B"),
      labels = expression(N[A], N[B], rho[A], rho[B]),
      name = "Population") +
    scale_linetype_manual(
      values = c("solid", "3131", "solid", "3131"),
      breaks = c("N_A", "N_B", "rho_A", "rho_B"),
      labels = expression(N[A], N[B], rho[A], rho[B]),
      name = "Population")
  #Return
  return(ggp)
}

#plot_sim(basic)
