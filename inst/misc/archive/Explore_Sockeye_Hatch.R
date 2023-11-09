#----------------------------------------------
# Test hatchery addition
#----------------------------------------------

library(CEMPRA)

filename_lc <-
  system.file("extdata/species_profiles",
              "life_cycles_socklow.csv",
              package = "CEMPRA")
life_cycles <- read.csv(filename_lc)

# Setup parameters for BH-plot
stage_1_capacity <- 2500000
surv_fry <- life_cycles$Value[life_cycles$Name == "S0"]

# Set adult capacity to inf
life_cycles$Value[life_cycles$Name == "k"] <- 100000000000

# print(life_cycles)
# life_cycles$Value[life_cycles$Name == "SE"] <- 0.9
# life_cycles$Value[life_cycles$Name == "surv_2"] <- 0.06

# Setup objects for population model
pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)

# --------------------------------------
# Save matrix elements
pop_mod_setup$projection_matrix
pop_mod_setup$life_histories$S
pop_mod_setup$life_histories$Surv_annual

sym_mat <- matrix(as.character(pop_mod_setup$life_stages_symbolic), nrow = life_cycles$Value[life_cycles$Name == "Nstage"], ncol = life_cycles$Value[life_cycles$Name == "Nstage"])



# ------------------------------------------
# Build matrix elements for population model
pop_mod_mat <-
  pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
# print(pop_mod_mat$projection_matrix)

names(pop_mod_mat)
pop_mod_mat$projection_matrix
pop_mod_mat$life_histories$s0.1.det
pop_mod_mat$life_histories$S
pop_mod_mat$life_histories$Surv_annual





# Preview density-independent transition projection_matrix
A <- pop_mod_mat$projection_matrix
# Assign nicknames for each stage
n_stage <- life_cycles$Value[life_cycles$Name == "Nstage"]
snames <- paste0("stage_", 1:n_stage)

rownames(A) <- colnames(A) <- snames
# Simple density-independent lambda estimate
(lambda <- popbio::lambda(A))

# Run full eigen analysis
popbio::eigen.analysis(A)$damping.ratio
popbio::eigen.analysis(A)$lambda1
popbio::eigen.analysis(A)$stable.stage
popbio::generation.time(A)


# Set the K.adj (K adjustment prior to pop model run)
life_histories <- pop_mod_mat$life_histories
# Mathematical expression of the transition matrix
life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
# Mathematical expression of the density matrix
density_stage_symbolic <- pop_mod_mat$density_stage_symbolic









#=========================================================
# Run simple population projection - project forward through time
M.mx = life_stages_symbolic
# projection matrix expression
D.mx = density_stage_symbolic
# density-dependence matrix
H.mx = NULL
dat = life_histories
# life history data
K = life_histories$Ka
# initial pop size as stage-structure vector
Nyears = 500
# years to run simulation
p.cat = 0
# Probability of catastrophe
CE_df = NULL
stage_k_override = c(50000000, 2500000, NA, NA, NA, NA)
bh_dd_stages = c("dd_hs_0", "bh_stage_1")
K_adj = FALSE
hatchery_addition_k2 = 15000

#=========================================================
# Run simple population projection - project forward through time
baseline <-
  Projection_DD_Hatchery(
    M.mx = life_stages_symbolic,
    # projection matrix expression
    D.mx = density_stage_symbolic,
    # density-dependence matrix
    H.mx = NULL,
    dat = life_histories,
    # life history data
    hatchery_addition_k2 = 0,
    K = life_histories$Ka,
    # initial pop size as stage-structure vector
    Nyears = 500,
    # years to run simulation
    p.cat = 0,
    # Probability of catastrophe
    CE_df = NULL,
    stage_k_override = stage_k_override,
    bh_dd_stages = bh_dd_stages
  )

mcols <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3')

# Time series of the population with CE
dplot <- baseline$pop
dplot <- dplot[dplot$year > 100, ]
dplot$N <- ifelse(dplot$N < 3, 0, dplot$N)
plot(dplot$N, type = 'l', xlim = c(100, 200), ylim = c(0, 2000), col = mcols[1], lwd = 2,
     xlab = "Simulation Year", ylab = "(N) Anadromous Spawners")



hatch <- c(5000, 10000, 20000)

for(h in 1:length(hatch)) {
  baseline <-
    Projection_DD_Hatchery(
      M.mx = life_stages_symbolic,
      # projection matrix expression
      D.mx = density_stage_symbolic,
      # density-dependence matrix
      H.mx = NULL,
      dat = life_histories,
      # life history data
      hatchery_addition_k2 = hatch[h],
      K = life_histories$Ka,
      # initial pop size as stage-structure vector
      Nyears = 500,
      # years to run simulation
      p.cat = 0,
      # Probability of catastrophe
      CE_df = NULL,
      stage_k_override = stage_k_override,
      bh_dd_stages = bh_dd_stages
    )

  # Time series of the population with CE
  dplot <- baseline$pop
  dplot <- dplot[dplot$year > 100, ]
  dplot$N <- ifelse(dplot$N < 3, 0, dplot$N)
  points(dplot$N, lwd = 2, type = 'l', lty = h, col = mcols[h + 1],)


}











################################
life_cycles_mod <- life_cycles
life_cycles_mod$Value

pseq <- seq(0, 1, by = 0.05)
hatch <- c(5000, 10000, 20000)

all_out <- list()
counter <- 1

for(h in 1:length(hatch)) {

  this_hatch <- hatch[h]

for(p in 1:length(pseq)) {

  this_prob <- pseq[p]
  print(this_prob)

  life_cycles_mod <- life_cycles

  life_cycles_mod$Value[which(life_cycles_mod$Name == "mat_3")] <- life_cycles_mod$Value[which(life_cycles_mod$Name == "mat_3")] * this_prob

  life_cycles_mod$Value[which(life_cycles_mod$Name == "mat_4")] <- life_cycles_mod$Value[which(life_cycles_mod$Name == "mat_4")] * this_prob

  life_cycles_mod$Value[which(life_cycles_mod$Name == "mat_5")] <- life_cycles_mod$Value[which(life_cycles_mod$Name == "mat_5")] * this_prob


# Setup objects for population model
pop_mod_setup <- pop_model_setup(life_cycles = life_cycles_mod)
pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
life_histories <- pop_mod_mat$life_histories
life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
density_stage_symbolic <- pop_mod_mat$density_stage_symbolic

#=========================================================
# Run simple population projection - project forward through time
baseline <-
  Projection_DD_Hatchery(
    M.mx = life_stages_symbolic,
    # projection matrix expression
    D.mx = density_stage_symbolic,
    # density-dependence matrix
    H.mx = NULL,
    dat = life_histories,
    # life history data
    hatchery_addition_k2 = this_hatch,
    K = life_histories$Ka,
    # initial pop size as stage-structure vector
    Nyears = 200,
    # years to run simulation
    p.cat = 0,
    # Probability of catastrophe
    CE_df = NULL,
    stage_k_override = stage_k_override,
    bh_dd_stages = bh_dd_stages
  )


#mcols <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3')

# Time series of the population with CE
dplot <- baseline$pop
dplot <- dplot[dplot$year > 100, ]
dplot$N <- ifelse(dplot$N < 3, 0, dplot$N)

geo_mean <- exp(mean(log(dplot$N)))

add_row <- data.frame(passage = this_prob, N = geo_mean, hatch = this_hatch)

dplot$passage <- this_prob
dplot$hatch <- this_hatch

all_out[[counter]] <- dplot
counter <- counter + 1

}
  print("HATCH")
}

mout <- do.call("rbind", all_out)
plot(mout$passage, mout$N)

head(mout, 3)
mout$hatch <- as.character(mout$hatch)

library(ggplot2)
ggplot(mout, aes(x = passage, y = N, colour = hatch, group = hatch)) + geom_smooth() +
  geom_point() +
  xlab("Upstream Passage Efficiency (%)") +
  ylab("Adult System Capacity (N)") +
  ylim(0, 1500) +
  theme_bw() + theme(legend.position = "bottom")




