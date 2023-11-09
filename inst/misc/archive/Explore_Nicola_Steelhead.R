#----------------------------------------------
# Load test profile for Nicola Steelhead
#----------------------------------------------

library(CEMPRA)

filename_lc <-
  system.file("extdata/species_profiles",
              "life_cycles_steelhead_nicola.csv",
              package = "CEMPRA")
life_cycles <- read.csv(filename_lc)

# Setup parameters for BH-plot
stage_1_capacity <- 160000
surv_fry <- life_cycles$Value[life_cycles$Name == "S0"]

# Set adult capacity to inf
life_cycles$Value[life_cycles$Name == "k"] <- 10000000

# print(life_cycles)
# life_cycles$Value[life_cycles$Name == "SE"] <- 0.9
# life_cycles$Value[life_cycles$Name == "surv_2"] <- 0.06

# Setup objects for population model
pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)

# --------------------------------------
# Save matrix elements
pop_mod_setup$projection_matrix
write.csv(pop_mod_setup$projection_matrix, row.names = FALSE, file = "./inst/misc/nicola/steelhead/steelhead_projection_matrix.csv")
pop_mod_setup$life_histories$S
pop_mod_setup$life_histories$Surv_annual

sym_mat <- matrix(as.character(pop_mod_setup$life_stages_symbolic), nrow = life_cycles$Value[life_cycles$Name == "Nstage"], ncol = life_cycles$Value[life_cycles$Name == "Nstage"])
write.csv(t(sym_mat), row.names = FALSE, file = "./inst/misc/nicola/steelhead/steelhead_symbolic_matrix.csv")



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
# Plot out simple Beverton-Holt relationship
getwd()
if(FALSE) {
  png(
    width = 5,
    height = 5,
    units = "in",
    res = 300,
    file = "bh.png"
  )
}


par(mar = c(5.1, 4.1, 2.1, 2.1))

bh <- function(K = NA,
               surv = NA,
               Nt = NA) {
  Nt2 <- (surv * Nt) / (1 + Nt * (surv / K))
  return(Nt2)
}

K <- stage_1_capacity / 1000
Nt <- seq(0, 5000, by = 5)
res <- lapply(Nt, bh, K = K, surv = surv_fry)
Nt2 <- unlist(res)
plot(
  Nt,
  Nt2,
  type = 'l',
  xlim = c(0, 3000),
  ylim = c(0, stage_1_capacity / 1000),
  xlab = "Age-0+ Fry ('000s)",
  ylab = "Age-1 Recruits ('000s)"
  #sub = "BH function with K = 100 and productivity = 0.8"
)
abline(0,
       surv_fry,
       lty = 3,
       col = "red",
       lwd = 1.5)
abline(
  h = K,
  lty = 2,
  col = "blue",
  lwd = 1.5
)

dev.off()


#=========================================================
# Run simple population projection - project forward through time
baseline <-
  Projection_DD(
    M.mx = life_stages_symbolic,
    # projection matrix expression
    D.mx = density_stage_symbolic,
    # density-dependence matrix
    H.mx = NULL,
    dat = life_histories,
    # life history data
    K = life_histories$Ka,
    # initial pop size as stage-structure vector
    Nyears = 500,
    # years to run simulation
    p.cat = 0,
    # Probability of catastrophe
    CE_df = NULL,
    stage_k_override = c(NA, stage_1_capacity, NA, NA, NA, 30000000, NA),
    bh_dd_stages = c("bh_stage_1")
  )


# Time series of the population with CE
dplot <- baseline$pop
dplot <- dplot[dplot$year > 100, ]
plot(dplot, type = 'l', xlim = c(100, 500))

names(baseline)
N <- as.data.frame(baseline$N)
N$year <- seq(0, nrow(N) - 1)
N <- N[N$year > 100, ]
N$adults <- dplot$N

tail(N, 3)
tail(dplot, 3)

getwd()
if(FALSE) {
  png(
    width = 5,
    height = 8,
    units = "in",
    res = 300
  )
}

par(mfrow = c(2, 1))

plot(
  N$adults / 1000,
  N$K1 / 1000,
  las = 1,
  ylab = "Age-1 Parr ('000s)",
  xlab = "Spawners ('000s)",
  cex = 0.5,
  main = "STEELHEAD SIMULATION"
)

plot(
  dplot,
  type = 'b',
  xlim = c(350, 500),
  cex = 0.5,
  pch = 19,
  ylab = "Simulated Spawners (N)",
  las = 1,
  xlab = "Simulation Year",
  main = "STEELHEAD SIMULATION"
)

dev.off()
