#' Project matrix with Density Dependence
#'
#' @description Project the matrix model forward in time with density dependence.
#'
#' @details The function runs the population projections forward through time. It is better to use PopulationModel_Run (a higher-level function) to ensure everything is input correctly for anadromous and non-anadromous runs.
#' Life-cycle specific stressors (if set) will be applied based on values in the `CE_df` table.
#' There are several ways to implement density dependence.
#'  deterministic projection matrix using `popbio::stable.stage` with initial parameters based on arguments provided. Applies CE stressors to appropriate targets based on `CE_df`. All population modeling components are contained within this function. Users define a projection matrix, density-dependence matrix, harm projection matrix, life history parameters, years to simulate, carrying capacity, catastrophic event probabilities and a cumulative effects data frame (CE_df). When run this function will project the population forward in time. See the vignette tutorial Population Model Overview for details and instructions.
#'
#' @param M.mx A projection matrix expression
#' @param D.mx A matrix of density-dependence effect
#' @param H.mx A harm projection matrix
#' @param dat Life history data
#' @param Nyears Years to run simulation
#' @param K The population carrying Capacity of adults (mature individuals). Used for back-calculating stage-specific carrying capacities with compensation ratios. Also used in anadromous model for setting K of adult spawners (across all age classes).
#' @param p.cat Probability of catastrophic event.
#' @param CE_df Cumulative effect data frame. Data frame identifying cumulative effects stressors targets system capacity or population parameter, or both, and target life stages.
#' @param K_adj Boolean. Should K_adj be run. Defaults to false.
#' @param stage_k_override Vector of K values for egg fry (age-0), fry (age-0) to stage_1, stage_1 to stage_2, etc. Defaults to NULL for compensation ratios. If set, values will override adult K value for density dependence with Beverton-Holt. NA values should be used in stages with no constraints. For example, if a fry to stage_1 bottle neck was set to 3000, the resulting vector should look like this c(NA, 3000, NA):
#' \itemize{
#'   \item 1. Egg to fry (K), NA - no constraint
#'   \item 2. Fry to stage_1 (K), constrained to 3000 fry entering stage_1
#'   \item 3. stage_1 to stage_2 (K), NA - no constraint
#' }
#' For anadromous species with a spawner capacity constraint, create a vector of NAs equal number
#' of stages + 1 and then set the spawner capacity in the K input parameter of this function.
#' @param bh_dd_stages Optional Character vector of life stages c("dd_hs_0", "bh_stage_1", "bh_stage_2", "bh_stage_3", ...) to apply classical Beverton-Holt density-dependence. To be used in place of compensation ratios if set. Use "dd_hs_0" for egg-to-fry k, "bh_stage_1" for fry to stage_1 k and "bh_stage_2" for stage_1 to stage_2 k etc. Densities are the capped value for the transition stage.
#' @param anadromous Boolean. If true, the model will apply anadromous life history parameters. Defaults to false.
#' @param alt_return Character. Alternative return objects for internal use.
#' @importFrom rlang .data
#'
#' @returns A list object with projected years, population size, lambda, fecundity, survival, catastrophic events.
#'
#' @export
Projection_DD <- function(M.mx = NA,
                          D.mx = NULL,
                          H.mx = NULL,
                          dat = NA,
                          Nyears = 100,
                          K = NA,
                          p.cat = NA,
                          CE_df = NULL,
                          K_adj = FALSE,
                          stage_k_override = NULL,
                          bh_dd_stages = NULL,
                          anadromous = FALSE,
                          alt_return = NULL
                          ) {


  life_stages_symbolic <- density_stage_symbolic <- NULL

  if(is.null(alt_return)) {
    alt_return <- ""
  }

  # Define variables in function as null
  # stable.stage <- Nstage <- E_est <- NULL

  # MJB set k to 500 if not defined in inputs
  if (is.na(K)) {
    K <- 500
    K_adults_set <- FALSE
  } else{
    K_adults_set <- TRUE
  }

  # MJB: Did user forget to set anadromous?
  if (("Nstage_B" %in% names(dat))  && !anadromous) {
    # print("Setting anadromous to TRUE in Projection_DD()...")
    anadromous <- TRUE
    if(!(dat$anadromous == anadromous)) {
      stop("anadromous must be set to TRUE in dat if Nstage_B is set...")
    }
  }

  # MJB: Should simple stage-specific K values be used instead of stable-stage
  # instead of compensation ratios
  if (!(is.null(stage_k_override))) {
    if ((length(stage_k_override) != (dat$Nstage + 1))) {
      stop(
        "length of stage_k_override must have value for dd_hs_0, bh_stage_1, and be equal to Nstage + 1..."
      )
    }
    # Reset fry survivorship to original
    dat$s0.1.det <- dat$Surv_annual[2]
    dat$S[2] <- dat$Surv_annual[2]
  }

  # Should total spawners be constrained
  dat$stage_k_spawners <- NULL
  if (!(is.null(bh_dd_stages))) {
    # check stage names ok
    if(!(bh_dd_stages_check_ok(bh_dd_stages))) {
      print(bh_dd_stages)
      stop("bh_dd_stages not set correctly...")
    }
    if (anadromous) {
      if(any(c("hs_spawners", "dd_hs_spawners", "bh_spawners") %in% bh_dd_stages)) {
        # Set total spawner adult capacity to K
        dat$stage_k_spawners <- K
        if(is.null(stage_k_override)) {
          stop("stage_k_override should be set...")
        }
      }
    }
  }


  # Adjust K to give correct mean after stochasticity
  if (is.null(dat$K.adj)) {
    dat$K.adj <- 1
  }
  Ka <- K * dat$K.adj


  # Make CC Adjustments run with K_adj (optional) but slower processing
  # Only relevant if running with compensation ratios
  if (K_adj) {
    kadj <- pop_model_K_adj(
      replicates = 100,
      dat = dat,
      mx = life_stages_symbolic,
      dx = density_stage_symbolic,
      Nyears = Nyears
    )
    dat$K.adj <- kadj$K.adj
    Ka <- K * dat$K.adj
  }


  # Set D = 1 if no density_depenence
  if (is.null(D.mx)) {
    D <- 1
  }

  # if no harm set H to 1
  if (is.null(H.mx)) {
    H <- rep(1, Nyears)
  } else if (is.matrix(H.mx)) {
    H <- replicate(Nyears, H.mx, simplify = F)
  } else {
    H <- H.mx
  }


  # Catastrophes
  # define which years will have catastrophic events
  if (any(is.na(dat$gen.time))) {
    message("NA value in gen.time (review matrix)...")
  }

  if(is.null(p.cat)) {
    p.cat <- 0
  }
  if(is.na(p.cat)) {
    p.cat <- 0
  }

  dat$gen.time <- ifelse(is.na(dat$gen.time), 1, dat$gen.time)
  Catastrophe <- sample(
    c(1, 0),
    Nyears,
    replace = TRUE,
    prob = c(p.cat / dat$gen.time, 1 - p.cat / dat$gen.time)
  )

  # effect of catastrophe on pop size (percent reduction)
  # scaled Beta dist'n fit from Reed et al. (2003)
  # Biological Conservation 113 (2003) 23–34
  e.cat <- sapply(Catastrophe, function(x) {
    ifelse(x == 0,
           NA,
           stats::rbeta(1, shape1 = 0.762, shape2 = 1.5) * (1 - .5) + .5)
  })


  # ----------------------------------
  # Initialize parameters
  # ----------------------------------
  # Create the deterministic projection matrix

  if(anadromous) {
    # For anadromous runs
    pmx.det <- pmx_eval(M.mx, c(dat, dat$S, dat$nYrs, dat$mat, dat$u, dat$smig, dat$eps))
    SS <-
      popbio::stable.stage(pmx_eval(M.mx, c(dat, dat$S, dat$nYrs, dat$mat, dat$u, dat$smig, dat$eps)))
  } else {
    # For non-anadromous populations
    pmx.det <- pmx_eval(M.mx, c(dat, dat$S, dat$nYrs, dat$mat))
    SS <-
      popbio::stable.stage(pmx_eval(M.mx, c(dat, dat$S, dat$nYrs, dat$mat)))
  } # end of deterministic projection matrix


  # ============================================
  # Define the initial K (Capacity) of adults
  # ============================================

  # Ka (e.g., 5000) will be split across adult stages that are mature
  mat_stage <- dat$mat[dat$mat > 0]

  # evaluate whether we are quantifying the initial carrying capacities correctly
  if(anadromous) {
    # For anadromous use stage names to reference adult spawners
    mat_stage_n <- as.numeric(gsub("mat", "", names(mat_stage)))
    mat_stage_index <- match(paste0("stage_B_", mat_stage_n), dat$stage_names)
    total_adults <- sum(SS[mat_stage_index])
    k_stage <- (SS / total_adults) * Ka

  } else {
    # For non-anadromous simple reference index from matrity
    mat_stage_n <- as.numeric(gsub("mat", "", names(mat_stage)))
    total_adults <- sum(SS[mat_stage_n])
    k_stage <- (SS / total_adults) * Ka
  }

  # MJB added line - Nstage was floating in global memory from older KW code
  Nstage <- dat$Nstage

  # -------------------------------------------------------------------
  # Define capacities across stage classes and name stages accordingly
  # -------------------------------------------------------------------
  if(anadromous) {

    # For anadromous populations assign stage names based on spawning and
    # non-spawning classes
    names(k_stage) <- dat$stage_names
    dat$K <- k_stage
    # Back-calculate from stage 1
    # MJB: At stable stage need to cal from stage 1
    dat$Ke <- dat$K[1] / (dat$S["sE"] * dat$S["s0"])
    dat$K0 <- dat$K[1] / (dat$S["s0"])
    names(dat$K0) <- "K0"
    names(dat$Ke) <- "Ke"

    #  Confirm bi-directional calculations line up (pre-birth pulse)
    #  Calculate N_eggs K from adult spawner K
    #     sum(N_B_adults * events * eps * SR * u / int)
    #     sum(5000 * 1 * 4000 * 0.5 * 0.95 / 1) # 9,500,000
    #  # Calculate N_eggs K from stage_1 count
    #     Number Stage 1 / (sE * s0)
    #     dat$K[1] / (dat$S["sE"] * dat$S["s0"])
    #     417710.9 / 0.04396957 # 9,500,000

  } else {

    # For non-anadromous populations simply assign stage names
    names(k_stage) <- paste("K", 1:Nstage, sep = "")
    dat$K <- k_stage
    # MJB: dat$K[-1] -1 index is to line up with missing mat1 designation
    dat$Ke <- E_est(N = dat$K[-1], dat = c(dat, dat$mat, dat$S))
    dat$K0 <- dat$Ke * dat$S["sE"]

  }


  # ---------------------------------------------------------------------
  # Override SS (stable-stage) stage-specific carrying capacity K-values
  # with custom values. Usually from BH functions
  # ---------------------------------------------------------------------

  # Should simple stage-specific K values be used instead of stable-stage
  # instead of compensation ratios
  if (!(is.null(stage_k_override))) {

    # Determine if stage K calculations should be name or index-based.
    if(is.null(names(stage_k_override))) {
      # Assign names based on index. Assume position 1 is fry (age-0)
      names(stage_k_override) <- c("K0", paste("K", 1:dat$Nstage, sep = ""))
    } # or use names provided

    dat$stage_k_override <- stage_k_override

    # Running with stage-specific K values supplied for location
    # egg to fry capacity
    dat$K0 <-
      ifelse(is.na(stage_k_override["K0"]), dat$K0, stage_k_override[1])

    # For older stages - update adult vector
    k_stage_mod <- stage_k_override[2:(Nstage + 1)]
    k_stage <- ifelse(is.na(k_stage_mod), k_stage, k_stage_mod)
    names(k_stage) <- paste("K", 1:Nstage, sep = "")
    dat$K <- k_stage

    # Override K and Ka if set
    last_stage <- stage_k_override[(Nstage + 1)]

    # Remember we can define spawners seperatly as their own entity
    Ka <- ifelse(is.na(last_stage), Ka, last_stage)
    K <- ifelse(is.na(last_stage), K, last_stage)

    if(alt_return == "check_k_stage") {
      return(list(dat, k_stage, Ka, K))
    }

  } # or run with default K values from compensation ratios (stable stage)



  # ----------------------------------
  # Apply CE stressors to vital rates
  # ----------------------------------

  # Apply the harm matrix to K or S (if needed)
  # MJB: Apply CE stressors to population parameters
  # if not null
  # if (!(is.null(CE_df))) {
  #   dat <- pop_model_ce_apply(
  #     CE_df = CE_df,
  #     dat = dat
  #   )
  # }

  # --------------------------------------------
  # Setup stochastic transition probabilities
  # --------------------------------------------

  # Fecundity
  ft <- lapply(1:(Nyears + 1), function(x) {
    f_temp <- f_rand(dat$eps, dat$eps_sd, rho = dat$egg_rho)
    f_temp <- ifelse(is.na(f_temp), dat$eps, f_temp)
    if(!(anadromous)) {
      names(f_temp) <- "eps"
    }
    return(f_temp)
  })

  # Survival
  suppressWarnings({
    st <- lapply(1:(Nyears + 1), function(x) {
      s_temp <- s_rand(dat$S, dat$M.cv, rho = dat$M.rho)
      s_temp <- ifelse(is.na(s_temp), 0, s_temp)
      s_temp
    })
  })

  # Pre-spawn mortality vector (ut) and spawning migration survival smig.
  if(anadromous) {
    ut <- lapply(1:(Nyears + 1), function(x) {
      ut_temp <- s_rand(dat$u, dat$M.cv, rho = dat$M.rho)
      ut_temp <- ifelse(is.na(ut_temp), 0, ut_temp)
      ut_temp
    })
    smigt <- lapply(1:(Nyears + 1), function(x) {
      smigt_temp <- s_rand(dat$smig, dat$M.cv, rho = dat$M.rho)
      smigt_temp <- ifelse(is.na(smigt_temp), 0, smigt_temp)
      smigt_temp
    })
  } else {
    ut <- NULL
    smigt <- NULL
  }

  # Create capacity reference vectors
  K_list <- replicate((Nyears + 1), dat$K, simplify = FALSE)
  Ke_list <- replicate((Nyears + 1), dat$Ke, simplify = FALSE)
  K0_list <- replicate((Nyears + 1), dat$K0, simplify = FALSE)
  Kspawners_list <- replicate((Nyears + 1), dat$stage_k_spawners, simplify = FALSE)


  # ===========================================
  # Apply CE stressors (if any) to vital rates
  # ===========================================

  if (!(is.null(CE_df))) {
    rates <- list(
      ft = ft, # Fecundity
      st = st, # Survivorship (stage)
      ut = ut, # Pre-spawn mortality
      smigt = smigt, # spawner migration mortality
      K_list = K_list, # stage capacities
      Ke_list = Ke_list, # egg capacities
      K0_list = K0_list, # fry capacities
      Kspawners_list = Kspawners_list # spawner capacities
    )
    # Reduce vital rates with CE stressors
    rates <- pop_model_ce_apply_year(
      rates = rates,
      CE_df = CE_df,
      anadromous = anadromous,
      mat_stage = mat_stage
    )

    # Update rates with CE effects applied
    ft <- rates$ft
    st <- rates$st
    ut <- rates$ut
    smigt <- rates$smigt
    K_list <- rates$K_list
    Ke_list <- rates$Ke_list
    K0_list <- rates$K0_list
    Kspawners_list <- rates$Kspawners_list
  }


  # Get ready to project populations forward through time

  new_dat <- dat
  new_dat[["eps"]] <- NULL

  # Population matrix
  if(anadromous) {
    # Anadrmous matrices
    M.list <- lapply(1:(Nyears + 1), function(x) {
      pmx_eval(M.mx, c(new_dat, st[[x]], ft[[x]], ut[[x]], smigt[[x]], new_dat$nYrs, new_dat$mat))
    })
  } else {
    # Non-anadrmous matrices
    M.list <- lapply(1:(Nyears + 1), function(x) {
      pmx_eval(M.mx, c(new_dat, st[[x]], ft[[x]], new_dat$nYrs, new_dat$mat))
    })
  }


  # Optional QA Check: Look at distribution of lambda value across projection matrices
  # Should be close to 1.0 if using compensation ratios to control DD
  # or should be close to lambda from static matrix if using stage-specific K values
  if(alt_return == "qa_matrices") {
    return(M.list)
  }

  # Initial population structure
  N <- sum(new_dat$K) * popbio::stable.stage(M.list[[1]])

  # MJB: If no k is set then start with 1 in each class
  if (all(is.na(N))) {
    N <- ifelse(is.na(N), 1, N)
  }

  # MJB: Otherwise set starting vector to custom k
  if (!(is.null(stage_k_override))) {
    N <- dat$K
  }

  # number of Egg produced
  if(anadromous) {
    # assume max 1 year per stage pre-birth census
    stage_1s <- N[1]
    N_fry <- stage_1s / st[[1]]["s0"]
    E <- N_fry / st[[1]]["sE"]
    names(E) <- NULL
    # Vector of spawners is directly B classes
    Na <- sum(N[grepl("_B_", dat$stage_names)])
  } else {
    Na <- Nb_est(N[-1], new_dat$mat)
    E <-
      E_est(N = N[-1],
            dat = c(new_dat, ft[[1]], st[[1]], new_dat$nYrs, new_dat$mat))
  }


  # initialize output vectors
  Nvec <- rep(NA, Nyears + 1)

  # Adult pop vector
  Nvec[1] <- Na

  # age-specific annual population size
  Ns <- list(N)

  # population growth rate
  lambdas <- rep(NA, Nyears)

  # ======================================
  # loop through (t) years
  for (t in 1:Nyears) {

    # Store previous abundance vector for BH equations
    N_prev <- N

    # Density-Dependent Survival
    if (is.null(D.mx) == FALSE) {
      # create vector of density dependence effects
      # using compensation ratios
      d.vec <-
        d.vec.f(
          df = new_dat,
          Nv = c(E, E * st[[t + 1]]["sE"], N),
          Ks = c(new_dat$Ke, new_dat$K0, new_dat$K)
        )

      # check if any survival rates > 1
      # survival rate after DD effects
      s.test <- d.vec[1:length(st[[t + 1]])] * st[[t + 1]]

      # added: to deal with frequent NA issue here...
      s.test <- ifelse(is.na(s.test), 0, s.test)

      if (any(s.test > 1)) {
        # any s.test > 1?
        s.err <- which(s.test > 1) # ID which is > 1
        # set density dependence effect of 1/survival - give s = 1 after DD effects
        d.vec[s.err] <- 1 / st[[t + 1]][s.err]
      }

      # create density dependence effects matrix
      D <- pmx_eval(D.mx, as.list(d.vec))

      # Remove possibility of NA values at very high/low adundances
      D <- ifelse(is.na(D), 1, D)
    }

    # Population Projection matrix
    A <- M.list[[t + 1]] * D * H[[t]]

    # Quick check - going up or down
    # popbio::lambda(A)

    # project the population 1 year ahead.
    # or apply catastrophe
    if (Catastrophe[t] == 1) {
      # Apply catastrophe
      N <- N * (1 - e.cat[t])
    } else {
      # Project ahead
      N <- as.vector(A %*% N)
    }

    # Prevent runaway to Inf
    too_many <- sum(N_prev, na.rm = TRUE)
    if(too_many > 10^100) {
      # Lock values at previous time step to prevent Inf
      N <- N_prev
    }


    # Allow for stage-specific DD effects
    # Constrain using Beverton-Holt functions

    if (is.null(bh_dd_stages) == FALSE) {
      if(!(all(is.na(N)))) {

        # Reference capacities for current year (after applying stressors)
        new_dat$Ke <- Ke_list[[t+1]]
        new_dat$K0 <- K0_list[[t+1]]
        new_dat$K  <- K_list[[t+1]]
        new_dat$stage_k_spawners  <- Kspawners_list[[t+1]]

        # Apply Density-Dependant Beverton-Holt Constraints
        N <- dd.N.bh(
          dat = new_dat,
          t = t,
          st = st,
          N = N,
          N_prev = N_prev,
          bh_dd_stages = bh_dd_stages,
          smig = smigt[[t + 1]]
        )
        # (MJB Sept 27 2023 - update to allow for extinction)
      }
    }



    if (anadromous) {

      # For anadromous populations
      # Number of adult spawners
      Na <- sum(N[grepl("_B_", dat$stage_names)])
      # Number of non-spawner stages
      Nj <- sum(N[grepl("_Pb_", dat$stage_names)])
      # Number of eggs
      E <- N[1] / (st[[t + 1]]["sE"] * st[[t + 1]]["s0"])
    } else {

      # For non-anadromous populations
      # Number of adults
      Na <- Nb_est(N[-1], new_dat$mat)
      # Number of Juveniles
      Nj <- sum(N * c(1, 1 - new_dat$mat))
      # number of Egg produced
      E <-
        E_est(N = N[-1], c(new_dat, st[[t + 1]], ft[[t + 1]], new_dat$mat))
    }



    # Number of mature fish in pop
    Nvec[t + 1] <- Na
    Ns[t + 1] <- list(N)

    # pop growth rate
    lambdas[t] <- Nvec[t + 1] / Nvec[t]

  } # end of loop through (t) years...
  # ======================================

  # Build return object from function
  ret_obj <- list(
    "pop" = as.data.frame(list(year = 0:Nyears, N = Nvec)),
    "N" = do.call(rbind, Ns),
    "lambdas" = lambdas,
    "vars" = list("ft" = do.call(rbind, ft), "st" = do.call(rbind, st)),
    "Cat." = as.data.frame(list("Cat." = Catastrophe, "e.cat" = e.cat))
  )

  return(ret_obj)
}
