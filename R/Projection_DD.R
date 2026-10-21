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
#' @param p.cat Probability of a catastrophic event per generation (converted
#' internally to an annual probability as p.cat / generation time). In a
#' catastrophe year the population is first projected normally - fish age,
#' spawn, transition between stages, suffer natural mortality and any density
#' dependence is applied - and then the catastrophe removes a random fraction
#' (50 to 100 percent; scaled beta distribution fit from Reed et al. 2003) of
#' every life stage. Catastrophic mortality is therefore IN ADDITION to
#' normal annual mortality, not a replacement for it.
#' @param CE_df Cumulative effect data frame. Data frame identifying cumulative effects stressors targets system capacity or population parameter, or both, and target life stages. Note for hand-built tables: a stressor represented by a SINGLE row is treated as a constant effect applied to every simulation year - even if that row carries a Year value. To make an effect year-specific, supply one row per simulation year (with a Year column), or use the From_Year/To_Year activation-window columns; the workbook pathway (PopulationModel_Run) always builds full per-year rows.
#' @param K_adj Boolean. DEPRECATED legacy pathway; defaults to FALSE and
#' should generally stay FALSE. When TRUE, runs a slow 100-replicate
#' stochastic calibration that inflates K by the ratio of the target adult
#' K to the realized stochastic mean (compensation-ratio mode only; never
#' set by PopulationModel_Run; requires Nyears > 51). Retained for
#' backwards compatibility with older scripts.
#' @param stage_k_override Vector of stage-specific carrying capacity (K) values. Position 1 is the egg-to-fry flow capacity K0 (maximum fry produced within a census year - this transition always completes within one year and can never carry holdovers). Positions 2 onward are the maximum TOTAL abundance of stage_1, stage_2, etc.: holdover individuals remaining in a multi-year stage plus newly admitted recruits. Defaults to NULL for compensation ratios. NA values should be used in stages with no constraints. For example, if stage_1 capacity was set to 3000, the resulting vector should look like this c(NA, 3000, NA):
#' \itemize{
#'   \item 1. Egg to fry flow capacity (K0), NA - no constraint
#'   \item 2. stage_1 total capacity, constrained to 3000 individuals
#'   \item 3. stage_2 total capacity, NA - no constraint
#' }
#' For anadromous species with a spawner capacity constraint, create a vector of NAs equal number
#' of stages + 1 and then set the spawner capacity in the K input parameter of this function.
#' Note that supplying stage_k_override (even as all NA values) switches the run into
#' habitat-capacity mode: fry survival (s0) is NOT recalibrated to force lambda = 1 at the
#' adult capacity K, because growth is expected to be restrained by the explicit stage
#' constraints instead. Blank/NA entries mean "no constraint for that stage", not that the
#' value is unknown.
#' @param bh_dd_stages Optional Character vector of life stages c("dd_hs_0", "bh_stage_1", "bh_stage_2", "bh_stage_3", ...) to apply Beverton-Holt (bh_ prefix) or hockey-stick (hs_ prefix) density-dependence. To be used in place of compensation ratios if set. Use "dd_hs_0" (or "bh_stage_0") for the egg-to-fry flow cap and "bh_stage_1", "bh_stage_2", etc. for stage capacities. Stage capacities are the maximum TOTAL abundance of each stage (holdovers surviving in a multi-year stage plus this year's recruits). Recruits are admitted into the residual capacity (K minus holdovers) through a Beverton-Holt curve (bh_) or the stage total is hard-capped (hs_). If holdovers alone exceed a stage's K (e.g., the year after a capacity stressor reduces K), the stage is trimmed down to K and no recruits are admitted.
#' @param stage_k_noise Optional matrix of per-year multiplicative noise
#' factors for the capacity constraints (habitat K uncertainty), generated
#' with \code{\link{build_stage_k_noise}} from the k_..._cv columns of the
#' habitat capacity file. Must have Nyears rows and Nstage + 2 columns:
#' column 1 scales the fry flow cap (K0), columns 2..(Nstage + 1) scale the
#' stage capacities, and the last column scales the total spawner capacity.
#' Factors multiply the capacity values AFTER any CE capacity stressors are
#' applied; the initial population vector and the derived egg capacity (Ke)
#' always use the mean (un-noised) capacities. Default NULL (capacities
#' treated as exact). PopulationModel_Run() generates one matrix per batch
#' replicate and passes the same matrix to the CE and baseline runs.
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
                          stage_k_noise = NULL,
                          anadromous = FALSE,
                          alt_return = NULL
                          ) {

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
    # Reset fry survivorship to original (raw file value).
    # Supplying stage_k_override - even a vector of all NA values - places
    # the run in habitat-capacity mode: growth is meant to be restrained by
    # explicit K constraints (NA = no constraint for that stage, by design),
    # so the lambda = 1 calibration of s0 from pop_model_matrix_elements()
    # is deliberately undone here. build_k_for_proj_dd() warns when a
    # habitat row contains no usable K values at all.
    dat$s0.1.det <- dat$Surv_annual[2]
    dat$S[2] <- dat$Surv_annual[2]
  }

  # Should total spawners be constrained
  dat$stage_k_spawners <- NULL
  if (!(is.null(bh_dd_stages))) {
    # dd_hs_spawners is an accepted alias of hs_spawners (mirroring the
    # dd_hs_0 alias of hs_stage_0) - normalize before validation so the
    # rest of the pipeline only sees the canonical token
    bh_dd_stages <- ifelse(tolower(bh_dd_stages) == "dd_hs_spawners",
                           "hs_spawners", bh_dd_stages)
    # check stage names ok
    if(!(bh_dd_stages_check_ok(bh_dd_stages))) {
      message(bh_dd_stages)
      stop("bh_dd_stages not set correctly...")
    }
    if(any(c("hs_spawners", "bh_spawners") %in% bh_dd_stages)) {
      # Set total spawner adult capacity to K
      dat$stage_k_spawners <- K
      if(is.null(stage_k_override)) {
        stop("stage_k_override should be set...")
      }
    }
    # Mixed density-dependence configuration: compensation ratios (cr > 1)
    # and stage-specific K constraints (bh_dd_stages) both active. This is
    # allowed but rarely intended - density dependence will be applied by
    # both mechanisms simultaneously.
    if (any(as.numeric(dat$cr) > 1, na.rm = TRUE)) {
      warning(
        "Both compensation ratios (cr > 1) and stage-specific K values ",
        "(bh_dd_stages) are active. Density dependence will be applied by ",
        "both mechanisms. Set cr values to 1 in the life cycles profile ",
        "when using habitat capacity K files.",
        call. = FALSE
      )
    }
  }


  # Adjust K to give correct mean after stochasticity
  if (is.null(dat$K.adj)) {
    dat$K.adj <- 1
  }
  Ka <- K * dat$K.adj


  # ---------------------------------------------------------------------
  # DEPRECATED PATHWAY: K_adj = TRUE (legacy stochastic K calibration)
  # ---------------------------------------------------------------------
  # Runs 100 replicate simulations to estimate the ratio between the target
  # adult K and the realized stochastic mean, then inflates K accordingly
  # (stochastic variability drags the long-run mean below the deterministic
  # K). Retained for backwards compatibility with older scripts only:
  #   - never set by PopulationModel_Run or the Shiny app
  #   - deliberately NOT exercised by the test suite (the 100 replicate
  #     simulations are too slow); repaired and verified manually 2026-09-15
  #     (previously passed NULL matrices and crashed: "0 x 0 matrix")
  #   - only meaningful in compensation-ratio mode
  if (K_adj) {
    if (Nyears <= 51) {
      stop(
        "K_adj = TRUE requires Nyears > 51: a 51-year burn-in is discarded ",
        "from each calibration replicate.",
        call. = FALSE
      )
    }
    if (!(is.null(bh_dd_stages))) {
      warning(
        "K_adj is a compensation-ratio-mode calibration and is not ",
        "meaningful when stage-specific K values (bh_dd_stages) control ",
        "density dependence.",
        call. = FALSE
      )
    }
    kadj <- pop_model_K_adj(
      replicates = 100,
      dat = dat,
      mx = M.mx,
      dx = D.mx,
      Nyears = Nyears,
      K = K
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
    H <- replicate(Nyears, H.mx, simplify = FALSE)
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
    # (dat$eps unpacks to eps2..epsN for the per-stage fecundity symbols)
    pmx.det <- pmx_eval(M.mx, c(dat, dat$S, dat$nYrs, dat$mat, dat$eps))
    SS <-
      popbio::stable.stage(pmx_eval(M.mx, c(dat, dat$S, dat$nYrs, dat$mat, dat$eps)))
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

    # Do not rely on stable stage distribution
    # assume other stages are unconstrained
    k_stage <- ifelse(is.na(k_stage_mod), k_stage, k_stage_mod)

    if (anadromous) {
      # Preserve the anadromous stage-class names (stage_Pb_x / stage_B_x)
      # through the override: the CE capacity machinery resolves
      # age-specific spawner targets (e.g. life_stage "b4") by these
      # names. Renaming to generic K1..KN here silently orphaned those
      # targets (a write to a missing name APPENDS an element, growing
      # the K vector and crashing dd.N.bh's length check).
      names(k_stage) <- dat$stage_names
    } else {
      names(k_stage) <- paste("K", 1:Nstage, sep = "")
    }
    dat$K <- k_stage

    # Override K and Ka if set
    last_stage <- stage_k_override[(Nstage + 1)]

    # Remember we can define spawners separately as their own entity
    Ka <- ifelse(is.na(last_stage), Ka, last_stage)
    K <- ifelse(is.na(last_stage), K, last_stage)

    if(alt_return == "check_k_stage") {
      return(list(dat, k_stage, Ka, K))
    }

  } # or run with default K values from compensation ratios (stable stage)



  # --------------------------------------------
  # Setup stochastic transition probabilities
  # --------------------------------------------
  # Note: CE stressors are applied per-year further below via
  # pop_model_ce_apply_year() once the stochastic rate lists are built.

  # Fecundity
  # Non-anadromous eps is per-stage (eps2..epsN, aligned with mat2..matN).
  # When every mature stage shares one fecundity value (the legacy
  # single-eps case) a single value is drawn per year and broadcast, so
  # the random-number stream - and therefore every seeded legacy result -
  # matches the historical scalar behaviour exactly. Distinct per-stage
  # values get correlated per-stage draws (compound symmetry, egg_rho),
  # mirroring the anadromous per-age behaviour.
  if (anadromous) {
    ft <- lapply(1:(Nyears + 1), function(x) {
      f_temp <- f_rand(dat$eps, dat$eps_sd, rho = dat$egg_rho)
      f_temp <- ifelse(is.na(f_temp), dat$eps, f_temp)
      return(f_temp)
    })
  } else {
    eps_mature <- dat$eps[!is.na(dat$mat) & dat$mat > 0]
    single_eps <- length(unique(eps_mature)) <= 1
    ft <- lapply(1:(Nyears + 1), function(x) {
      if (single_eps) {
        mn <- if (length(eps_mature) > 0) eps_mature[[1]] else 0
        f_one <- f_rand(mn, dat$eps_sd, rho = dat$egg_rho)
        f_one <- ifelse(is.na(f_one), mn, f_one)
        f_temp <- rep(f_one, length(dat$eps))
      } else {
        # Draw only for stages with positive fecundity; structural zeros
        # (immature stages) stay 0 without consuming random numbers or
        # producing qlnorm NaN warnings.
        f_temp <- rep(0, length(dat$eps))
        pos <- which(dat$eps > 0)
        if (length(pos) > 0) {
          f_draw <- f_rand(dat$eps[pos], dat$eps_sd, rho = dat$egg_rho)
          f_draw <- ifelse(is.na(f_draw), dat$eps[pos], f_draw)
          f_temp[pos] <- f_draw
        }
      }
      names(f_temp) <- names(dat$eps)
      return(f_temp)
    })
  }

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


  # ===============================================================
  # Apply stochastic capacity noise (habitat K uncertainty), if any
  # ===============================================================
  # stage_k_noise holds per-year multiplicative factors for the capacity
  # constraints: column 1 = fry flow cap (K0), columns 2..(Nstage + 1) =
  # stage capacities, column Nstage + 2 = total spawner capacity. Factors
  # are applied on top of any CE capacity stressors (multiplicative). The
  # initial population vector and the derived egg capacity Ke use the mean
  # capacities (year slot 1 is left untouched). NA capacities stay NA
  # (no constraint).
  if (!is.null(stage_k_noise)) {
    if (!is.matrix(stage_k_noise) ||
        ncol(stage_k_noise) != (Nstage + 2) ||
        nrow(stage_k_noise) < Nyears) {
      stop(
        "stage_k_noise must be a matrix with Nyears rows and Nstage + 2 ",
        "columns (fry flow cap K0, stage capacities, total spawner ",
        "capacity). See build_stage_k_noise()..."
      )
    }
    for (t in 1:Nyears) {
      K0_list[[t + 1]] <- K0_list[[t + 1]] * stage_k_noise[t, 1]
      K_list[[t + 1]] <- K_list[[t + 1]] * stage_k_noise[t, 2:(Nstage + 1)]
      if (!is.null(Kspawners_list[[t + 1]])) {
        Kspawners_list[[t + 1]] <-
          Kspawners_list[[t + 1]] * stage_k_noise[t, (Nstage + 2)]
      }
    }
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

  # If all zeros then start with 1 in each stage class
  if (all(N == 0)) {
    N <- rep(1, length(N))
  }

  # Number of adults at initialization. (The egg count E used for the
  # egg/fry crowding terms is computed INSIDE the year loop, from the
  # standing population at the start of each year with THAT year's rate
  # draws - see the census-timing note in the loop.)
  if(anadromous) {
    # Vector of spawners is directly B classes
    Na <- sum(N[grepl("_B_", dat$stage_names)])
  } else {
    Na <- Nb_est(N[-1], new_dat$mat)
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

    # Reference capacities for the current year (CE capacity stressors
    # applied via K_list/Ke_list/K0_list). Must be set before d.vec.f()
    # so both the compensation-ratio and Beverton-Holt pathways see the
    # current year's capacities.
    new_dat$Ke <- Ke_list[[t + 1]]
    new_dat$K0 <- K0_list[[t + 1]]
    new_dat$K  <- K_list[[t + 1]]
    new_dat$stage_k_spawners <- Kspawners_list[[t + 1]]

    # Census-timing convention for the egg/fry crowding terms (cr_E and
    # cr_0 compensation ratios): the eggs "in the gravel" during year t's
    # projection step are those laid by the CURRENT standing population
    # (the spawners at the start of the step), evaluated with YEAR t's
    # fecundity / pre-spawn survival draws - exactly the quantities the
    # projection matrix A applies below. The fry count is those eggs times
    # year t's egg survival. (Historically E was carried over from the end
    # of the previous year - computed with the previous year's draws, and
    # in anadromous runs back-calculated from a stage-1 count already
    # reduced by capacity trimming and catastrophes - which smeared the
    # crowding terms across two years' rates and deflated the egg count.)
    if (anadromous) {
      b_idx <- match(paste0("stage_B_", dat$spawning_years), dat$stage_names)
      eps_yr <- ft[[t + 1]][paste0("eps", dat$spawning_years)]
      u_yr <- ut[[t + 1]][paste0("u", dat$spawning_years)]
      E <- sum(N[b_idx] * u_yr * new_dat$events * eps_yr *
                 new_dat$sR / new_dat$int, na.rm = TRUE)
      names(E) <- NULL
    } else {
      E <- E_est(N = N[-1],
                 dat = c(new_dat, st[[t + 1]], ft[[t + 1]], new_dat$mat),
                 eps = ft[[t + 1]])
    }

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

    # Project the population 1 year ahead. Every year - including
    # catastrophe years - runs the full projection: fish age, spawn,
    # transition between stages and suffer natural mortality. Catastrophic
    # mortality (applied further below, after density dependence) acts IN
    # ADDITION to the normal annual projection, not instead of it.
    N <- as.vector(A %*% N)

    # Prevent runaway to Inf
    too_many <- sum(N_prev, na.rm = TRUE)
    if(too_many > 10^100) {
      # Lock values at previous time step to prevent Inf
      N <- N_prev
    }


    # Allow for stage-specific DD effects
    # Constrain using Beverton-Holt functions
    # Runs on the projected vector; catastrophic mortality is applied AFTER
    # this step, so density dependence can never rebuild a stage above its
    # post-catastrophe value.
    if (is.null(bh_dd_stages) == FALSE) {
      if(!(all(is.na(N)))) {

        # (Capacities for the current year are refreshed at the top of the
        # year loop, before d.vec.f())

        # Apply Density-Dependant Beverton-Holt Constraints
        # A (realized projection matrix for this year) lets dd.N.bh split
        # each stage into holdovers and inflow exactly
        N <- dd.N.bh(
          dat = new_dat,
          t = t,
          st = st,
          N = N,
          N_prev = N_prev,
          bh_dd_stages = bh_dd_stages,
          A = A
        )
        # (MJB Sept 27 2023 - update to allow for extinction)
      }
    }


    # Apply catastrophic mortality (if any) IN ADDITION to the normal
    # annual projection: the event removes a random fraction e.cat
    # (50 - 100%; Reed et al. 2003) of every stage on top of that year's
    # natural mortality, aging, reproduction and density dependence.
    # Applied last so no downstream step can rebuild killed individuals.
    if (Catastrophe[t] == 1) {
      N <- N * (1 - e.cat[t])
    }


    if (anadromous) {

      # For anadromous populations
      # Number of adult spawners
      Na <- sum(N[grepl("_B_", dat$stage_names)])
      # Number of non-spawner stages
      Nj <- sum(N[grepl("_Pb_", dat$stage_names)])
    } else {

      # For non-anadromous populations
      # Number of adults
      Na <- Nb_est(N[-1], new_dat$mat)
      # Number of Juveniles
      Nj <- sum(N * c(1, 1 - new_dat$mat))
    }
    # (Egg count E for the crowding terms is recomputed at the start of
    # the next year from the new standing population - see above)



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
