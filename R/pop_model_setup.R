#' Population Model Setup
#'
#' @description Generate symbolic objects for the population model and
#' then generate parameter matrices.
#'
#' @details This is an intermediate setup function to run the population,
#' but some of its outputs are useful on their own, especially for eigen
#' analyses from the projection matrix. pop_model_setup() is run
#' before pop_model_matrix_elements() and Projection_DD().
#'
#' Transition Matrix:
#' * Calculates stage-specific survivals and transition rates, survival probabilities, transition probabilities.
#' * Initializes the survival/transition rates subject to a stage-specific density-dependence.
#' * Checks for excessive compensation ratios.
#'
#' Dataset for life-histories:
#' * transition probabilities
#' * egg-survival, age-0 survival, and then stage-specific survival probabilities
#' * spawning events per year
#' * eggs-per-spawner
#' * number of spawning intervals
#' * adult carrying capacity
#' * sex ratio
#'
#' @param life_cycles Data frame. Raw life cycles.csv data frame.
#'
#' @returns a list object of symbolic objects.
#'
#' @export
#'
pop_model_setup <- function(life_cycles = NA) {

  # Are there problems with the input parameters
  possible_error_state <- "All Good"

  # Load the species life cycles traits
  # must have N number of survival, years, and compensation ratios

  # Drop extra columns (if any)
  life_cycles <- life_cycles[, c("Parameters", "Name", "Value")]

  #------------------------------------------------------------
  # Determine if anadromous transformations need to be applied
  #------------------------------------------------------------
  if(!("anadromous" %in% life_cycles$Name)) {

    # if anadromous is not specified assume false
    anadromous <- FALSE

  } else {

    # anadromous exists in the input file
    anadromous <- life_cycles$Value[life_cycles$Name == "anadromous"]

    if(anadromous == "TRUE") {
      anadromous <- TRUE
    }
    if(anadromous == "FALSE") {
      anadromous <- FALSE
    }

    if(!is.logical(anadromous)) {
      anadromous <- as.numeric(as.character(anadromous))
    }

    if(anadromous == 1) {
      anadromous <- TRUE
    }
    if(anadromous == 0) {
      anadromous <- FALSE
    }
    if(is.na(anadromous)) {
      stop("Error - could not determine anadromous class...")
    }
  }

  # Re-strcuture for anadromous model
  if(anadromous) {
    # Call pop_model_setup_anadromous() and exit this function
    life_cycles$Value[life_cycles$Name == "anadromous"] <- TRUE
    ret_obj <- pop_model_setup_anadromous(life_cycles = life_cycles)
    return(ret_obj)
  }


  # ==========================================
  # Fix eps
  # ==========================================

  # Check if "eps_1", "eps_2", "eps_3... is defined but not "eps"
  check_sum <- sum(grepl("eps_", life_cycles$Name))

  # should be just 1 for eps_sd - if many
  if(check_sum > 1 & !("eps" %in% life_cycles$Name)) {
    # Find the eps_1, eps_2, eps_3... and sum them
    eps_vals <- life_cycles$Value[life_cycles$Name %in% paste0("eps_", 1:1000)]
    eps_vals <- as.numeric(eps_vals)
    eps_vals <- eps_vals[!(is.na(eps_vals))]
    eps_vals <- eps_vals[eps_vals > 0]
    # Add the sum to the data frame
    add_row <- data.frame(Parameters = "eps", Name = "eps", Value = stats::median(eps_vals, na.rm = TRUE))
    life_cycles <- rbind(life_cycles, add_row)
  }

  # Check if both "eps_1", "eps_2", "eps_3... and "eps" are defined
  if(check_sum > 1 & ("eps" %in% life_cycles$Name)) {
    # check if eps value is na or blank
    main_eps <- life_cycles$Value[life_cycles$Name == "eps"]
    if(is.na(main_eps)) {
      eps_vals <- life_cycles$Value[life_cycles$Name %in% paste0("eps_", 1:1000)]
      eps_vals <- eps_vals[!(is.na(eps_vals))]
      eps_vals <- eps_vals[eps_vals > 0]
      life_cycles$Value[life_cycles$Name == "eps"] <- eps_vals
    }
    # delete the opther eps_ rows
    life_cycles <- life_cycles[which(!(life_cycles$Name %in% paste0("eps_", 1:1000))), ]
  }
  # .........................................................


  # ==========================================
  # Fix Survival - cannot be 100%
  # ==========================================
  surv_100 <- which(life_cycles$Name %in% paste0("surv_", 1:1000) & life_cycles$Value == 1)
  if(length(surv_100) > 0) {
    life_cycles$Value[surv_100] <- 0.999999
  }
  # .........................................................

  # =======================================================
  # Fix Survival - cannot be 0% in the middle of the chain
  # =======================================================
  surv_0 <- which(life_cycles$Name %in% paste0("surv_", 1:1000) & life_cycles$Value == 0)
  if(length(surv_0) > 1) {
    surv_0_index <- surv_0[1:(length(surv_0) - 1)]
    life_cycles$Value[surv_0_index] <- 0.001
    possible_error_state <- "Cannot have 0 survival in intermediate life stage"
  }




  # Rename to match reference code
  life_pars <- life_cycles
  row.names(life_pars) <- life_pars$Name

  Nstage <- life_pars["Nstage", "Value"]

  Nstage <- as.numeric(Nstage)

  stage_names <- paste("stage", 1:Nstage, sep = "_")

  survival <-
    life_pars[match(paste("surv", 1:Nstage, sep = "_"), life_pars$Name), "Value"]
  years <-
    life_pars[match(paste("year", 1:Nstage, sep = "_"), life_pars$Name), "Value"]
  cr <-
    life_pars[match(paste("cr", 1:Nstage, sep = "_"), life_pars$Name), "Value"]

  survival <- as.numeric(survival)
  years <- as.numeric(years)
  cr <- as.numeric(cr)


  if (any((cr * survival) > 1)) {
    message("compensation ratios too high")
    possible_error_state <- "compensation ratios too high"
  }

  # Sexual maturity vector
  mat <-
    life_pars[match(paste("mat", 1:Nstage, sep = "_"), life_pars$Name), "Value"]

  mat <- as.numeric(mat)


  names(survival) <- names(years) <- names(cr) <- stage_names

  # Equilibrium adults population K
  N0 <- life_pars["k", "Value"]

  N0 <- as.numeric(N0)

  # Sensitivity to equilibrium
  sens <- life_pars["sens", "Value"]

  # Build matrix
  life_stages <- matrix(0, nrow = Nstage, ncol = Nstage)

  # Stay in current stage or transition to next
  diag(life_stages) <- paste("pr", 1:Nstage, sep = "_")

  pos <- Nstage * (1:Nstage) + (1:Nstage)

  life_stages[pos] <- paste("tr", 1:Nstage, sep = "_")

  # Note in above line there will always be transitions beyond limit
  # MJB added to prevent error messages
  life_stages <- life_stages[1:(Nstage * Nstage)]

  life_stages <- matrix(life_stages,
           nrow = Nstage,
           ncol = Nstage,
           byrow = TRUE)

  # identify the reproductive stage
  life_stages[1, which(mat > 0)] <-
    "fec"

  life_stages_symbols <- matrix(0, nrow = Nstage, ncol = Nstage)

  diag(life_stages_symbols) <-
    paste(
      "s",
      1:Nstage,
      "*(1-",
      "s",
      1:Nstage,
      "^(",
      "n",
      1:Nstage,
      "-1))/(1-",
      "s",
      1:Nstage,
      "^",
      "n",
      1:Nstage,
      ")",
      sep = ""
    )

  for (i in 1:Nstage)
  {
    life_stages_symbols[pmin(i + 1, Nstage), i] <-
      paste(
        "s",
        i,
        "-",
        "s",
        i,
        "*(1-",
        "s",
        i,
        "^(",
        "n",
        i,
        "-1))/(1-",
        "s",
        i,
        "^",
        "n",
        i,
        ")",
        sep = ""
      )
  }

  # identify the reproductive stages
  for (i in 2:Nstage)
  {
    life_stages_symbols[1, i] <-
      paste("(mat", i, " * events * eps * sE * s0 * sR)/int", sep = "")
  }

  diag(life_stages_symbols) <-
    paste(
      "s",
      1:Nstage,
      "*(1-",
      "s",
      1:Nstage,
      "^(",
      "n",
      1:Nstage,
      "-1))/(1-",
      "s",
      1:Nstage,
      "^",
      "n",
      1:Nstage,
      ")",
      sep = ""
    )

  #life_stages_symbols <- as.data.frame(life_stages_symbols)
  temp_symbol <- unlist(t(life_stages_symbols))

  life_stages_symbolic <-
    unlist(sapply(1:length(temp_symbol), function(x) {
      parse(text = temp_symbol[x])
    }))

  # density dependent symbols
  density_stage_symbols <- matrix(1, nrow = Nstage, ncol = Nstage)

  diag(density_stage_symbols) <- paste("s", 1:Nstage, sep = "")
  for (i in 1:Nstage)
  {
    density_stage_symbols[pmin(i + 1, Nstage), i] <-
      paste("s", i, sep = "")
  }

  # identify the reproductive stage
  density_stage_symbols[1, 2:Nstage] <- "sE * s0"
  diag(density_stage_symbols) <- paste("s", 1:Nstage, sep = "")


  temp_dens_symbol <- unlist(t(density_stage_symbols))

  density_stage_symbolic <-
    unlist(sapply(1:length(temp_dens_symbol), function(x) {
      parse(text = temp_dens_symbol[x])
    }))

  # stressor response symbols
  stressor_stage_symbols <- matrix(1, nrow = Nstage, ncol = Nstage)
  diag(stressor_stage_symbols) <- paste("s", 1:Nstage, sep = "")

  for (i in 1:Nstage)
  {
    stressor_stage_symbols[pmin(i + 1, Nstage), i] <-
      paste("s", i, sep = "")
  }

  # identify the reproductive stage
  stressor_stage_symbols[1, 2:Nstage] <- "sE * s0"

  diag(stressor_stage_symbols) <- paste("s", 1:Nstage, sep = "")

  #life_stages_symbols <- as.data.frame(life_stages_symbols)
  temp_stress_symbol <- unlist(t(stressor_stage_symbols))

  stressor_stage_symbolic <-
    unlist(sapply(1:length(temp_stress_symbol), function(x) {
      parse(text = temp_stress_symbol[x])
    }))

  varNames <- c("sE", "s0", paste("s", 1:Nstage, sep = ""))



  # -------------------------------------------------
  # parameterizing matrices.R
  # -------------------------------------------------

  # calculate stage-specific survivals and transition rates
  probs <- stage_probs(survival, years)

  # Probability of staying within existing stage
  # MJB added Nov 19 2024 - one year in the last stage
  probs <- ifelse(is.na(probs), 0, probs)

  tr_prob <- survival - probs

  life_cycle <- matrix(0, Nstage, Nstage)

  # survival probabilities
  msub <- life_stages[grepl("pr", life_stages)]
  msub <- gsub("pr_", "stage_", msub)
  probs_sub_in <- probs[msub]

  life_cycle[grep("pr", life_stages)] <- probs_sub_in # probs

  # transition probabilities
  life_cycle[grep("tr", life_stages)] <- tr_prob[-Nstage]

  # Create numeric copy
  lpn <- life_pars

  suppressWarnings({ lpn$Value <- as.numeric(lpn$Value) })

  life_cycle[grep("fec", life_stages)] <-
    mat[which(mat > 0)] * lpn["events", "Value"] * lpn["eps", "Value"] *
    lpn["SE", "Value"] * lpn["S0", "Value"] * lpn["SR", "Value"] /
    lpn["int", "Value"]

  # initialize the survival/transition rates subject to a stage-specific density-dependence
  d_mat <- matrix(1, nrow = Nstage, Nstage)
  d_stage <- matrix(0, Nstage, Nstage)
  d_stage[1, which(mat > 0)] <- 1

  for (i in 2:Nstage)
  {
    d_stage[i, (i - 1):i] <- i
  }

  # create dataset for life-histories
  life_histories <- list()

  # transition probabilities
  life_histories$Nstage <- Nstage
  names(life_histories$Nstage) <- "Nstage"

  life_histories$nYrs <- years
  names(life_histories$nYrs) <- paste("n", 1:Nstage, sep = "")

  # egg-survival, age-0 survival, and then stage-specific survival probabilities
  life_histories$S <-
    c(lpn["SE", "Value"], lpn["S0", "Value"], survival)

  names(life_histories$S) <- c("sE", "s0", paste("s", 1:Nstage, sep = ""))

  life_histories$Surv_annual <-
    c(lpn["SE", "Value"], lpn["S0", "Value"], survival)

  names(life_histories$Surv_annual) <-
    c("sE", "s0", paste("s", 1:Nstage, sep = ""))

  # maturity vector for stages 2+
  life_histories$mat <- mat[-1]
  names(life_histories$mat) <- paste("mat", 2:Nstage, sep = "")

  # spawning events per year
  life_histories$events <-
    lpn["events", "Value"]

  names(life_histories$events) <- "events"

  # eggs per spawner
  life_histories$eps <- lpn["eps", "Value"]

  names(life_histories$eps) <- "eps"

  # spawning intervals
  life_histories$int <- lpn["int", "Value"]
  names(life_histories$int) <- "int"

  # adult carrying capacity
  life_histories$Ka <- N0
  names(life_histories$Ka) <- "Ka"

  # sex ratio
  life_histories$sR <- lpn["SR", "Value"]
  names(life_histories$sR) <- "sR"



  #--------------------------------------------
  # Build return object
  #--------------------------------------------
  ret_obj <- list()

  # Transition Matrix
  ret_obj$anadromous <- FALSE

  ret_obj$projection_matrix <- life_cycle
  ret_obj$projection_matrix_symbol <- life_stages_symbols
  ret_obj$projection_matrix_txt <- life_stages_symbols

  # Life Histories
  ret_obj$life_histories <- life_histories
  ret_obj$life_stages_symbolic <- life_stages_symbolic
  ret_obj$life_pars <- life_pars
  ret_obj$Nstage <- Nstage
  ret_obj$density_stage_symbolic <- density_stage_symbolic
  ret_obj$possible_error_state <- possible_error_state
  ret_obj$spanwing_years <- NULL


  return(ret_obj)


}
