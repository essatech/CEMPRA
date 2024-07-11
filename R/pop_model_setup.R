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

  #------------------------------------------------------------
  # Determine if anadromous transformations need to be applied
  #------------------------------------------------------------
  if(!("anadromous" %in% life_cycles$Name)) {
    # if anadromous is not specifed assume false
    anadromous <- FALSE
  } else {
    anadromous <- life_cycles$Value[life_cycles$Name == "anadromous"]
    if(anadromous == "TRUE") {
      anadromous <- TRUE
    } else {
      if(isTRUE(anadromous)) {
        anadromous <- TRUE # if someone typed "No"
      } else {
        anadromous <- FALSE
      }
    }
  }

  # Re-strcuture for anadromous model
  if(anadromous) {
    # Call pop_model_setup_anadromous() and exit this function
    life_cycles$Value[life_cycles$Name == "anadromous"] <- TRUE
    ret_obj <- pop_model_setup_anadromous(life_cycles = life_cycles)
    return(ret_obj)
  }




  # Rename to match reference code
  life_pars <- life_cycles
  row.names(life_pars) <- life_pars$Name
  Nstage <- life_pars["Nstage", "Value"]

  #if(Nstage != 4) {
    # possible_error_state <- "Model only accepts 4 unique stages"
  #}

  stage_names <- paste("stage", 1:Nstage, sep = "_")

  survival <-
    life_pars[match(paste("surv", 1:Nstage, sep = "_"), life_pars$Name), "Value"]
  years <-
    life_pars[match(paste("year", 1:Nstage, sep = "_"), life_pars$Name), "Value"]
  cr <-
    life_pars[match(paste("cr", 1:Nstage, sep = "_"), life_pars$Name), "Value"]

  if (any((cr * survival) > 1)) {
    print("compensation ratios too high")
    possible_error_state <- "compensation ratios too high"
  }

  # Sexual maturity vector
  mat <-
    life_pars[match(paste("mat", 1:Nstage, sep = "_"), life_pars$Name), "Value"]

  names(survival) <- names(years) <- names(cr) <- stage_names

  # Equilibrium adults population K
  N0 <- life_pars["k", "Value"]

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
      paste("(mat", i, "* events * eps * sE * s0 * sR)/int", sep = "")
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
  tr_prob <- survival - probs
  life_cycle <- matrix(0, Nstage, Nstage)

  # survival probabilities
  life_cycle[grep("pr", life_stages)] <- probs

  # transition probabilities
  life_cycle[grep("tr", life_stages)] <- tr_prob[-Nstage]

  life_cycle[grep("fec", life_stages)] <-
    mat[which(mat > 0)] * life_pars["events", "Value"] * life_pars["eps", "Value"] *
    life_pars["SE", "Value"] * life_pars["S0", "Value"] * life_pars["SR", "Value"] /
    life_pars["int", "Value"]

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
    c(life_pars["SE", "Value"], life_pars["S0", "Value"], survival)

  names(life_histories$S) <- c("sE", "s0", paste("s", 1:Nstage, sep = ""))

  life_histories$Surv_annual <-
    c(life_pars["SE", "Value"], life_pars["S0", "Value"], survival)

  names(life_histories$Surv_annual) <-
    c("sE", "s0", paste("s", 1:Nstage, sep = ""))

  # maturity vector for stages 2+
  life_histories$mat <- mat[-1]
  names(life_histories$mat) <- paste("mat", 2:Nstage, sep = "")

  # spawning events per year
  life_histories$events <-
    life_pars["events", "Value"]

  names(life_histories$events) <- "events"

  # eggs per spawner
  life_histories$eps <- life_pars["eps", "Value"]

  names(life_histories$eps) <- "eps"

  # spawning intervals
  life_histories$int <- life_pars["int", "Value"]
  names(life_histories$int) <- "int"

  # adult carrying capacity
  life_histories$Ka <- N0
  names(life_histories$Ka) <- "Ka"

  # sex ratio
  life_histories$sR <- life_pars["SR", "Value"]
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
