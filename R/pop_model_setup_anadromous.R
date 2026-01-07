#' Population Model Setup for Anadromous Species
#'
#' @description Generate symbolic objects for the population model and
#' then generate parameter matrices for Pacific salmon.
#'
#' @details This is an intermediate setup function to run the population,
#' but some of its outputs are useful on their own, especially for eigen
#' analyses from the projection matrix. pop_model_setup_anadromous() is run
#' before pop_model_matrix_elements() and Projection_DD().
#'
#' pop_model_setup_anadromous() is a customized version of pop_model_setup() for
#' anadromous species (Salmon). Follow convention from Davison and Statterthwaite
#' 2016 (Natural Resource Modelling Use of matrix model to predict life history
#' schedules for semelparous population).
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
#' @importFrom utils tail
#' @export
#'
pop_model_setup_anadromous <- function(life_cycles = NA) {

  # Are there problems with the input parameters
  possible_error_state <- "All Good"

  # Load the species life cycles traits
  # must have N number of survival, years, and compensation ratios

  # print("Running... pop_model_setup_anadromous()")

  #------------------------------------------------------------
  # Determine if anadromous transformations need to be applied
  #------------------------------------------------------------
  if(!("anadromous" %in% life_cycles$Name)) {
    # if anadromous is not specifed assume false
    anadromous <- FALSE
  } else {
    anadromous <- life_cycles$Value[life_cycles$Name == "anadromous"]
    anadromous <- as.logical(anadromous)
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
  if (!(anadromous)) {
    stop("Use pop_model_setup() instead of pop_model_setup_anadromous() for non-anadromous species...")
  }


  # ==========================================
  # Fix Survival - cannot be 100%
  # ==========================================
  surv_100 <- which(life_cycles$Name %in% paste0("surv_", 1:1000) & life_cycles$Value == 1)
  if(length(surv_100) > 0) {
    life_cycles$Value[surv_100] <- 0.999999
  }
  # .........................................................


  # ============================================================
  # Fix Survival - cannot be 0% surv in the middle of the chain
  # ============================================================
  surv_fix <- life_cycles[life_cycles$Name %in% paste0("surv_", 1:1000), ]
  surv_fix <- surv_fix[order(surv_fix$Name), ]

  eps_check <- life_cycles[life_cycles$Name %in% paste0("eps_", 1:1000), ]
  eps_check <- eps_check[order(eps_check$Name), ]

  # Find max repo age - based on non-zero eps
  # May not work if older format or only single eps defined.
  max_repo_age <- suppressWarnings({ max(as.numeric(gsub("eps_", "", eps_check$Name[eps_check$Value > 0]))) })

  surv_0 <- surv_fix$Name[surv_fix$Value == 0]

  # More than one zero Pb endpoint
  if(length(surv_0) > 1) {
    # Fix surv zeros in the life stage chain
    # Set surv_x where x < max_repo_age to 0.00001
    for(k in 1:length(surv_0)) {
      if(as.numeric(gsub("surv_", "", surv_0[k])) < max_repo_age) {
        life_cycles$Value[life_cycles$Name == surv_0[k]] <- 0.00001
      }
    }
    possible_error_state <- "Cannot have 0 survival in intermediate life stage"
  }

  # ==============================================================
  # Drop extra 0 placeholder values in the life cycle params file
  # ==============================================================

  # Rename to match reference code
  life_pars <- life_cycles

  # Assign row names
  row.names(life_pars) <- life_pars$Name
  # Ensure life_pars value is numeric
  life_pars$Value <- suppressWarnings({
    as.numeric(as.character(life_pars$Value))
  })
  # Make sure anadromous field is still set to 1.0
  life_pars[life_pars$Name == "anadromous", "Value"] <- 1.0

  # Determine Nstage from non-zero survival
  Nstage <- sum(life_pars[grepl("^surv_", rownames(life_pars)), "Value"] > 0)

  # Get the mature stage classes
  mature_spawners <- life_pars[grepl("mat_", rownames(life_pars)), ]
  # Filter for any non-zero classes (probability of maturity greater than 0)
  mature_spawners <- mature_spawners[mature_spawners$Value > 0, ]

  # Extend Nstage to create a vector of pre-breeder and breeder stage
  # total stage class count will increase by number of spawner year stages

  # Follow convention from Davison and Statterthwaite 2016: Natural Resource Modelling
  # Use of matrix model to predict life history schedules for semelparous population

  #Nstage for pre-breeders (Nstage_B)
  Nstage_Pb <- Nstage

  # Nstage for breeders (Nstage_B)
  Nstage_B <- Nstage + nrow(mature_spawners)

  # New Nstage (total)
  Nstage <- Nstage + nrow(mature_spawners)

  # Age of earliest spawner
  spanwing_years <- as.numeric(sapply(strsplit(mature_spawners$Name, "_"), `[`, 2))
  # First year of spawning
  first_year_spawning <- min(spanwing_years)

  # Stage name vector is then Pb followed by B
  stage_names_Pb <- c(paste("stage", "Pb", 1:Nstage_Pb, sep = "_"))
  stage_names_B <- c(paste("stage", "B", spanwing_years, sep = "_"))
  stage_names <- c(stage_names_Pb, stage_names_B)

  # stage_names <- paste("stage", 1:Nstage, sep = "_") # MJB CHECK...

  survival <-
    life_pars[match(paste("surv", 1:Nstage_Pb, sep = "_"), life_pars$Name), "Value"]

  years <-
    life_pars[match(paste("year", 1:Nstage_Pb, sep = "_"), life_pars$Name), "Value"]
  years <- ifelse(is.na(years), 1, years)

  cr <-
    life_pars[match(paste("cr", 1:Nstage_Pb, sep = "_"), life_pars$Name), "Value"]

  if (any((cr * survival) > 1)) {
    message("compensation ratios too high")
    possible_error_state <- "compensation ratios too high"
  }


  # Sexual maturity vector
  mat <-
    life_pars[match(paste("mat", spanwing_years, sep = "_"), life_pars$Name), "Value"]

  names(survival) <- names(years) <- names(cr) <- stage_names_Pb

  # Equilibrium adults population K
  N0 <- life_pars["k", "Value"]

  # Sensitivity to equilibrium
  sens <- life_pars["sens", "Value"]

  # =========================================================
  #  Build matrix ~ Assume only one year spent in each stage
  # =========================================================

  life_stages <- matrix(0,
                        nrow = Nstage,
                        ncol = Nstage,
                        byrow = TRUE)

  # Stay in current stage or transition to next
  diag(life_stages) <- paste("pr", 1:Nstage, sep = "_")

  # Transition to next stage
  pos <- get_transition_positions(Nstage)

  # Nstage - 1 assignment to prevent projection beyond last stage
  life_stages[pos] <- paste("tr", 1:(Nstage - 1), sep = "_")


  # identify the reproductive B stage
  life_stages[1, grepl("_B_", stage_names)] <-
    "fec"

  rownames(life_stages) <- stage_names
  colnames(life_stages) <- stage_names

  # -------------------------------------------------
  # Build symbolic matrix
  # -------------------------------------------------

  life_stages_symbols <- matrix(0, nrow = Nstage, ncol = Nstage)

  rownames(life_stages_symbols) <- stage_names
  colnames(life_stages_symbols) <- stage_names


  # Diag is zero because age-stage matrix
  # Must advance to next stage, mature, or die
  diag(life_stages_symbols) <- 0

  # Fill in transition symbols
  for (i in 1:Nstage)
  {
    # determine if portion mature to spawners at end
    # of stage class. For example, stage2 to stage3 survival
    # will be split to individuals that mature to stage3 spawners
    # and those that mature to regular stage3 (non-mature individuals)
    # ... and remember we are doing prebirth pulse census.

    if ((i + 1) %in% spanwing_years) {
      # mature to spawners or survive next stage
      # remember to offset mat by 1 year (moving into spawn year)
      cell_obj <- paste0("s", i, "*(1-", "mat", i + 1, ")", sep = "")

      # Build transition to spawning class
      cell_obj_s <- paste0("s", i, "*mat", i + 1, "*smig", i + 1, sep = "")

    } else {
      # simple transition to next stage
      cell_obj <- paste0("s", i)
      cell_obj_s <- "0"
    }

    # Fix terminal spawner stage # last stage must spawn
    if (i >= Nstage_Pb) {
      cell_obj <- "0"
    }

    # Add symbol for survival progression
    life_stages_symbols[pmin(i + 1, Nstage), i] <- cell_obj

    # Add symbol for maturation progression
    life_stages_symbols[grepl(paste0("_B_", (i + 1)), stage_names), i] <- cell_obj_s
    #####
  }

  # identify the reproductive stages
  for (i in 1:Nstage)
  {
    # offset the repo stage id
    param_ref <- sapply(strsplit(stage_names[i], "_"), utils::tail, 1)

    if (grepl("_B_", stage_names[i])) {
      life_stages_symbols[1, i] <-
        paste("(u",
              param_ref,
              " * events * eps",
              param_ref,
              " * sE * s0 * sR)/int",
              sep = "")
    }
  }

  # enforce stage transition
  diag(life_stages_symbols) <- "0"


  # life_stages_symbols <- as.data.frame(life_stages_symbols)
  temp_symbol <- unlist(t(life_stages_symbols))

  life_stages_symbolic <-
    unlist(sapply(1:length(temp_symbol), function(x) {
      parse(text = temp_symbol[x])
    }))

  life_stages_symbols

  # ===============================------------
  # density dependent symbols -- MJB TODO FIX
  # ===============================------------

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


  # Need to replace extra classes with spawning calsses (b)
  # s5, s6, s7 becomes b3, b4, b5

  for (i in 1:length(stage_names)) {
    if (grepl("_B_", stage_names[i])) {
      param_ref <- sapply(strsplit(stage_names[i], "_"), tail, 1)
      density_stage_symbols <- gsub(paste0("s", i),
                                    paste0("b", param_ref),
                                    density_stage_symbols)
    }
  }

  temp_dens_symbol <- unlist(t(density_stage_symbols))

  density_stage_symbolic <-
    unlist(sapply(1:length(temp_dens_symbol), function(x) {
      parse(text = temp_dens_symbol[x])
    }))

  # ----------------------------
  # stressor response symbols
  # ----------------------------
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

  # Need to replace extra classes with spawning calsses (b)
  # s5, s6, s7 becomes b3, b4, b5

  for (i in 1:length(stage_names)) {
    if (grepl("_B_", stage_names[i])) {
      param_ref <- sapply(strsplit(stage_names[i], "_"), tail, 1)
      stressor_stage_symbols <- gsub(paste0("s", i),
                                     paste0("b", param_ref),
                                     stressor_stage_symbols)
    }
  }


  #life_stages_symbols <- as.data.frame(life_stages_symbols)
  temp_stress_symbol <- unlist(t(stressor_stage_symbols))

  stressor_stage_symbolic <-
    unlist(sapply(1:length(temp_stress_symbol), function(x) {
      parse(text = temp_stress_symbol[x])
    }))

  varNames <- c("sE",
                "s0",
                paste("s", 1:Nstage_Pb, sep = ""),
                paste("b", spanwing_years, sep = ""))



  # ------------------------------------------------------
  # Convert transition matrix from symbolsto numeric data
  # ------------------------------------------------------

  # older code from parameterizing matrices.R

  # calculate stage-specific survivals and transition rates
  years <- ifelse(is.na(years), 1, years)

  probs <- stage_probs(survival, years)

  # MJB added Nov 19 2024 - one year in the last stage
  probs <- ifelse(is.na(probs), 0, probs)

  tr_prob <- survival - probs
  life_cycle <- matrix(0, Nstage, Nstage)

  # survival probabilities - stay within same stage
  # life_cycle[grep("pr", life_stages)] <- probs


  # Replace all elemets in life_stages_symbols
  # with numeric values from life_pars
  life_stages_numeric <- life_stages_symbols

  for (i in 1:Nstage) {
    # Assume age transition survival is 1.0 if unspecified
    new_val <- life_pars[paste0("surv_", i), "Value"]
    new_val <- ifelse(is.na(new_val), 1, new_val)
    life_stages_numeric <- gsub(paste0("^s", i), new_val, life_stages_numeric)

    # Migratio term must always be specified
    life_stages_numeric <- gsub(paste0("mat", i), life_pars[paste0("mat_", i), "Value"], life_stages_numeric)

    # Assume migration survival is 1.0 if unspecified
    new_val <- life_pars[paste0("smig_", i), "Value"]
    new_val <- ifelse(is.na(new_val), life_pars[paste0("smig", i), "Value"], new_val)
    new_val <- ifelse(is.na(new_val), 1, new_val)
    life_stages_numeric <- gsub(paste0("smig", i), new_val, life_stages_numeric)

    # Assume prespawn survival is 1.0 if unspecified
    new_val <- life_pars[paste0("u_", i), "Value"]
    new_val <- ifelse(is.na(new_val), life_pars[paste0("u", i), "Value"], new_val)
    new_val <- ifelse(is.na(new_val), 1, new_val)
    life_stages_numeric <- gsub(paste0("u", i), new_val, life_stages_numeric)

    # Assume eps_x is just eps if unspecified
    new_val <- life_pars[paste0("eps_", i), "Value"]
    new_val <- ifelse(is.na(new_val), life_pars[paste0("eps", i), "Value"], new_val)
    new_val <- ifelse(is.na(new_val), life_pars["eps", "Value"], new_val)
    life_stages_numeric <- gsub(paste0("eps", i), new_val, life_stages_numeric)

    if (any(is.na(life_stages_numeric))) {
      warnings(paste0("Missing parameter for stage ", i))
      possible_error_state <- paste0("Missing parameter for stage ", i)
    }

  }

  # Replace static reproductive values
  # spawning events per year
  new_val <- life_pars["events", "Value"]
  new_val <- ifelse(is.na(new_val), 1, new_val)
  life_stages_numeric <- gsub("events", new_val, life_stages_numeric)

  # egg survival
  new_val <- life_pars["SE", "Value"]
  new_val <- ifelse(is.na(new_val), 1, new_val)
  life_stages_numeric <- gsub("sE", new_val, life_stages_numeric)

  # yearling survival
  new_val <- life_pars["S0", "Value"]
  new_val <- ifelse(is.na(new_val), life_pars["s0", "Value"], new_val)
  new_val <- ifelse(is.na(new_val), 1, new_val)
  life_stages_numeric <- gsub("s0", new_val, life_stages_numeric)

  # sex ratio
  new_val <- life_pars["SR", "Value"]
  new_val <- ifelse(is.na(new_val), life_pars["sR", "Value"], new_val)
  new_val <- ifelse(is.na(new_val), 1, new_val)
  life_stages_numeric <- gsub("sR", new_val, life_stages_numeric)

  # spawning interval per year
  new_val <- life_pars["int", "Value"]
  new_val <- ifelse(is.na(new_val), life_pars["int", "Value"], new_val)
  new_val <- ifelse(is.na(new_val), 1, new_val)
  life_stages_numeric <- gsub("int", new_val, life_stages_numeric)

  # save as text object for reference
  life_stages_numeric_txt <- life_stages_numeric

  # Parse and evalaute text values
  evaluate_expression <- function(expr) {
    eval(parse(text = expr))
  }
  life_cycle <- apply(life_stages_numeric, c(1, 2), evaluate_expression)



  # initialize the survival/transition rates subject to a stage-specific density-dependence
  d_mat <- matrix(1, nrow = Nstage, Nstage)
  d_stage <- matrix(0, Nstage, Nstage)
  d_stage[1, which(mat > 0)] <- 1

  for (i in 2:Nstage)
  {
    d_stage[i, (i - 1):i] <- i
  }

  # ====================================
  # Create final dataset for life-histories
  # ====================================
  life_histories <- list()

  # transition probabilities
  life_histories$Nstage <- Nstage

  names(life_histories$Nstage) <- "Nstage"

  life_histories$Nstage_Pb <- Nstage_Pb
  life_histories$Nstage_B <- Nstage_B

  life_histories$stage_names <- stage_names

  life_histories$nYrs <- years
  names(life_histories$nYrs) <- paste("n", 1:Nstage_Pb, sep = "")

  # egg-survival, age-0 survival, and then stage-specific survival probabilities
  life_histories$S <-
    c(life_pars["SE", "Value"], life_pars["S0", "Value"], survival)

  names(life_histories$S) <- c("sE", "s0", paste("s", 1:Nstage_Pb, sep = ""))

  life_histories$Surv_annual <-
    c(life_pars["SE", "Value"], life_pars["S0", "Value"], survival)

  names(life_histories$Surv_annual) <-
    c("sE", "s0", paste("s", 1:Nstage_Pb, sep = ""))

  # maturity vector for stages 2+
  life_histories$mat <- mat
  names(life_histories$mat) <- paste("mat", spanwing_years, sep = "")

  # spawning events per year
  life_histories$events <-
    life_pars["events", "Value"]

  names(life_histories$events) <- "events"

  # eggs per spawner
  life_histories$eps <- life_pars[paste("eps_", spanwing_years, sep = ""), "Value"]
  # eps is onlye defined as a global fixed value
  if(any(is.na(life_histories$eps))) {
    # eps not defined for specific life stage
    life_histories$eps[which(is.na(life_histories$eps))] <- life_pars["eps", "Value"]
  }
  names(life_histories$eps) <- paste("eps", spanwing_years, sep = "")

  # spawning intervals
  life_histories$int <- life_pars["int", "Value"]
  names(life_histories$int) <- "int"

  # adult carrying capacity
  life_histories$Ka <- N0
  names(life_histories$Ka) <- "Ka"

  # sex ratio
  life_histories$sR <- life_pars["SR", "Value"]
  names(life_histories$sR) <- "sR"

  # prespawn mortality
  life_histories$u <- life_pars[paste("u_", spanwing_years, sep = ""), "Value"]
  names(life_histories$u) <- paste("u", spanwing_years, sep = "")

  # spawner migration mortality
  life_histories$smig <- life_pars[paste("smig_", spanwing_years, sep = ""), "Value"]
  names(life_histories$smig) <- paste("smig", spanwing_years, sep = "")



  #--------------------------------------------
  # Build return object
  #--------------------------------------------
  ret_obj <- list()

  # Transition Matrix
  ret_obj$anadromous <- TRUE

  ret_obj$projection_matrix <- life_cycle
  ret_obj$projection_matrix_symbol <- life_stages_symbols
  ret_obj$projection_matrix_txt <- life_stages_numeric_txt

  # Life Histories
  ret_obj$life_histories <- life_histories
  ret_obj$life_stages_symbolic <- life_stages_symbolic
  ret_obj$life_pars <- life_pars
  ret_obj$Nstage <- Nstage
  ret_obj$density_stage_symbolic <- density_stage_symbolic
  ret_obj$possible_error_state <- possible_error_state
  ret_obj$spanwing_years <- spanwing_years


  return(ret_obj)


}
