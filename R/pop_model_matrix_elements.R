#'Population Model Matrix Elements
#'
#' @description Build population model matrix elements.
#'
#' @details This is an intermediate setup function to run the population model. However, some of outputs are useful on their own, especially for eigen analyses. pop_model_matrix_elements() is run after pop_model_setup() or pop_model_setup_anadromous(), but pop_model_matrix_elements() must be run before Projection_DD(). Key outputs to explore include a density independent projection matrix, life histories, and a symbolic representation of the matrix math for density dependent and density independent components.
#'
#' @param pop_mod_setup List. Object returned from population_model_setup().
#' @param s0_calibrate Logical. If TRUE (default) and an adult capacity `k`
#'  is set (with no Beverton-Holt flags), fry survival (s0) is re-calibrated
#'  so that lambda = 1. Set to FALSE to keep the raw s0 from the life cycles
#'  file - used by sensitivity analyses (e.g., pop_model_ltre) that must
#'  measure lambda responses on the un-calibrated matrix.
#'
#' @returns a list object of symbolic objects.
#'
#'@examples
#'\dontrun{
#'
#' # Load the CEMPRA package.
#' library(CEMPRA)
#'
#' filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
#' life_cycles <- read.csv(filename_lc)
#'
#' # Setup objects for population model
#' pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
#'
#' # Build matrix elements for population model
#' pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
#' names(pop_mod_mat)
#'
#' # A density independent projection matrix
#' pop_mod_mat$projection_matrix
#'
#'
#'
#' }
#'
#' @export
#'
pop_model_matrix_elements <- function(pop_mod_setup = NA, s0_calibrate = TRUE) {

  # Detrmine if we should use pop_model_matrix_elements_anadromous()
  # instead of pop_model_matrix_elements()
  if(pop_mod_setup$anadromous) {
    ret_obj <- pop_model_matrix_elements_anadromous(pop_mod_setup,
                                                    s0_calibrate = s0_calibrate)
    return(ret_obj)
  }


  life_histories <- pop_mod_setup$life_histories
  life_stages_symbolic <- pop_mod_setup$life_stages_symbolic
  life_pars <- pop_mod_setup$life_pars
  Nstage <- pop_mod_setup$Nstage
  density_stage_symbolic <- pop_mod_setup$density_stage_symbolic




  Nyears <- 10 * sum(life_histories$nYrs)


  # MJB Nov 19, 2024 Check for issue with one year and surv 1
  # Check for issue where survivorship is 1.0 and years in stage is 1.0
  bad_match <- life_histories$S[paste0("s", 1:pop_mod_setup$Nstage)] == 1
  # NA-safe (blank survival cells) and name-based so NA entries cannot
  # shift the assignment onto the wrong stage
  bad_match <- bad_match[!(is.na(bad_match))]
  if(any(bad_match)) {
    life_histories$S[names(bad_match)[bad_match]] <- 0.999999
  }


  # Build the projection matrix
  pmx.det <-
    pmx_eval(
      life_stages_symbolic,
      c(
        life_histories,
        life_histories$S,
        life_histories$nYrs,
        life_histories$mat,
        life_histories$eps
      )
    )

  # Set NA values to zero for final transition stage - Nov 19th 2024
  if(is.na(pmx.det[nrow(pmx.det), ncol(pmx.det)])) {
    pmx.det[nrow(pmx.det), ncol(pmx.det)] <- 0
  }




  # Initialize populations and find carrying capacity for each life stage
  # Carrying capacity is defined at the adult stage and
  # estimated for all other life-stages using the stable-stage distribution
  # (i.e., if the carrying capacity of the adult stage was set to 100 the
  # carrying capacity for the hatchling (fry) stage is the amount of hatchlings
  # necessary to result in k (e.g., 100) adults at equilibrium conditions)

  # replaced with try catch - gives error with high suv...
  if (s0_calibrate) {
    gets0 <- tryCatch(
      {
        stats::optimize(
            s0_optim.f,
            interval = c(0, 1),
            tol = 1e-16,
            mx = life_stages_symbolic,
            dat = life_histories,
            target.lambda = 1
          )$minimum
      },
      error=function(cond) {
        life_histories$S["s0"]
      }
    )
  } else {
    # Sensitivity-analysis mode: keep the raw fry survival so lambda
    # responds to vital-rate perturbations instead of being re-tuned to 1
    gets0 <- as.numeric(life_histories$S["s0"])
  }

  s0_optim_min <- gets0


  # s0_optim_min <-
  #   stats::optimize(
  #     s0_optim.f,
  #     interval = c(0, 1),
  #     tol = 1e-16,
  #     mx = life_stages_symbolic,
  #     dat = life_histories,
  #     target.lambda = 1
  #   )$minimum


  life_histories$s0.1.det <- s0_optim_min


  # MJB override: If adult k is NA or NULL then keep s0.1.det
  # at original fry survivorship
  adult_k <- life_pars$Value[life_pars$Name == "k"]
  adult_k <- as.numeric(adult_k)

  if(length(adult_k) == 0) {
    # Cant find
    life_histories$s0.1.det <- as.numeric(life_histories$S["s0"])
  } else {
    if(is.na(adult_k)) {
      # Set to NA
      life_histories$s0.1.det <- as.numeric(life_histories$S["s0"])
    }
  }
  # MJB: end of density-independent growth

  # If any BH/HS density-dependence flag is ENABLED (any spelling, any
  # life stage) the run is in habitat-capacity mode and s0 keeps its raw
  # file value. Only a run truly in compensation-ratio mode (no enabled
  # flags) gets the lambda = 1 calibration so the stated adult k is the
  # equilibrium. Note this checks flag VALUES via dd_flag_enabled() - a
  # profile whose flag rows are all 0/blank runs in CR mode and IS
  # calibrated (previously row presence alone skipped the calibration,
  # silently letting CR runs equilibrate ~2.7x above the stated k).
  if (any_dd_flag_enabled(life_pars)) {
    life_histories$s0.1.det <- as.numeric(life_histories$S["s0"])
  }

  # Add YOY survival to survival rate vector
  life_histories$S["s0"] <- life_histories$s0.1.det

  M.1.pmx <- {
    pmx <-
      pmx_eval(
        life_stages_symbolic,
        c(
          life_histories,
          life_histories$S,
          life_histories$nYrs,
          life_histories$mat,
          life_histories$eps
        )
      )
  }

  # life_histories

  # Set NA values to zero for final transition stage - Nov 19th 2024
  if(is.na(M.1.pmx[nrow(M.1.pmx), ncol(M.1.pmx)])) {
    M.1.pmx[nrow(M.1.pmx), ncol(M.1.pmx)] <- 0
  }


  # isTRUE(): NA-safe - an NA matrix cell must not crash this check
  if (!isTRUE(all(pop_mod_setup$projection_matrix == M.1.pmx))) {
    message(
      "Running with S0 adjusted to s0.1.det..."
    )
  }


  # Replace any missing NA values with 1
  M.1.pmx <- ifelse(is.na(M.1.pmx), 1, M.1.pmx)


  life_histories$gen.time <- popbio::generation.time(M.1.pmx)
  names(life_histories$gen.time) <- "gen.time"

  # variation in eggs per spawner
  life_histories$eps_sd <- as.numeric(life_pars["eps_sd", "Value"])
  names(life_histories$eps_sd) <- "eps_sd"
  if (is.na(life_histories$eps_sd)) {
    message(
      "eps_sd is missing or blank: fecundity will not vary between years ",
      "(treated as SD = 0)."
    )
  }

  # names(life_histories$sR) <- "eps_sd"

  # correlation among stage-specific fecundity draws within a year
  # (note: applied across stages, not across years)
  life_histories$egg_rho <- as.numeric(life_pars["egg_rho", "Value"])
  names(life_histories$egg_rho) <- "egg_rho"
  if (is.na(life_histories$egg_rho)) {
    message(
      "egg_rho is missing or blank: treated as 0 (stage-specific ",
      "fecundity draws uncorrelated)."
    )
  }

  # variation in natural mortality
  life_histories$M.cv <- as.numeric(life_pars["M.cv", "Value"])
  names(life_histories$M.cv) <- "M.cv"
  if (is.na(life_histories$M.cv)) {
    message(
      "M.cv is missing or blank: survival will not vary between years ",
      "(treated as CV = 0)."
    )
  } else if (life_histories$M.cv > 0.21) {
    message(
      "M.cv is ", life_histories$M.cv, ": at CVs above ~0.21 the mortality ",
      "distribution is truncated at zero, so a fraction of yearly survival ",
      "draws will sit exactly at 100% survival. Consider whether this much ",
      "interannual variability is intended."
    )
  }

  # correlation among stage-specific survival draws within a year
  # (note: applied across stages, not across years)
  life_histories$M.rho <- as.numeric(life_pars["M.rho", "Value"])
  names(life_histories$M.rho) <- "M.rho"
  if (is.na(life_histories$M.rho)) {
    message(
      "M.rho is missing or blank: treated as 0 (stage-specific survival ",
      "draws uncorrelated)."
    )
  }

  # Compensation ratios are looked up BY NAME (mirroring the anadromous
  # variant) - the previous positional grep silently mis-assigned every
  # ratio when rows were missing or out of canonical order (the data
  # cleaner appends missing cr_E/cr_0 rows at the end of the file).
  # A missing or blank row defaults to 1 (no compensation), matching the
  # data cleaner's fill value. d.vec.f() pairs this vector positionally
  # with the survival vector (sE, s0, s1..sN), so canonical order is
  # guaranteed here.
  cr_names <- c("cr_E", "cr_0", paste0("cr_", 1:Nstage))
  cr_vals <- suppressWarnings(as.numeric(
    life_pars[match(cr_names, life_pars$Name), "Value"]))
  if (all(is.na(cr_vals))) {
    message(
      "No compensation ratio (cr) rows found in the life cycles file - ",
      "all compensation ratios default to 1 (no compensatory density ",
      "dependence)."
    )
  }
  cr_vals[is.na(cr_vals)] <- 1
  life_histories$cr <- cr_vals
  names(life_histories$cr) <- paste("cr", c("E", 0:Nstage), sep = "")


  life_histories$anadromous <- FALSE

  # Set stage names for non-anadromous populations
  # Consistent with anadromous stage_names convention
  life_histories$stage_names <- paste0("K", 1:Nstage)

  #--------------------------------------------
  # Build return object
  #--------------------------------------------
  ret_obj <- list()

  # Density-independent projection matrix
  ret_obj$projection_matrix <- pmx.det

  ret_obj$life_histories <- life_histories
  ret_obj$life_stages_symbolic <- life_stages_symbolic
  ret_obj$density_stage_symbolic <- density_stage_symbolic
  ret_obj$anadrmous <- FALSE


  return(ret_obj)



}
