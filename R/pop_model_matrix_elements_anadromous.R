#'Population Model Matrix Elements for Anadromous Fish
#'
#' @description Build population model matrix elements for anadromous fish.
#'
#' @details This is an intermediate setup function to run the population model. However, some of outputs are useful on their own, especially for eigen analyses. pop_model_matrix_elements() is run after pop_model_setup() or pop_model_setup_anadromous(), but pop_model_matrix_elements() must be run before Projection_DD(). Key outputs to explore include a density independent projection matrix, life histories, and a symbolic representation of the matrix math for density dependent and density independent components. population_model_setup() should be run with pop_model_matrix_elements() for non-anadromous species. population_model_setup_anadromous() should be run with pop_model_matrix_elements_anadromous() for anadromous species.
#'
#' @param pop_mod_setup List. Object returned from population_model_setup_anadromous(). Use pop_model_matrix_elements() for objects returned from population_model_setup().
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
pop_model_matrix_elements_anadromous <- function(pop_mod_setup = NA) {




  life_histories <- pop_mod_setup$life_histories
  life_stages_symbolic <- pop_mod_setup$life_stages_symbolic
  life_pars <- pop_mod_setup$life_pars
  Nstage <- pop_mod_setup$Nstage
  density_stage_symbolic <- pop_mod_setup$density_stage_symbolic


  Nyears <- 10 * sum(life_histories$nYrs)

  # Check for issue where survivorship is 1.0 and years in stage is 1.0
  bad_match <- life_histories$S[paste0("s", 1:(pop_mod_setup$Nstage - 1))] == 1
  bad_match <- bad_match[!(is.na(bad_match))]

  if(any(bad_match)) {
    life_histories$S[paste0("s", 1:(pop_mod_setup$Nstage - 1))][which(bad_match)] <- 0.999999
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
        life_histories$eps,
        life_histories$u,
        life_histories$smig
      )
    )

  if(!(all(pop_mod_setup$projection_matrix == pmx.det))) {
    stop("Projection matrix generated from pmx.det does not match projection matrix from pop_mod_setup...")
  }

  # Initialize populations and find carrying capacity for each life stage
  # Carrying capacity is defined at the adult stage and
  # estimated for all other life-stages using the stable-stage distribution
  # (i.e., if the carrying capacity of the adult stage was set to 100 the
  # carrying capacity for the hatchling (fry) stage is the amount of hatchlings
  # necessary to result in k (e.g., 100) adults at equilibrium conditions)

  # replaced with try catch - gives error with high suv...
  gets0 <- tryCatch(
    {
      stats::optimize(
        s0_optim_anadromous.f,
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

  # Also, if any Beverton-Holt DD functions are present then set s0.1.det back to s0
  bh_functions <- pop_mod_setup$life_pars[grep("bh_stage_", life_pars$Name), "Value"]

  if(length(bh_functions) > 0) {
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
          life_histories$u,
          life_histories$smig,
          life_histories$eps
        )
      )
  }



  if (!(all(pop_mod_setup$projection_matrix == M.1.pmx))) {
    message(
      "Anadromous: S0 adjusted to s0.1.det..."
    )
  }

  # Replace any missing NA values with 1
  M.1.pmx <- ifelse(is.na(M.1.pmx), 1, M.1.pmx)

  # Replace any Inf values with 1
  # M.1.pmx <- ifelse(is.infinite(M.1.pmx), 1, M.1.pmx)

  life_histories$gen.time <- popbio::generation.time(M.1.pmx)
  names(life_histories$gen.time) <- "gen.time"

  # variation in eggs per spawner
  life_histories$eps_sd <- life_pars["eps_sd", "Value"]
  names(life_histories$sR) <- "eps_sd"

  # temporal correlation in eggs per spawner
  life_histories$egg_rho <- life_pars["egg_rho", "Value"]
  names(life_histories$egg_rho) <- "egg_rho"

  # variation in natural mortality
  life_histories$M.cv <- life_pars["M.cv", "Value"]
  names(life_histories$M.cv) <- "M.cv"

  # temporal correlation in natural mortality
  life_histories$M.rho <- life_pars["M.rho", "Value"]
  names(life_histories$M.rho) <- "M.rho"


  # add custom vector of compensation ratios.
  # set to 1 if AN

  cr_vec <- c("cr_E", "cr_0", paste0("cr_", seq(1, pop_mod_setup$life_histories$Nstage_Pb)))
  life_histories$cr <- life_pars[cr_vec, "Value"]
  names(life_histories$cr) <- cr_vec


  # Add spawning years for reference
  life_histories$spanwing_years <- pop_mod_setup$spanwing_years

  life_histories$anadromous <- TRUE

  #--------------------------------------------
  # Build return object
  #--------------------------------------------
  ret_obj <- list()

  # Density-independent projection matrix
  ret_obj$projection_matrix <- pmx.det

  ret_obj$life_histories <- life_histories
  ret_obj$life_stages_symbolic <- life_stages_symbolic
  ret_obj$density_stage_symbolic <- density_stage_symbolic
  ret_obj$anadrmous <- TRUE


  pop_mod_setup


  return(ret_obj)



}
