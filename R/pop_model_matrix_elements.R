#'Population Model Matrix Elements
#'
#' @description Build population model matrix elements.
#'
#' @details This is an intermediate setup function to run the population model. However, some of outputs are useful on their own, especially for eigen analyses. pop_model_matrix_elements() is run after pop_model_setup() or pop_model_setup_anadromous(), but pop_model_matrix_elements() must be run before Projection_DD(). Key outputs to explore include a density independent projection matrix, life histories, and a symbolic representation of the matrix math for density dependent and density independent components.
#'
#' @param pop_mod_setup List. Object returned from population_model_setup().
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
pop_model_matrix_elements <- function(pop_mod_setup = NA) {

  # Detrmine if we should use pop_model_matrix_elements_anadromous()
  # instead of pop_model_matrix_elements()
  if(pop_mod_setup$anadromous) {
    ret_obj <- pop_model_matrix_elements_anadromous(pop_mod_setup)
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
  if(any(bad_match)) {
    life_histories$S[paste0("s", 1:pop_mod_setup$Nstage)][which(bad_match)] <- 0.999999
  }


  # Build the projection matrix
  pmx.det <-
    pmx_eval(
      life_stages_symbolic,
      c(
        life_histories,
        life_histories$S,
        life_histories$nYrs,
        life_histories$mat
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
          life_histories$mat
        )
      )
  }

  # life_histories

  # Set NA values to zero for final transition stage - Nov 19th 2024
  if(is.na(M.1.pmx[nrow(M.1.pmx), ncol(M.1.pmx)])) {
    M.1.pmx[nrow(M.1.pmx), ncol(M.1.pmx)] <- 0
  }


  if (!(all(pop_mod_setup$projection_matrix == M.1.pmx))) {
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

  # names(life_histories$sR) <- "eps_sd"

  # temporal correlation in eggs per spawner
  life_histories$egg_rho <- as.numeric(life_pars["egg_rho", "Value"])
  names(life_histories$egg_rho) <- "egg_rho"

  # variation in natural mortality
  life_histories$M.cv <- as.numeric(life_pars["M.cv", "Value"])
  names(life_histories$M.cv) <- "M.cv"

  # temporal correlation in natural mortality
  life_histories$M.rho <- as.numeric(life_pars["M.rho", "Value"])
  names(life_histories$M.rho) <- "M.rho"

  life_histories$cr <- as.numeric(life_pars[grep("cr", life_pars$Name), "Value"])
  names(life_histories$cr) <- paste("cr", c("E", 0:Nstage), sep = "")


  life_histories$anadromous <- FALSE


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
