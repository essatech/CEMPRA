#' Population Model Get Spawners
#'
#' @description Get spawners from `PopulationModel_Run()` output.
#'
#' @details Transform verbose output from PopulationModel_Run() into clean dataframe of abundance data for target life stage. This should be run after `PopulationModel_Run()` or `Projection_DD`.
#'
#' @param data list output from `PopulationModel_Run()`.
#' @param life_stage Target lifestage. Character string. Can be either `spawners` or.
#' @param life_cycle_params dataframe. Life cycle parameters.
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(CEMPRA)
#' }
#'
#' @export
pop_model_get_spawners <- function(data = NULL,
                                   life_stage = "spawners",
                                   life_cycle_params = NULL) {

  # Run these to get n-stages
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycle_params)

  # Stage names and n-stage
  Nstage <- pop_mod_setup$Nstage
  Nstage_Pb <- pop_mod_setup$life_histories$Nstage_Pb
  stage_names <- pop_mod_setup$life_histories$stage_names

  spanwer_stages <- which(grepl("_B_", stage_names))

  # Get data from batch replicate
  get_rep_data <- function(rep, spawn_cols) {

    location_id <- rep$info$huc_id[1]
    rep_id <- rep$info$sim[1]

    # dataframe of spawners
    spawners <- rep$N[, c(spawn_cols)]
    spawners <- rowSums(as.data.frame(spawners))

    spawners <- round(spawners, 0)

    # Add on rows
    exp_df <- data.frame(
      location_id = location_id,
      rep_id = rep_id,
      year = 1:length(spawners),
      spawners = spawners
    )

    return(exp_df)
  }

  reps <- data$ce

  # Run function across batch replicates to get data
  reps_data <- lapply(reps, get_rep_data, spawn_cols = spanwer_stages)
  reps_data <- do.call("rbind", reps_data)

  return(reps_data)

}
