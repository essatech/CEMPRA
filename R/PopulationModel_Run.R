#' Population Model Run
#'
#' @description Runs the population model with stressors and stressor-response relationships
#'
#' @details Runs the population model within
#'  the Joe Model stress-response framework. The population model is
#'  run independently for each single sub-watershed unit (HUC_ID). Inputs for
#'  `PopulationModel_Run` include the stressor-response and stressor magnitude
#'  workbooks as well as the life cycle parameters object.
#'
#'  If the life cycle parameters include a `p.cat` row (probability of a
#'  catastrophic event per generation), catastrophe years first run the
#'  normal annual projection (aging, reproduction, natural mortality,
#'  density dependence) and then remove a random 50-100 percent of every
#'  life stage. Catastrophic mortality is applied in addition to normal
#'  annual mortality, not instead of it (see \code{\link{Projection_DD}}).
#'
#' @param dose dataframe. Stressor magnitude dataset
#'  imported from StressorMagnitudeWorkbook().
#' @param sr_wb_dat list object. Stressor response workbook
#' imported from StressorResponseWorkbook().
#' @param life_cycle_params dataframe. Life cycle parameters.
#' @param HUC_ID character. HUC_ID for the location unit. Can only be one HUC_ID (not an array).
#' @param n_years numeric. Number of years to run the population.
#' @param MC_sims numeric. set number of Monte
#'  Carlo simulations for the Population Model.
#' @param stressors (optional) character vector of stressor
#'  names to include in the Population Model. Leave the default
#'  value as NA if you wish to include all stressors (and all
#'  two-stressor interaction matrices) applicable to the population
#'  model. The selection scopes BOTH regular stressors and interaction
#'  matrices (use the MInt_ sheet name to select a matrix). A selected
#'  matrix has its two axis stressors' doses computed internally to
#'  feed the interpolation, but those mains only contribute their own
#'  effects if they are also selected. Any combination works: main
#'  effects only, matrix only, or both (acting multiplicatively). The
#'  matrix's Main_Effect setting is ignored in the population model -
#'  the selection is the control. Matrix CE rows are tagged with the
#'  MInt_ sheet's Life_stages and Parameters header cells (e.g.,
#'  "stage_1" / "survival").
#' @param output_type (optional) character. Set to "full" for all data of "adults" for only adult data.
#' @param s0_calibrate (optional) logical. If TRUE (default) fry survival
#'  (s0) is re-calibrated so lambda = 1 when an adult capacity `k` is set
#'  and no Beverton-Holt flags are present. Set to FALSE to keep the raw
#'  s0 from the life cycles file (used by pop_model_ltre so that lambda
#'  responds to vital-rate perturbations instead of being re-tuned to 1).
#' @param habitat_dd_k (optional) dataframe of location and stage-specific habitat capacity k values for the target species. If used this dataframe will override the capacity estimates. Blank or NA K cells mean "no constraint for that life stage" (not unknown/to-be-determined). Supplying a row for a HUC switches that location into habitat-capacity mode: fry survival (s0) is not recalibrated to force lambda = 1, because growth is expected to be restrained by the K constraints. A row whose K cells are all blank leaves every stage unconstrained and triggers a warning. Optional k_..._cv columns hold the coefficient of variation for the matching k_..._mean capacity estimate: each capacity with a cv > 0 is redrawn every projection year and every batch replicate from a normal distribution (mean = the K value, sd = K * cv) clamped at zero, with the CE and baseline runs of a replicate sharing the same draws (see build_stage_k_noise). Missing cv columns or blank/NA cv cells mean cv = 0 (capacity treated as exact; no random draws).
#'
#' @returns List object of population model outputs. These include the population vectors for each Monte Carlo simulation and year. List objects names include "ce" for the runs with stressors and "baseline" for the runs without stressors.
#'
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(CEMPRA)
#' }
#'
#' @export
PopulationModel_Run <- function(dose = NA,
                                sr_wb_dat = NA,
                                life_cycle_params = NA,
                                HUC_ID = NA,
                                n_years = 100,
                                MC_sims = 10,
                                stressors = NA,
                                output_type = "full",
                                habitat_dd_k = NULL,
                                s0_calibrate = TRUE) {

  # Define variables in function as null
  # .data <- HUC <- simulation <- NULL

  #------------------------------------------------------------------------
  # Run the population model time series projection for a target watershed
  #------------------------------------------------------------------------

  # Two-stressor interaction matrices (sr_wb_dat$MInt) ARE applied in the
  # population model (added 2026-09-23): each selected matrix is
  # interpolated from its two axis stressors' per-year sampled doses and
  # contributes its own CE rows, tagged with the matrix sheet's
  # Life_stages / Parameters cells. Unlike the Joe Model, the matrix's
  # Main_Effect setting is deliberately IGNORED here - the caller's
  # `stressors` selection controls what contributes (main effects, the
  # matrix, or both, acting multiplicatively like any other stressors).

  # Check to see if stressor-response data is missing and fill slots
  if (is.data.frame(dose)) {
    #------------------------------------------------------------------------
    # Subset the stressor magnitude dataframe to
    # include only the target HUC unit
    # (population model is run seperatly for each HUC)

    ce_df_sub <- dose[which(dose$HUC_ID == HUC_ID), ]

    # Align dose stressor names with the (final) stressor-response names in
    # case duplicates were disambiguated via Stressor_cat on the SR side
    ce_df_sub <- align_dose_stressor_names(ce_df_sub,
                                           sr_wb_dat$main_sheet$Stressors)

    # Interaction matrices: pull them aside so the internal JoeModel_Run
    # call below never applies them (the pop model applies its own,
    # without Main_Effect absorption)
    mint_all <- sr_wb_dat$MInt
    sr_wb_dat$MInt <- NULL
    mint_names <- names(mint_all)

    # Omit non-target stressors from the stressor magnitude and sr dataframes.
    # The `stressors` argument scopes BOTH regular stressors and interaction
    # matrices (MInt_ sheet names). Selected matrices need their two axis
    # stressors' doses computed internally even when those mains are not
    # selected - such helper rows are removed from the final CE_df below.
    if (any(!is.na(stressors))) {

      selected_mains <- stressors[!grepl("^MInt_", stressors)]
      selected_matrices <- intersect(stressors, mint_names)
      unknown_matrices <-
        setdiff(stressors[grepl("^MInt_", stressors)], mint_names)
      if (length(unknown_matrices) > 0) {
        warning(
          "Unknown interaction matrix name(s) in `stressors`: ",
          paste(unknown_matrices, collapse = ", "),
          call. = FALSE
        )
      }

      # Axis stressors required to evaluate the selected matrices
      axis_stressors <- unlist(lapply(mint_all[selected_matrices],
                                      function(m) {
                                        c(trimws(as.character(m$Columns)),
                                          trimws(as.character(m$Rows)))
                                      }))
      keep_set <- union(selected_mains, axis_stressors)

      # Subset the stressor magnitude dataframe to include only the target stressors
      ce_df_sub <- ce_df_sub[which(ce_df_sub$Stressor %in% keep_set), ]
      # Subset the stressor response dataframe to include only the target stressors
      sr_wb_dat$main_sheet <- sr_wb_dat$main_sheet[which(sr_wb_dat$main_sheet$Stressors %in% keep_set), ]
      sr_wb_dat$stressor_names <- sr_wb_dat$stressor_names[sr_wb_dat$stressor_names %in% keep_set] # nolint
      sr_wb_dat$sr_dat <- sr_wb_dat$sr_dat[which(names(sr_wb_dat$sr_dat) %in% keep_set)]

      explicit_selection <- TRUE

    } else {
      # Default: all stressors and all interaction matrices in the workbook
      selected_mains <- NULL
      selected_matrices <- mint_names
      explicit_selection <- FALSE
    }

    mint_sel <- mint_all[selected_matrices]

    # Multiple From_Year/To_Year activation windows for the same stressor
    # are NOT supported: the downstream dose pooling (SystemCapacity
    # combines all rows for one stressor) and the per-replicate
    # de-duplication would silently collapse them into the first window
    # with a contaminated magnitude (see OUTSTANDING_ISSUES 1.11), so this
    # is a hard error. Workaround (full multi-window behaviour today):
    # give each window its own stressor name (e.g. "Drawdown_early",
    # "Drawdown_late") pointing at the same stressor-response curve - each
    # row is then processed independently end to end.
    # FUTURE: native multi-window support could be developed by carrying
    # the window columns through the Joe Model dose stage (per-row
    # disambiguation so magnitudes are not pooled), adding From/To to the
    # per-replicate dedup key in the batch loop, and exempting
    # non-overlapping windows from the duplicate-CE-row conflict check in
    # pop_model_ce_apply_year().
    if (any(c("From_Year", "To_Year") %in% colnames(ce_df_sub)) &&
        nrow(ce_df_sub) > 0) {
      fy <- if ("From_Year" %in% colnames(ce_df_sub)) ce_df_sub$From_Year else NA
      ty <- if ("To_Year" %in% colnames(ce_df_sub)) ce_df_sub$To_Year else NA
      win_key <- paste(fy, ty, sep = "|")
      n_windows <- tapply(win_key, ce_df_sub$Stressor, function(x) length(unique(x)))
      bad <- names(n_windows)[n_windows > 1]
      if (length(bad) > 0) {
        stop(
          "Stressor(s) ", paste(bad, collapse = ", "), " at HUC_ID ", HUC_ID,
          " have multiple rows with different From_Year/To_Year activation ",
          "windows. Multiple windows per stressor are not supported - give ",
          "each window its own stressor name (e.g. 'Drawdown_early', ",
          "'Drawdown_late') pointing at the same stressor-response curve.",
          call. = FALSE
        )
      }
    }

    # Merge stressor_response main sheet data
    ce_df_sub$Stressor_cat <- NULL
    ce_df_sub <-
      merge(
        ce_df_sub,
        sr_wb_dat$main_sheet,
        by.x = "Stressor",
        by.y = "Stressors",
        all.x = TRUE
      )

    # Stressor Magnitude...
    smw_sample <-
      data.frame(
        HUC_ID = ce_df_sub$HUC_ID,
        NAME = ce_df_sub$NAME,
        Stressor = ce_df_sub$Stressor,
        Stressor_cat = ce_df_sub$Stressor_cat,
        Mean = ce_df_sub$Mean,
        SD = ce_df_sub$SD,
        Distribution = ce_df_sub$Distribution,
        Low_Limit = ce_df_sub$Low_Limit,
        Up_Limit = ce_df_sub$Up_Limit
      )

    #------------------------------------------------------------------------
    # Calculate the dose and system capacity score for the selected HUC
    # for each stressor

    # n_reps will be MC_sims * number of years
    n_reps <- MC_sims * n_years

    jm <- suppressWarnings({
      JoeModel_Run(
        dose = smw_sample,
        sr_wb_dat = sr_wb_dat,
        MC_sims = n_reps,
        adult_sys_cap = FALSE
      )
    })

    # Gather summary at stressor level
    dobj <- jm$sc.dose.df

    # Apply two-stressor interaction matrices (if any): interpolate each
    # matrix from its axis stressors' sampled doses per replicate-year.
    # keep_main_effects retains BOTH axis stressors - the matrix's
    # Main_Effect setting is deliberately ignored in the population
    # model (the caller's selection controls what contributes; a matrix
    # and its mains simply combine multiplicatively when both present).
    if (length(mint_sel) > 0) {
      for (mm in seq_along(mint_sel)) {
        MInt <- mint_sel[[mm]]
        dobj <- interaction_matrix_sys_cap(
          MInt,
          sc.dose.df = dobj,
          adult_sys_cap = FALSE,
          keep_main_effects = c(trimws(as.character(MInt$Columns)),
                                trimws(as.character(MInt$Rows)))
        )
      }
    }

    # Ensure no NA values (a matrix with missing axis doses degrades to
    # "no effect" here as well)
    dobj$sys.cap <- ifelse(is.na(dobj$sys.cap), 1, dobj$sys.cap)

    # Merge on MCMC sims and year
    mcmc_yr <- expand.grid(MCMC = 1:MC_sims, Year = 1:n_years)
    mcmc_yr$simulation <- 1:nrow(mcmc_yr)

    dobj <-
      merge(
        dobj,
        mcmc_yr,
        by.x = "simulation",
        by.y = "simulation",
        all.x = TRUE,
        all.y = FALSE
      )

    # Overwrite simulation with MCMC
    dobj$simulation <- dobj$MCMC
    dobj$MCMC <- NULL



    # add on missing attr columns
    base_merge_cols <- c("Stressor", "Life_stages", "Parameters", "Stressor_cat")

    # Carry optional From_Year / To_Year activation-window columns (if supplied).
    # These are dropped from the Joe Model input (smw_sample) above, but are
    # propagated here so the population model can restrict stressors to a year
    # window. Absent columns -> current behaviour (applied to all years).
    if ("From_Year" %in% colnames(ce_df_sub)) {
      base_merge_cols <- c(base_merge_cols, "From_Year")
    }
    if ("To_Year" %in% colnames(ce_df_sub)) {
      base_merge_cols <- c(base_merge_cols, "To_Year")
    }

    merge_cols <-
      ce_df_sub[, base_merge_cols]

    merge_cols <-
      merge_cols[!(duplicated(merge_cols)), ]

    # Interaction matrix rows: life stage and parameter linkages come
    # from the MInt_ sheet's Life_stages / Parameters header cells
    # (matrices have no dose rows or Main-sheet rows of their own).
    # No From/To year window in the workbook path - applied to all years.
    if (length(mint_sel) > 0) {
      mint_rows <- do.call(rbind, lapply(mint_sel, function(m) {
        r <- data.frame(
          Stressor = as.character(m$Matrix_Name),
          Life_stages = as.character(m$Life_stages),
          Parameters = as.character(m$Parameters),
          Stressor_cat = as.character(m$Matrix_Name),
          stringsAsFactors = FALSE
        )
        if ("From_Year" %in% base_merge_cols) r$From_Year <- NA
        if ("To_Year" %in% base_merge_cols) r$To_Year <- NA
        r
      }))
      merge_cols <- rbind(merge_cols, mint_rows)
    }

    m_all <-
      merge(
        merge_cols,
        dobj,
        by.x = "Stressor",
        by.y = "Stressor",
        all.x = TRUE,
        all.y = TRUE
      )

    # Fix col names
    colnames(m_all)[colnames(m_all) == "Stressors"] <-
      "Stressor"
    colnames(m_all)[colnames(m_all) == "Life_stages"] <-
      "life_stage"
    colnames(m_all)[colnames(m_all) == "Parameters"] <-
      "parameter"
    colnames(m_all)[colnames(m_all) == "Stressor_cat"] <-
      "Stressor_cat"

    # Under an explicit `stressors` selection, only the selected
    # stressors and matrices contribute - axis mains computed solely to
    # feed a matrix are removed here
    if (explicit_selection) {
      mint_result_names <- unique(c(
        names(mint_sel),
        unlist(lapply(mint_sel, function(m) as.character(m$Matrix_Name)))
      ))
      m_all <-
        m_all[m_all$Stressor %in% c(selected_mains, mint_result_names), ]
    }

    # Return cleaned object
    CE_df <- m_all

  } else {
    # Run without CE layers
    CE_df <- NULL
  }


    # nrow(CE_df) # MC_sims * n_years * nrow(merge_cols)
    # head(CE_df)
    # tail(CE_df)
    # hist(CE_df$sys.cap[CE_df$Stressor == "MWAT Rearing"])

    #------------------------------------------------------------------------
    # Setup the population model to project the population forward in time

    # Gather population model inputs
    # Setup objects for population model
    pop_mod_setup <-
        pop_model_setup(life_cycles = life_cycle_params)

    # Build matrix elements for population model
    pop_mod_mat <-
        pop_model_matrix_elements(pop_mod_setup = pop_mod_setup,
                                  s0_calibrate = s0_calibrate)


    # Set the K.adj (K adjustment prior to pop model run)
    life_histories <- pop_mod_mat$life_histories

    # Mathematical expression of the transition matrix
    life_stages_symbolic <-
        pop_mod_mat$life_stages_symbolic

    # Mathematical expression of the density matrix
    density_stage_symbolic <-
        pop_mod_mat$density_stage_symbolic

    all_outputs <- list()
    all_outputs_baseline <- list()
    counter_huc <- 1

    #------------------------------------------------------------------------
    # Add in effect of population catastrophe
    # probability per generation. Catastrophic mortality is applied in
    # addition to the normal annual projection (see Projection_DD).
    p.cat <- life_cycle_params$Value[life_cycle_params$Name == "p.cat"]
    if(length(p.cat) == 0) {
      p.cat <- 0
    } else {
      if(is.na(p.cat)) {
        p.cat <- 0
      }
    }
    p.cat <- ifelse(is.na(p.cat), 0, p.cat)



    #------------------------------------------------------------------------
    # (optional) habitat_dd_k - import the habitat capacity k values
    # Need to build table of bh_stage_0, bh_stage_pb_1, bh_spawners, etc.
    # to flag with stages will have DD restrictions

    # Clean habitat capacity column names (normalize spawner column variants)
    if (!is.null(habitat_dd_k)) {
      habitat_dd_k <- pop_model_hab_dens_clean(hab_dens = habitat_dd_k)
    }

    ret_obj <- build_k_for_proj_dd(
      habitat_dd_k = habitat_dd_k,
      HUC_ID = HUC_ID,
      life_histories = life_histories,
      life_cycle_params = life_cycle_params
    )

    stage_k_override <- ret_obj$ret_stage_k_override
    bh_dd_stages <- ret_obj$ret_bh_dd_stages
    total_anadromous_spawners <- ret_obj$ret_total_anadromous_spawners

    # Capacity uncertainty (cv) values from the habitat file's k_..._cv
    # columns (0 where absent/blank). Consumed per batch replicate below.
    stage_k_cv <- ret_obj$ret_stage_k_cv
    spawner_k_cv <- ret_obj$ret_spawner_k_cv

    #------------------------------------------------------------------------
    # Run the population model for each batch replicate

    # Need to copy and rename some variables for code consistency
    this_huc <- HUC_ID
    huc_outputs <- list()
    huc_outputs_baseline <- list()
    counter_sim <- 1
    test_n_replicates <- MC_sims
    test_n_years <- n_years

    # Loop through simulations per HUC
    for (ii in 1:test_n_replicates) {

      # Environmental sample for this rep
      if (is.null(CE_df)) {

        CE_df_rep <- CE_df

      } else {

        CE_df_rep <-
          CE_df[which(CE_df$simulation == ii &
                        CE_df$HUC == this_huc), ]
        CE_df_rep <-
          CE_df_rep[!(duplicated(CE_df_rep[, c("Stressor", "life_stage", "HUC", "Year")])), ]

        # Do not include regular Joe parameters
        CE_df_rep <-
          CE_df_rep[which(!(is.na(CE_df_rep$parameter))), ]

      }

      # Determine if K spawners should override Ka
      K_input <- life_histories$Ka

      if (!(is.null(total_anadromous_spawners))) {
        if (total_anadromous_spawners > 0) {
          # Override the Ka value with spawner capacity
          K_input <- total_anadromous_spawners
        }
      }


      # Should normal population vector be returned...
      # or should matricies be returned
      alt_return <- ""
      alt_return <- ifelse(output_type == "qa_matrices", "qa_matrices", "")

      # Stochastic capacity draw for this batch replicate (habitat K
      # uncertainty from the k_..._cv columns): per-year multiplicative
      # factors, clamped at zero. The SAME matrix is passed to the CE and
      # baseline runs so the pair sees identical capacity trajectories.
      # NULL (and no random numbers consumed) when no cv values are set.
      stage_k_noise <- build_stage_k_noise(
        stage_k_cv = stage_k_cv,
        spawner_k_cv = spawner_k_cv,
        Nyears = test_n_years
      )

        # Run simple population projection - project forward through time
        # Run the simulation with CE stressors
        run_with_ce <-
            Projection_DD(
                M.mx = life_stages_symbolic,
                # projection matrix expression
                D.mx = density_stage_symbolic,
                # density-dependence matrix
                H.mx = NULL,
                dat = life_histories,
                # life history data
                K = K_input,
                # initial pop size as stage-structure vector
                Nyears = test_n_years,
                # years to run simulation
                p.cat = p.cat,
                # Probability of catastrophe
                CE_df = CE_df_rep,
                # Vector of K values for fry, stage 1 ...
                stage_k_override = stage_k_override,
                # Vector of life stages "dd_hs_0", "bh_stage_1" with DD
                bh_dd_stages = bh_dd_stages,
                # Per-year capacity noise (habitat K uncertainty)
                stage_k_noise = stage_k_noise,
                anadromous = life_histories$anadromous,
                alt_return = alt_return
            )


        # Run baseline with no CE
        run_with_baseline <-
            Projection_DD(
                M.mx = life_stages_symbolic,
                # projection matrix expression
                D.mx = density_stage_symbolic,
                # density-dependence matrix
                H.mx = NULL,
                dat = life_histories,
                # life history data
                K = K_input,
                # initial pop size as stage-structure vector
                Nyears = test_n_years,
                # years to run simulation
                p.cat = p.cat,
                # Probability of catastrophe
                CE_df = NULL,
                # Vector of K values for fry, stage 1 ...
                stage_k_override = stage_k_override,
                # Vector of life stages "dd_hs_0", "bh_stage_1" with DD
                bh_dd_stages = bh_dd_stages,
                # Same capacity noise as the CE run (paired draws)
                stage_k_noise = stage_k_noise,
                anadromous = life_histories$anadromous,
                alt_return = alt_return
            )


        # Return matrices here if for lambda calculations
        if (output_type == "qa_matrices") {
          out_list <- list(ce = run_with_ce, baseline = run_with_baseline)
          return(out_list)
        }
        # or continue script below with population vector


        # Gather info - for CE run
        run_with_ce$vars <- NULL
        run_with_ce$Cat. <- NULL

        run_with_ce$info <-
            data.frame(
                huc_id = this_huc,
                sim = ii,
                type = "CE"
            )

        huc_outputs[[counter_sim]] <- run_with_ce

        # Gather info - for CE run
        run_with_baseline$vars <- NULL
        run_with_baseline$Cat. <- NULL

        run_with_baseline$info <-
            data.frame(
                huc_id = this_huc,
                sim = ii,
                type = "baseline"
            )

        huc_outputs_baseline[[counter_sim]] <-
            run_with_baseline

        counter_sim <- counter_sim + 1
    }

    #------------------------------------------------------------------------
    # Gather the outputs from the population model

    full_output <- list()
    full_output[["ce"]] <- huc_outputs
    full_output[["baseline"]] <- huc_outputs_baseline
    full_output[["MC_sims"]] <- MC_sims


    #------------------------------------------------------------------------
    # Return the adult population vectors in a clean dataframe
    if (output_type == "adults") {
        # Gather the adult vectors
        getPopvec <- function(x) {
            x[["pop"]]
        }
        n_adults <- lapply(full_output[["ce"]], getPopvec)
        avec <- do.call("rbind", n_adults)
        avec$MC_sim <- rep(1:MC_sims, each = n_years + 1)
        avec$group <- "ce"

        n_adults <- lapply(full_output[["baseline"]], getPopvec)
        bvec <- do.call("rbind", n_adults)
        bvec$MC_sim <- rep(1:MC_sims, each = n_years + 1)
        bvec$group <- "baseline"

        # Gather the population vectors
        vec_all <- rbind(avec, bvec)
        return(vec_all)
    }


    #------------------------------------------------------------------------
    # Return all the data in a full list format
    if (output_type == "full") {
        return(full_output)
    }
}
