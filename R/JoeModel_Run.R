#' Joe Model Run
#'
#' @description Runs the Joe Model.
#'
#' @details Runs the Joe Model for cumulative system capacity
#' across stressors and watersheds. Note that only stressors with the applicable to the 'adult' Life_stages from the 'main_sheet' of the Stressor Response workbook are included in the Joe Model. socioeconomic_inputs can be optionally supplied to run the Joe Model with socioeconomic inputs reductions.
#'
#' @param dose dataframe. Stressor magnitude file exported from StressorMagnitudeWorkbook().
#' @param sr_wb_dat list object. Stressor response workbook returned from StressorResponseWorkbook().
#' @param MC_sims numeric. set number of Monte Carlo simulations for the Joe Model.
#' @param stressors (optional) character vector of stressor names to include in the Joe Model. Leave the default value as NA if you wish to include all stressors (and all interaction matrices) or provide a character vector if you only want to run the model on a subset. The selection scopes BOTH regular stressors and two-stressor interaction matrices (use the MInt_ sheet name to select a matrix): only selected items contribute to the cumulative effect. Matrix-only selections are supported - the matrix's two axis stressors are computed internally to feed the interaction surface but are excluded from the CE unless they are also selected. When a matrix and its main-effect stressors are BOTH selected, the matrix's Main_Effect setting governs composition exactly as in a full run ("Included" absorbs the mains into the matrix - never double-counted; "Excluded" keeps them standalone alongside the matrix). To run main effects without their matrix, simply leave the matrix out of the selection.
#' @param adult_sys_cap Should the Joe Model be run only with variables identified for `adult` system capacity.
#' @param socioeconomic_inputs (optional) list object. Socioeconomic inputs returned from SocioEconomicWorkbook().
#' @param strict_na Logical. Controls how NA and missing stressor values affect the final CE score. If FALSE (default), missing stressor rows are assigned system capacity of 1 (no effect) and NA values are ignored in the cumulative product. If TRUE, missing stressor rows result in NA system capacity and any NA values propagate to the final CE score (CE becomes NA).
#'
#' @returns A list object with the following components:
#' \item{ce.df}{dataframe. Cumulative effects data frame with columns for HUC, simulation, and CE (cumulative effect).}
#' \item{sc.dose.df}{dataframe. Stressor-specific system capacity and dose data frame with columns for HUC, Stressor, simulation, dose, sys.cap, int.type, and link.}
#' \item{socioeconomic_inputs}{(optional) list object. Socioeconomic inputs returned from SocioEconomicWorkbook() if socioeconomic_inputs were provided to the function.}
#'
#'
#'   set number of Monte Carlo simulations for the Joe model.
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#'@examples
#'\dontrun{
#' library(CEMPRA)
#'
#' # Load in the sample data from the reference Excel workbook
#' filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
#' filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
#' # Stressor Magnitue and Doese Response Workbooks
#' dose <- StressorMagnitudeWorkbook(filename = filename_rm)
#' sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
#'
#' # Run the Joe Model
#' jmr <- JoeModel_Run(dose = dose,
#'              sr_wb_dat = sr_wb_dat,
#'              MC_sims = 100)
#'
#' # The Joe model holds cumulative effects data frame
#' # and sc.dose.df for individual stressors
#' names(jmr)
#'
#' # Evaluate the cumulative system capacity
#' summary(jmr$ce.df$CE)
#' }
#'
#' @export
JoeModel_Run <- function(dose = NA,
                         sr_wb_dat = NA,
                         MC_sims = 100,
                         stressors = NA,
                         adult_sys_cap = FALSE,
                         socioeconomic_inputs = NULL,
                         strict_na = FALSE) {

  # Define variables in function as null
  .data <- HUC <- simulation <- NULL

  # Preserve the caller's selection; the `stressors` symbol is reused
  # below for the final run list
  stressors_arg <- stressors

  # Stressor Magnitude
  # dose or sm_wb_dat

  # ----------------------------------------------------------------
  # Scope the run: which stressors and interaction matrices to apply
  # ----------------------------------------------------------------
  msheet <- sr_wb_dat$main_sheet

  # Align dose stressor names with the (final) stressor-response names in
  # case duplicates were disambiguated via Stressor_cat on the SR side
  if (is.data.frame(dose)) {
    dose <- align_dose_stressor_names(dose, msheet$Stressors)
  }

  # Life-stage scoping
  if (adult_sys_cap) {
    allowable_stressors <-
      msheet$Stressors[which(msheet$Life_stages == 'adult')]
  } else {
    allowable_stressors <- msheet$Stressors
  }

  # Interaction matrices available in the workbook
  mint_names <- names(sr_wb_dat$MInt)

  if (all(is.na(stressors_arg))) {

    # Default run: every allowable stressor plus every interaction matrix
    selected_mains <- allowable_stressors
    selected_matrices <- mint_names
    # Sentinel: no explicit selection - Main_Effect alone governs whether
    # a matrix absorbs its two main-effect stressors
    explicit_mains <- NULL

    if (length(selected_mains) == 0) {
      stop("Must include more than one stressor applicable to adult system capacity")
    }

  } else {

    # The `stressors` argument scopes BOTH regular stressors and
    # interaction matrices (MInt_ sheet names): only what is selected
    # contributes to the cumulative effect. Selected matrices additionally
    # need their two axis stressors computed internally (their dose draws
    # feed the interaction surface) even when those mains are not
    # selected - such helper mains are removed from the CE after the
    # matrix values are computed (see below).
    selected_matrices <- intersect(stressors_arg, mint_names)
    unknown_matrices <-
      setdiff(stressors_arg[grepl("^MInt_", stressors_arg)], mint_names)
    if (length(unknown_matrices) > 0) {
      warning(
        "Unknown interaction matrix name(s) in `stressors`: ",
        paste(unknown_matrices, collapse = ", "),
        call. = FALSE
      )
    }

    explicit_mains <- intersect(allowable_stressors, stressors_arg)

    # Axis stressors required to evaluate the selected matrices
    axis_stressors <- unlist(lapply(sr_wb_dat$MInt[selected_matrices],
                                    function(m) {
                                      c(trimws(as.character(m$Columns)),
                                        trimws(as.character(m$Rows)))
                                    }))
    axis_stressors <- intersect(unique(axis_stressors), msheet$Stressors)

    selected_mains <- union(explicit_mains, axis_stressors)

    if (length(selected_mains) == 0) {
      stop(
        "None of the requested `stressors` match the stressor response workbook",
        if (adult_sys_cap) " (after the adult life-stage filter)" else "",
        "..."
      )
    }
  }

  # Drop non-selected interaction matrices so they are not applied
  if (length(mint_names) > 0) {
    sr_wb_dat$MInt <- sr_wb_dat$MInt[selected_matrices]
    if (length(sr_wb_dat$MInt) == 0) {
      sr_wb_dat$MInt <- NULL
    }
  }

  # Filter the sr_wb_dat in the local scope to the computation set.
  # pretty_names is subset with the SAME index as stressor_names (the
  # historical code filtered pretty_names against the already-filtered
  # stressor_names - a latent mis-indexing).
  keep_idx <- which(sr_wb_dat$stressor_names %in% selected_mains)
  sr_wb_dat$main_sheet <-
    sr_wb_dat$main_sheet[which(sr_wb_dat$main_sheet$Stressors %in% selected_mains), ]
  sr_wb_dat$pretty_names <- sr_wb_dat$pretty_names[keep_idx]
  sr_wb_dat$stressor_names <- sr_wb_dat$stressor_names[keep_idx]
  sr_wb_dat$sr_dat <-
    sr_wb_dat$sr_dat[which(names(sr_wb_dat$sr_dat) %in% selected_mains)]


  # Stressor Response
  stressors <- sr_wb_dat$stressor_names

  # Re-create the main sheet object
  main.sheet <- sr_wb_dat$main_sheet

  # A stressor set to Minimum/Maximum combination with a BLANK Linked
  # group name silently falls out of the cumulative effect calculation
  # (it lands in tapply's NA group in ce.func) - warn so the user can
  # name the group or switch the Interaction back to the default
  if (all(c("Interaction", "Linked") %in% colnames(main.sheet))) {
    int_v <- tolower(trimws(as.character(main.sheet$Interaction)))
    lnk_v <- trimws(as.character(main.sheet$Linked))
    orphaned <- int_v %in% c("minimum", "maximum") &
      (is.na(lnk_v) | lnk_v == "" | toupper(lnk_v) == "NA")
    if (any(orphaned)) {
      warning(
        "Stressor(s) set to Minimum/Maximum combination with a BLANK ",
        "'Linked' group name: ",
        paste(main.sheet$Stressors[orphaned], collapse = ", "),
        ". These stressors contribute NOTHING to the cumulative effect. ",
        "Enter a group name in the Linked column (stressors sharing the ",
        "group name are combined) or clear the Interaction setting.",
        call. = FALSE
      )
    }
  }

  # Get the stressor names
  stressor.list <- sr_wb_dat$sr_dat


  # Run the Joe curves to generate the response functions
  mean.resp.list <-
    suppressWarnings({
      mean_Response(
        n.stressors = nrow(sr_wb_dat$main_sheet),
        str.list = sr_wb_dat$sr_dat,
        main = sr_wb_dat$main_sheet
      )
    })


  # We now have two important objects:

  # 1) dose - This is a tibble of the doses for
  # each stressor (mean, SD, distribution type, low limit,
  # up limit).  At the moment there are only two distribution
  # types allowed for uncertainty in the stressor dose, truncated normal
  # and truncated lognormal.

  # 2) mean.resp.list - This is a list of lists
  # The primary list is for each stressor ordered according to the tibble
  # main.sheet.  Each list within this list contains
  # 4 approx functions that calculate the mean, SD, low limit and upper limit
  # for system capacity given a dose for that stressor.

  # Get unique HUCs and stressors.
  hucs <- unique(dose$HUC_ID)


  # Output is going to be stored in a 3D array to start.
  # Each row will be a HUC, each column is system capacity
  # for a stressor and sheets are different Monte Carlo
  # simulations.  Order of HUCs (rows) and system capacity per
  # stressor (columns) are given in the vectors hucs and stressors,
  # respectively. Make sys.capacity Global

  sys.capacity <- array(
    NA,
    dim = c(length(hucs),
            length(stressors),
            MC_sims),
    dimnames = list(hucs,
                    stressors,
                    1:MC_sims)
  )

  # the next array and list is more for debugging purposes

  dose.values <- array(
    NA,
    dim = c(length(hucs),
            length(stressors),
            MC_sims),
    dimnames = list(hucs,
                    stressors,
                    1:MC_sims)
  )

  dose.values.list <- array(list(),
                            dim = c(length(hucs),
                                    length(stressors)))


  # ---------------------------------------------------------------
  # (optional) Run the socioeconomic model for each of the MC_sims
  if (!is.null(socioeconomic_inputs)) {
    se_costs <- list()
    se_sr_reductions <- list()
    se_counter <- 1

    while(se_counter < (MC_sims + 1)) {
      socioeconomic_inputs <- SocioEconomicRun(socioeconomic_inputs)
      sub1 <- socioeconomic_inputs$cost_summary[, c("action", "id", "total_cost")]
      sub1$replicate <- se_counter
      se_costs[[se_counter]] <- sub1
      sub2 <- socioeconomic_inputs$stressor_reductions[, c("action", "id", "linked_stressor", "stressor_reductions")]
      sub2$replicate <- se_counter
      se_sr_reductions[[se_counter]] <- sub2
      se_counter <- se_counter + 1
    }
    se_costs <- do.call("rbind", se_costs)
    se_sr_reductions <- do.call("rbind", se_sr_reductions)

    # Override with batch outputs
    socioeconomic_inputs$cost_summary <- se_costs
    socioeconomic_inputs$stressor_reductions <- se_sr_reductions

  } # end of socio-economic setup


  # ---------------------------------------------------------------
  # Loop through HUCs and stressors
  # Apply stressor-response functions across locations

  for (i in 1:length(hucs)) {
    for (j in 1:length(stressors)) {

      # find combination of HUC and stressor in the dose table

      # MJB fix Sept 19th 2024 - older AEP code users grep.
      # This is problematic since some stressors may share the same prefix
      # For example, HUC 4, 44, 14, 140 all have '4'
      # pnt.dose <- intersect(grep(hucs[i], dose$HUC_ID),
      #                       grep(stressors[j], dose$Stressor))
      pnt.dose <- which(dose$HUC_ID == hucs[i] & dose$Stressor == stressors[j])

      # Multiple rows per HUC+Stressor are legitimate additive components
      # (SystemCapacity combines them as 1 - prod(1 - dose)). Guard against
      # accidental copy-paste duplicates: drop rows whose sampling columns
      # are exact duplicates and keep only the distinct components.
      if (length(pnt.dose) > 1) {
        comp_cols <-
          intersect(c("Mean", "SD", "Distribution", "Low_Limit", "Up_Limit"),
                    colnames(dose))
        dup <- duplicated(dose[pnt.dose, comp_cols, drop = FALSE])
        if (any(dup)) {
          pnt.dose <- pnt.dose[!dup]
        }
      }

      # If nothing set - handle based on strict_na argument
      if (length(pnt.dose) == 0) {
        # If strict_na = TRUE, set to NA so final CE becomes NA
        # If strict_na = FALSE (default), assume system capacity is 100%
        sys.capacity[i, j,] <- ifelse(strict_na, NA, 1)
        dose.values[i, j,] <- NA
        dose.values.list[i, j][[1]] <- NA
        next
      }

      # find stressor in main.sheet for relationship
      # (duplicate stressor names are rejected upstream by the workbook
      # reader; keep a defensive first-match)
      pnt.curv <- which(main.sheet$Stressors == stressors[j])
      pnt.curv <- pnt.curv[1]


      # call system capacity function for each stressor
      temp.list <- SystemCapacity(
        f.dose.df = dose[pnt.dose,],
        f.main.df = main.sheet[pnt.curv,],
        f.stressor.df = stressor.list[[stressors[j]]],
        f.mean.resp.list = mean.resp.list[[pnt.curv]],
        n.sims = MC_sims,
        socioeconomic_inputs = socioeconomic_inputs
      )

      # assign system capacity for each stressor to array.
      sys.capacity[i, j,] <- temp.list$sys.cap
      # store dose values as this is good output as well
      dose.values[i, j,] <- temp.list$dose
      # The next dose array stores doses as a list and includes
      # the individual additive doses (i.e., mortality doses)
      dose.values.list[i, j][[1]] <- temp.list$dose.mat


    }
    # end j
  }
  # end k


  # Print error messages if NA appears in the system capacity
  # or stressor values arrays
  if (any(is.na(sys.capacity))) {
    message("At least one NA in system capacity array")
  }

  if (any(is.na(dose.values))) {
    message("At least one NA in stressor values array")
  }

  # change the 3D array to a data frame
  # adply is really slow
  # melt is rocket fuel
  sc.df <- reshape2::melt(sys.capacity)

  # do the same for doses as this is useful output
  dose.df <- reshape2::melt(dose.values)


  # rename columns
  sc.df <-
    dplyr::rename(
      sc.df,
      HUC = "Var1",
      Stressor = "Var2",
      simulation = "Var3",
      sys.cap = "value"
    )

  # do same for dose values
  dose.df <-
    dplyr::rename(
      dose.df,
      HUC = "Var1",
      Stressor = "Var2",
      simulation = "Var3",
      dose = "value"
    )

  # combine SC and dose dataframes (this df is used for output so make global)
  sc.dose.df <- merge(dose.df, sc.df)

  # add interaction type and link to sc.df data.frame
  sc.df$int.type <-
    main.sheet$Interaction[match(sc.df$Stressor, main.sheet$Stressors)]

  sc.df$link <-
    main.sheet$Linked[match(sc.df$Stressor, main.sheet$Stressors)]



  # ---------------------------------------------------------------------
  # If two-factor interaction matrix exists then update the prediction
  if (!(is.null(sr_wb_dat$MInt))) {

    # Loop through interaction matrices (if multiple)
    m_mats <- sr_wb_dat$MInt

    for (mm in 1:length(m_mats)) {
      MInt <- sr_wb_dat$MInt[[mm]]

      # Update sc.dose.df object to account for interaction matrix.
      # Main_Effect semantics are unchanged under an explicit selection:
      # an "Included" matrix absorbs its two mains (never double-counted)
      # whether or not they were also selected - deselect the matrix to
      # run the main effects standalone.
      sc.dose.df <-
        interaction_matrix_sys_cap(MInt, sc.dose.df = sc.dose.df,
                                   adult_sys_cap = adult_sys_cap)

    }

    # Under an explicit selection, only the selected stressors and the
    # matrix results contribute to the CE - axis mains computed solely
    # to feed a matrix (helper doses for Excluded matrices, or mains not
    # absorbed because the matrix was skipped) are removed here
    if (!is.null(explicit_mains)) {
      matrix_result_names <- unique(c(
        names(sr_wb_dat$MInt),
        vapply(sr_wb_dat$MInt, function(m) as.character(m$Matrix_Name), "")
      ))
      sc.dose.df <-
        sc.dose.df[sc.dose.df$Stressor %in% c(explicit_mains, matrix_result_names), ]
    }

    # Update sc.df object to match updated sc.dose.df object
    sc.df_tmp <- sc.dose.df
    sc.df_tmp$dose <- NULL

    sc.df_tmp$int.type <-
      main.sheet$Interaction[match(sc.df_tmp$Stressor, main.sheet$Stressors)]

    sc.df_tmp$link <-
      main.sheet$Linked[match(sc.df_tmp$Stressor, main.sheet$Stressors)]

    sc.df_tmp$int.type <- ifelse(is.na(sc.df_tmp$int.type), "NA", sc.df_tmp$int.type)
    sc.df_tmp$link <- ifelse(is.na(sc.df_tmp$link), "NA", sc.df_tmp$link)

    # Overwrite
    sc.df <- sc.df_tmp

  } # end of interaction matrix
  # ---------------------------------------------------------------------




  # calculate cumulative system capacity by multiplying values in each row (HUC)
  # the complicating factor is some stressors are linked
  # (e.g., take the minimum of the linked and not their product)
  # so we'll need to account for this.
  # use ddply and the function ce.func to create the cumulative effect
  # and make it global
  # uses ddply which is pretty slow (use dplyr??)
  # MJB updated with dplyr

  ce_df_raw <- sc.df %>%
    dplyr::group_by(HUC, simulation) %>%
    dplyr::group_modify( ~ ce.func(.x, strict_na = strict_na))

  ce.df <- ce_df_raw



  # Build return object
  return_list <- list()
  return_list$ce.df <- ce.df
  return_list$sc.dose.df <- sc.dose.df


  # Include socioeconomic inputs if they exist
  if (!is.null(socioeconomic_inputs)) {
    return_list$socioeconomic_inputs <- socioeconomic_inputs
  }




  return(return_list)
}
