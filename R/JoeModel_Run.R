#' Joe Model Run
#'
#' @description Runs the Joe Model.
#'
#' @details Runs the Joe Model for cumulative system capacity
#' across stressors and watersheds. Note that only stressors with the applicable to the 'adult' Life_stages from the 'main_sheet' of the Stressor Response workbook are included in the Joe Model
#'
#' @param dose dataframe. Stressor magnitude file exported from StressorMagnitudeWorkbook().
#' @param sr_wb_dat list object. Stressor response workbook returned from StressorResponseWorkbook().
#' @param MC_sims numeric. set number of Monte Carlo simulations for the Joe Model.
#' @param stressors (optional) character vector of stressor names to include in the Joe Model. Leave the default value as NA if you wish to include all stressors applicable to the adult life stage or provide a character vector of stressors if you only want to run the model on a subset of the stressors.
#' @param adult_sys_cap Should the Joe Model be run only with variables identified for adult system capacity.
#'
#'   set number of Monte Carlo simulations for the Joe model.
#' @importFrom rlang .data
#'
#'@examples
#'\dontrun{
#' library(CEMPRA)
#'
#' # Load in the sample data from the reference Excel workbook
#' filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
#' filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
#' # Stessor Magnitue and Doese Response Workbooks
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
                         adult_sys_cap = TRUE) {

  # Define variables in function as null
  .data <- HUC <- simulation <- NULL

  # Stressor Magnitude
  # dose or sm_wb_dat

  # -------------------------------------------------------
  # Exclude Stressors not applicable to the adult lifestage
  # -------------------------------------------------------
  msheet <- sr_wb_dat$main_sheet

  if (adult_sys_cap) {
    # Allowable stressors
    allowable_stressors <-
      msheet$Stressors[which(msheet$Life_stages == 'adult')]

    # Filter the sr_wb_dat in the local scope to omit stressors
    if (all(is.na(stressors))) {
      allowable_stressors <- allowable_stressors
    } else {
      allowable_stressors <- intersect(allowable_stressors, stressors)
    }

    if (length(allowable_stressors) == 0) {
      stop("Must include more than one stressor applicable to adult system capacity")
    }


    # Filter the sr_wb_dat in the local scope to omit stressors
    sr_wb_dat$main_sheet <-
      sr_wb_dat$main_sheet[which(sr_wb_dat$main_sheet$Stressors %in% allowable_stressors),]
    # Raw names
    sr_wb_dat$stressor_names <-
      sr_wb_dat$stressor_names[which(sr_wb_dat$stressor_names %in% allowable_stressors)]
    # Pretty names
    sr_wb_dat$pretty_names <-
      sr_wb_dat$pretty_names[which(sr_wb_dat$stressor_names %in% allowable_stressors)]
    # Dose-response data
    sr_wb_dat$sr_dat <-
      sr_wb_dat$sr_dat[which(names(sr_wb_dat$sr_dat) %in% allowable_stressors)]

  }


  # Stressor Response
  stressors <- sr_wb_dat$stressor_names

  # Re-create the main sheet object
  main.sheet <- sr_wb_dat$main_sheet

  # Get the stressor names
  stressor.list <- sr_wb_dat$sr_dat


  # Run the Joe curves to generate the response functions
  mean.resp.list <- mean_Response(
    n.stressors = nrow(sr_wb_dat$main_sheet),
    str.list = sr_wb_dat$sr_dat,
    main = sr_wb_dat$main_sheet
  )


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



  # Loop through HUCs and stressors

  for (i in 1:length(hucs)) {
    for (j in 1:length(stressors)) {
      # find combination of HUC and stressor in the dose table
      pnt.dose <- intersect(grep(hucs[i], dose$HUC_ID),
                            grep(stressors[j], dose$Stressor))

      # If nothing set - assume system capacity is 100%
      if (length(pnt.dose) == 0) {
        sys.capacity[i, j,] <- 1 # System capacity is 1 if mssing
        dose.values[i, j,] <- NA
        dose.values.list[i, j][[1]] <- NA
        next
      }

      # find stressor in main.sheet for relationship
      pnt.curv <- grep(stressors[j], main.sheet$Stressors)

      # call system capacity function for each stressor
      temp.list <- SystemCapacity(
        f.dose.df = dose[pnt.dose,],
        f.main.df = main.sheet[pnt.curv,],
        f.stressor.df = stressor.list[[stressors[j]]],
        f.mean.resp.list = mean.resp.list[[pnt.curv]],
        n.sims = MC_sims
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
      HUC = .data$Var1,
      Stressor = .data$Var2,
      simulation = .data$Var3,
      sys.cap = .data$value
    )

  # do same for dose values
  dose.df <-
    dplyr::rename(
      dose.df,
      HUC = .data$Var1,
      Stressor = .data$Var2,
      simulation = .data$Var3,
      dose = .data$value
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
      sc.dose.df <-
        interaction_matrix_sys_cap(MInt, sc.dose.df = sc.dose.df,
                                   adult_sys_cap = adult_sys_cap)

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
    dplyr::group_modify( ~ ce.func(.x))

  ce.df <- ce_df_raw




  # Build return object
  return_list <- list()
  return_list$ce.df <- ce.df
  return_list$sc.dose.df <- sc.dose.df

  return(return_list)
}
