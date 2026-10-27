#' pop_model_dat_clean
#' @details Cleans input parameters from csv.
#'
#' Fecundity (non-anadromous): per-stage rows (eps_1, eps_2, ...) are the
#' canonical format and are used directly by the population model. A legacy
#' single "eps" row is spread across stages based on maturity - mature
#' stages (mat_x > 0) receive the legacy value and immature stages get 0 -
#' and the legacy row is then removed, so cleaned data always carries
#' per-stage eps rows. If both formats are present, the per-stage values
#' win and the legacy value only fills missing mature-stage rows.
#'
#' @param dat Data frame of life history parameters.
#' @param nstage_fill Numeric. Number of stages to fill out to.
#'
#' @export
pop_model_dat_clean <- function(dat, nstage_fill = NULL) {

  # ===========================================
  # Create function to fix and clean dat object
  # before running population model
  # ===========================================

  # Drop any extra columns - clean column order input
  # dat <- life_stages

  # Validate required columns exist
  required_cols <- c("Parameters", "Name", "Value")
  missing_cols <- setdiff(required_cols, colnames(dat))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Life cycle CSV is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ". Found columns: ",
      paste(colnames(dat), collapse = ", ")
    ))
  }

  dat <- dat[, required_cols]

  # Check that Nstage row exists
  if (!("Nstage" %in% dat$Name)) {
    stop("Life cycle CSV is missing the 'Nstage' row (in the 'Name' column). This parameter is required.")
  }

  # Fix vectors to match Nstage
  n_stage <- as.numeric(dat$Value[dat$Name == "Nstage"])

  if(length(nstage_fill) == 0) {
    nstage_fill <- n_stage
  }

  if(length(n_stage) == 0) { stop("Nstage must be set. Ensure your CSV has a row with Name='Nstage' and a numeric Value.") }
  if(is.na(n_stage)) { stop("Nstage value is NA. Ensure the Value for the 'Nstage' row is a whole number (e.g., 3, 4, 5).") }
  if(n_stage < 2) { stop(paste0("Nstage must be 2 or greater, but got ", n_stage, ".")) }
  if(n_stage > 10) {
    stop(paste0(
      "Nstage must be 10 or fewer (the life cycle model supports between ",
      "2 and 10 stages), but got ", n_stage, "."
    ))
  }

  # Assume anadromous if not set then assume life history is not anadromous
  if(length(dat$Value[dat$Name == "anadromous"]) == 0) {
    add_row <- data.frame(Parameters = "Anadromous",
                          Name = "anadromous",
                          Value = FALSE)
    dat <- rbind(dat, add_row)
  }
  if(is.na(dat$Value[dat$Name == "anadromous"])) {
    dat$Value[dat$Name == "anadromous"] <- FALSE
  }

  anadromous <- dat$Value[dat$Name == "anadromous"]

  if(anadromous == "FALSE") {
    anadromous <- FALSE
    dat$Value[dat$Name == "anadromous"] <- 0
  }
  if(anadromous == "TRUE") {
    anadromous <- TRUE
    dat$Value[dat$Name == "anadromous"] <- 1
  }


  if(anadromous == 0) {
    anadromous <- FALSE
    dat$Value[dat$Name == "anadromous"] <- 0
  }
  if(anadromous == 1) {
    anadromous <- TRUE
    dat$Value[dat$Name == "anadromous"] <- 1
  }


  # If "TRUE" then 1; if "FALSE" then 0
  dat$Value <- ifelse(dat$Value == "TRUE", 1, dat$Value)
  dat$Value <- ifelse(dat$Value == "FALSE", 0, dat$Value)


  # Strip thousands separators (e.g. "2,315" from a formatted Excel
  # export) so they parse as numbers instead of silently becoming NA.
  # Commas are removed only where the de-commaed string is a valid
  # number; anything else is left untouched for the coercion below.
  val_chr <- trimws(as.character(dat$Value))
  val_nocomma <- gsub(",", "", val_chr, fixed = TRUE)
  has_comma <- !is.na(val_chr) & grepl(",", val_chr, fixed = TRUE)
  is_num <- !is.na(suppressWarnings(as.numeric(val_nocomma)))
  dat$Value[has_comma & is_num] <- val_nocomma[has_comma & is_num]

  # Make sure remaining rows are numeric
  dat$Value <- as.numeric(as.character(dat$Value))

  # Fix or adjust adult capacity - make sure it is set
  if(length(dat$Value[dat$Name == "k"]) == 0) {
    add_row <- data.frame(Parameters = "Adult capacity",
                          Name = "k",
                          Value = 1000)
    dat <- rbind(dat, add_row)
  }

  if(is.na(dat$Value[dat$Name == "k"])) {
    dat$Value[dat$Name == "k"] <- 1000
  }


  # Loop through allowable ages and rebuild df


  #================================================
  # Probability of catastrophy p.cat
  #================================================
  # print("Add rules for p.cat")

  if("p.cat" %in% dat$Name) {
    # p.cat is present - make sure it's run-able
    # can't be NA
    if(is.na(dat$Value[dat$Name == "p.cat"])) {
      dat$Value[dat$Name == "p.cat"] <- 0
    }
    # Can't be negative
    if(dat$Value[dat$Name == "p.cat"] < 0) {
      dat$Value[dat$Name == "p.cat"] <- 0
    }
    # can't be greater than 1
    if(dat$Value[dat$Name == "p.cat"] >= 1) {
      dat$Value[dat$Name == "p.cat"] <- 0.999
    }
  } else {
    # p.cat is absent - assume it is zero
    add_row <- data.frame(Parameters = "p.cat",
                          Name = "p.cat",
                          Value = 0)
    dat <- rbind(dat, add_row)
  }

  #================================================
  # Survival surv_
  #================================================

  # Survival
  all_surv <- dat$Name[grepl("^surv_", dat$Name)]
  surv_keep <- paste0("surv_", 1:n_stage)
  surv_drop <- all_surv[!all_surv %in% surv_keep]

  # Drop rows that are out of range
  if(length(surv_drop) > 0) {
    dat <- dat[which(!(dat$Name %in% surv_drop)), ]
  }

  # Add missing surv_ inputs with default value
  surv_missing <- surv_keep[!(surv_keep %in% dat$Name)]

  if(length(surv_missing) > 0) {
    mfill <- surv_missing
    add_row <- data.frame(Parameters = mfill,
                          Name = mfill,
                          Value = 0.999)
    dat <- rbind(dat, add_row)
  }

  # Extend missing values
  if(nstage_fill > n_stage) {
    surv_fill <-
      paste0("surv_", (n_stage + 1):nstage_fill)
    add_row <- data.frame(Parameters = surv_fill,
                          Name = surv_fill,
                          Value = 0)
    dat <- rbind(dat, add_row)
  }


  #================================================
  # Maturity mat_
  #================================================

  # Maturity
  all_mat <- dat$Name[grepl("^mat_", dat$Name)]
  mat_keep <- paste0("mat_", 1:n_stage)
  mat_drop <- all_mat[!all_mat %in% mat_keep]

  # Drop rows that are out of range
  if(length(mat_drop) > 0) {
    dat <- dat[which(!(dat$Name %in% mat_drop)), ]
  }

  # Add missing inputs with default value
  mat_missing <- mat_keep[!(mat_keep %in% dat$Name)]

  if(length(mat_missing) > 0) {
    mfill <- mat_missing
    # Assume missing rows are not mature
    add_row <- data.frame(Parameters = mfill,
                          Name = mfill,
                          Value = 0)
    dat <- rbind(dat, add_row)
  }


  if(nstage_fill > n_stage) {
    m_fill <-
      paste0("mat_", (n_stage + 1):nstage_fill)
    add_row <- data.frame(Parameters = m_fill,
                          Name = m_fill,
                          Value = 0)
    dat <- rbind(dat, add_row)
  }


  #================================================
  # Time in stage year_
  #================================================

  # Years in stage
  all_year  <- dat$Name[grepl("^year_", dat$Name)]
  year_keep <- paste0("year_", 1:n_stage)
  year_drop <- all_year[!all_year %in% year_keep]

  # Drop rows that are out of range
  if(length(year_drop) > 0) {
    dat <- dat[which(!(dat$Name %in% year_drop)), ]
  }

  # Add missing inputs with default value
  year_missing <- year_keep[!(year_keep %in% dat$Name)]

  if(length(year_missing) > 0) {
    mfill <- year_missing
    # Assume one year in each stage
    add_row <- data.frame(Parameters = mfill,
                          Name = mfill,
                          Value = 1)
    dat <- rbind(dat, add_row)
  }

  if(nstage_fill > n_stage) {
    m_fill <-
      paste0("year_", (n_stage + 1):nstage_fill)
    add_row <- data.frame(Parameters = m_fill,
                          Name = m_fill,
                          Value = 1)
    dat <- rbind(dat, add_row)
  }


  #================================================
  # Eggs per spawner eps_
  #================================================

  # (esp) ------------------------------------
  # eps single value or vector
  if(anadromous) {

    # Anadromous
    # eps (regex rather than a fixed 1:10 list so out-of-range rows like
    # eps_11 are removed for any stage count)
    all_eps <- dat$Name[grepl("^eps_[0-9]+$", dat$Name)]
    eps_keep <- paste0("eps_", 1:n_stage)
    eps_drop <- all_eps[!all_eps %in% eps_keep]

    # Drop rows that are out of range
    if(length(eps_drop) > 0) {
      dat <- dat[which(!(dat$Name %in% eps_drop)), ]
    }

    # Add missing inputs with default value
    eps_missing <- eps_keep[!(eps_keep %in% dat$Name)]

    # If a legacy single "eps" row is present, use its value as the fill
    # default (mirrors the non-anadromous legacy handling below).
    # pop_model_setup_anadromous() only falls back to the legacy "eps" row
    # when eps_x entries are absent (NA), not zero - so filling with 0 here
    # would silently zero fecundity for legacy-format anadromous files.
    legacy_eps <- suppressWarnings(as.numeric(dat$Value[dat$Name == "eps"]))
    legacy_eps <- legacy_eps[!is.na(legacy_eps)]
    eps_fill <- if (length(legacy_eps) > 0) legacy_eps[1] else 0

    if(length(eps_missing) > 0) {
      mfill <- eps_missing
      # Assume no eps if missing (unless a legacy single eps value is given)
      add_row <- data.frame(Parameters = mfill,
                            Name = mfill,
                            Value = eps_fill)
      dat <- rbind(dat, add_row)
    }


    if(nstage_fill > n_stage) {
      m_fill <-
        paste0("eps_", (n_stage + 1):nstage_fill)
      add_row <- data.frame(Parameters = m_fill,
                            Name = m_fill,
                            Value = eps_fill)
      dat <- rbind(dat, add_row)
    }

  } else {

    # Non-anadromous
    # Per-stage rows (eps_1, eps_2, ...) are the canonical format. A legacy
    # single "eps" row is spread across stages based on maturity: mature
    # stages (mat_x > 0) receive the legacy value, immature stages get 0.
    # The legacy row is then removed so all downstream code sees per-stage
    # rows only. (mat_x rows are guaranteed present by this point - the
    # maturity section above runs first.)

    has_eps_x <- any(grepl("^eps_[0-9]+$", dat$Name))

    legacy_eps <- suppressWarnings(as.numeric(dat$Value[dat$Name == "eps"]))
    legacy_eps <- legacy_eps[!is.na(legacy_eps)]
    legacy_eps <- if (length(legacy_eps) > 0) legacy_eps[1] else NA

    # Maturity lookup for the mat-dependent spread/fill below
    get_mat <- function(i) {
      m <- suppressWarnings(as.numeric(dat$Value[dat$Name == paste0("mat_", i)]))
      if (length(m) == 0 || is.na(m[1])) 0 else m[1]
    }

    if (!has_eps_x) {

      # Legacy format only: spread the single eps value across the mature
      # stages (mat-dependent); immature stages get eps of 0.
      add_names <- paste0("eps_", 1:nstage_fill)
      add_vals <- sapply(1:nstage_fill, function(i) {
        if (!is.na(legacy_eps) && get_mat(i) > 0) legacy_eps else 0
      })
      add_row <- data.frame(Parameters = add_names,
                            Name = add_names,
                            Value = add_vals)
      dat <- rbind(dat, add_row)

    } else {

      # Per-stage rows exist: drop out-of-range rows, then fill any gaps
      # (legacy value for mature stages if available, otherwise 0)
      all_eps <- dat$Name[grepl("^eps_[0-9]+$", dat$Name)]
      eps_keep <- paste0("eps_", 1:n_stage)
      eps_drop <- all_eps[!all_eps %in% eps_keep]

      # Drop rows that are out of range
      if(length(eps_drop) > 0) {
        dat <- dat[which(!(dat$Name %in% eps_drop)), ]
      }

      # Add missing inputs with default value
      eps_missing <- eps_keep[!(eps_keep %in% dat$Name)]

      if(length(eps_missing) > 0) {
        miss_i <- as.numeric(gsub("eps_", "", eps_missing))
        miss_vals <- sapply(miss_i, function(i) {
          if (!is.na(legacy_eps) && get_mat(i) > 0) legacy_eps else 0
        })
        add_row <- data.frame(Parameters = eps_missing,
                              Name = eps_missing,
                              Value = miss_vals)
        dat <- rbind(dat, add_row)
      }

      if(nstage_fill > n_stage) {
        m_fill <-
          paste0("eps_", (n_stage + 1):nstage_fill)
        add_row <- data.frame(Parameters = m_fill,
                              Name = m_fill,
                              Value = 0)
        dat <- rbind(dat, add_row)
      }

      if (!is.na(legacy_eps)) {
        message(
          "Life cycles file has both a legacy 'eps' row and per-stage ",
          "eps_x rows; using the per-stage values (the legacy value only ",
          "fills missing mature-stage rows)."
        )
      }
    } # end of non-anadromous eps alt case handling

    # Remove the legacy single-eps row: downstream code is per-stage only
    dat <- dat[which(!(dat$Name == "eps")), ]

  } # end of if anadormous eps

  # ================================================
  # (u) Prespawn mortality ------------------------------------
  # ================================================

  if(anadromous) {

    # Anadromous
    # Prespawn mortality
    all_u <- dat$Name[grepl("^u_", dat$Name)]
    u_keep <- paste0("u_", 1:n_stage)
    u_drop <- all_u[!all_u %in% u_keep]

    # Drop rows that are out of range
    if(length(u_drop) > 0) {
      dat <- dat[which(!(dat$Name %in% u_drop)), ]
    }

    # Add missing inputs with default value
    u_missing <- u_keep[!(u_keep %in% dat$Name)]

    if(length(u_missing) > 0) {
      mfill <- u_missing
      # Assume surv is 100%
      add_row <- data.frame(Parameters = mfill,
                            Name = mfill,
                            Value = 1)
      dat <- rbind(dat, add_row)
    }

    if(nstage_fill > n_stage) {
      m_fill <-
        paste0("u_", (n_stage + 1):nstage_fill)
      add_row <- data.frame(Parameters = m_fill,
                            Name = m_fill,
                            Value = 1)
      dat <- rbind(dat, add_row)
    }

  } else {

    # Drop empty values (regex, not a fixed 1:10 list, so any stray u_X
    # row is removed regardless of stage count)
    u_drop <- dat$Name[grepl("^u_[0-9]+$", dat$Name)]
    dat <- dat[which(!(dat$Name %in% u_drop)), ]

    # non-anadromous add empty values
    if(nstage_fill > n_stage) {

      m_fill <-
        paste0("u_", 1:nstage_fill)
      add_row <- data.frame(Parameters = m_fill,
                            Name = m_fill,
                            Value = 1)
      dat <- rbind(dat, add_row)
    }

  }




  # (smig_) spawner migration mortality -----------------------
  if(anadromous) {
    all_smig <- dat$Name[grepl("^smig_", dat$Name)]
    smig_keep <- paste0("smig_", 1:n_stage)
    smig_drop <- all_smig[!all_smig %in% smig_keep]

    # Drop rows that are out of range
    if(length(smig_drop) > 0) {
      dat <- dat[which(!(dat$Name %in% smig_drop)), ]
    }

    # Add missing inputs with default value
    smig_missing <- smig_keep[!(smig_keep %in% dat$Name)]

    if(length(smig_missing) > 0) {
      mfill <- smig_missing
      # Assume no eps if missing
      add_row <- data.frame(Parameters = mfill,
                            Name = mfill,
                            Value = 1)
      dat <- rbind(dat, add_row)
    }

    if(nstage_fill > n_stage) {
      m_fill <-
        paste0("smig_", (n_stage + 1):nstage_fill)
      add_row <- data.frame(Parameters = m_fill,
                            Name = m_fill,
                            Value = 1)
      dat <- rbind(dat, add_row)
    }

  } else {

    # Drop empty values (regex, not a fixed 1:10 list, so any stray
    # smig_X row is removed regardless of stage count)
    smig_drop <- dat$Name[grepl("^smig_[0-9]+$", dat$Name)]
    dat <- dat[which(!(dat$Name %in% smig_drop)), ]

    # non-anadromous add empty values
    if(nstage_fill > n_stage) {

      m_fill <-
        paste0("smig_", 1:nstage_fill)
      add_row <- data.frame(Parameters = m_fill,
                            Name = m_fill,
                            Value = 1)
      dat <- rbind(dat, add_row)
    }
  }


  # Compensation ratios (cr) ==================================

  all_cr <- dat$Name[grepl("^cr_", dat$Name)]
  cr_keep <- paste0("cr_", 1:n_stage)
  cr_keep <- c("cr_E", "cr_0", cr_keep)
  cr_drop <- all_cr[!all_cr %in% cr_keep]

  # Drop rows that are out of range
  if(length(cr_drop) > 0) {
    dat <- dat[which(!(dat$Name %in% cr_drop)), ]
  }

  # Add missing inputs with default value
  cr_missing <- cr_keep[!(cr_keep %in% dat$Name)]

  if(length(cr_missing) > 0) {
    mfill <- cr_missing
    # Assume no eps if missing
    add_row <- data.frame(Parameters = mfill,
                          Name = mfill,
                          Value = 1)
    dat <- rbind(dat, add_row)
  }


  if(nstage_fill > n_stage) {
    m_fill <-
      paste0("cr_", (n_stage + 1):nstage_fill)
    add_row <- data.frame(Parameters = m_fill,
                          Name = m_fill,
                          Value = 1)
    dat <- rbind(dat, add_row)
  }

  return(dat)

}
