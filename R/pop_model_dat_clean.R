#' pop_model_dat_clean
#' @details Cleans input parameters from csv.
#'
#' @keywords internal
pop_model_dat_clean <- function(dat, nstage_fill = NULL) {

  # ===========================================
  # Create function to fix and clean dat object
  # before running population model
  # ===========================================

  # Drop any extra columns - clean column order input
  # dat <- life_stages
  dat <- dat[, c("Parameters", "Name", "Value")]

  # Fix vectors to match Nstage
  n_stage <- as.numeric(dat$Value[dat$Name == "Nstage"])

  if(length(nstage_fill) == 0) {
    nstage_fill <- n_stage
  }

  if(length(n_stage) == 0) { stop("Nstage must be set...") }
  if(is.na(n_stage)) { stop("Nstage cannot be NA...") }
  if(n_stage < 2) { stop("Nstage must be 2 or greater...") }

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
    # eps
    all_eps <- paste0("eps_", 1:10)
    eps_keep <- paste0("eps_", 1:n_stage)
    eps_drop <- all_eps[!all_eps %in% eps_keep]

    # Drop rows that are out of range
    if(length(eps_drop) > 0) {
      dat <- dat[which(!(dat$Name %in% eps_drop)), ]
    }

    # Add missing inputs with default value
    eps_missing <- eps_keep[!(eps_keep %in% dat$Name)]

    if(length(eps_missing) > 0) {
      mfill <- eps_missing
      # Assume no eps if missing
      add_row <- data.frame(Parameters = mfill,
                            Name = mfill,
                            Value = 0)
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

  } else {

    # Non-anadromous
    # Make sure eps is defined for each age class (eps_1, eps_2, etc.)...
    # not just eps (legacy)

    if(length(dat$Value[dat$Name == "eps"]) == 1 & length(grepl("eps_", dat$Name) == 1)) {

      eps_single <- as.numeric(dat$Value[dat$Name == "eps"])

      # Drop existing eps row
      dat <- dat[which(!(dat$Name == "eps")), ]

      # Create new rows for each age class
      add_rows <- data.frame(Parameters = paste0("eps_", 1:n_stage),
                             Name = paste0("eps_", 1:n_stage),
                             Value = eps_single)
      dat <- rbind(dat, add_rows)

      # Add additional rows to fill inputs
      if(nstage_fill > n_stage) {
        m_fill <-
          paste0("eps_", (n_stage + 1):nstage_fill)
        add_row <- data.frame(Parameters = m_fill,
                              Name = m_fill,
                              Value = eps_single)
        dat <- rbind(dat, add_row)
      }

    } else {

      # If eps is defined for each age class
      # and population is non-anadromous
      all_eps <- paste0("eps_", 1:10)
      eps_keep <- paste0("eps_", 1:n_stage)
      eps_drop <- all_eps[!all_eps %in% eps_keep]

      # Drop rows that are out of range
      if(length(eps_drop) > 0) {
        dat <- dat[which(!(dat$Name %in% eps_drop)), ]
      }

      # Add missing inputs with default value
      eps_missing <- eps_keep[!(eps_keep %in% dat$Name)]

      if(length(eps_missing) > 0) {
        mfill <- eps_missing
        # Assume no eps if missing
        add_row <- data.frame(Parameters = mfill,
                              Name = mfill,
                              Value = 0)
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
    } # end of non-anadromous eps alt case handling
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

    # Drop empty values
    u_drop <- paste0("u_", 1:10)
    dat <- dat[which(!(dat$Name %in% u_drop)), ]

    # non-anadromous add empty values
    if(nstage_fill > n_stage) {

      u_drop <- paste0("u_", 1:nstage_fill)
      dat <- dat[which(!(dat$Name %in% u_drop)), ]

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

    # Drop empty values
    u_drop <- paste0("smig_", 1:10)
    dat <- dat[which(!(dat$Name %in% u_drop)), ]

    # non-anadromous add empty values
    if(nstage_fill > n_stage) {

      u_drop <- paste0("smig_", 1:nstage_fill)
      dat <- dat[which(!(dat$Name %in% u_drop)), ]

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
