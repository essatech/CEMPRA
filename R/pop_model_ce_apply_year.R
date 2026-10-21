#' Align per-year stressor sys.cap values to the internal rate lists
#'
#' The stochastic rate lists built in Projection_DD() have Nyears + 1 slots:
#' slot 1 is the initialization ("year 0") entry and slot t + 1 holds
#' simulation year t. CE_df carries one sys.cap value per simulation year
#' (Year = 1..Nyears). This helper returns a vector of length n_slots with
#' slot 1 carrying Year 1's value and slot t + 1 carrying Year t's value,
#' matching on the Year column when available (never on row order).
#'
#' History: the year vector was previously padded at the END
#' (`c(sys.cap, sys.cap[1])`), which applied Year t+1's magnitude in
#' simulation year t and wrapped Year 1's magnitude onto the final
#' simulated year; length mismatches were then "fixed" with
#' sample(replace = TRUE), randomly shuffling the temporal sequence.
#' Constant (single-value) stressors were and are unaffected.
#'
#' Ambiguous duplicate rows (same key, conflicting values, no year
#' information to distinguish them) are a hard error: the previous
#' behaviours (random resample, then in-order recycling) both silently
#' invented an answer the input does not contain. Users who want two
#' effects on the same life stage should use two distinctly named
#' stressors - their effects multiply.
#'
#' @param trow Data frame of CE_df rows for one stressor / life stage /
#'   parameter / HUC / simulation combination.
#' @param n_slots Length of the target rate list (Nyears + 1). May be 0
#'   (e.g. the anadromous-only ut/smigt lists in a non-anadromous run).
#' @noRd
#' @keywords internal
align_sys_caps <- function(trow, n_slots) {
  if (n_slots == 0) {
    return(numeric(0))
  }
  v <- trow$sys.cap
  n_years <- n_slots - 1

  # Single value = constant stressor: applies to every slot
  if (length(v) == 1) {
    return(rep(v, n_slots))
  }

  key_txt <- paste0(
    "stressor '", trow$stressor[1], "', parameter '", trow$parameter[1],
    "', life stage '", trow$life_stage[1], "'"
  )

  # A usable Year column takes precedence: even identical values may cover
  # only a subset of years (remaining years are unstressed)
  if ("Year" %in% names(trow) && !all(is.na(trow$Year))) {
    yr <- suppressWarnings(as.numeric(trow$Year))

    # Conflicting values within a single year are just as ambiguous
    ok <- !is.na(yr)
    per_year_conflict <- vapply(
      split(v[ok], yr[ok]),
      function(x) length(unique(x)) > 1,
      logical(1)
    )
    if (any(per_year_conflict)) {
      stop(
        "CE_df contains multiple conflicting sys.cap values for ",
        key_txt, " within the same simulation year. Provide one value ",
        "per year. If two separate effects should apply to the same ",
        "life stage, use two distinctly named stressors (their effects ",
        "multiply).",
        call. = FALSE
      )
    }

    idx <- match(seq_len(n_years), yr)
    if (all(is.na(idx))) {
      stop(
        "CE_df has a 'Year' column for ", key_txt, " but none of its ",
        "values match the simulation years 1..", n_years, ". Year must ",
        "index simulation years, not calendar years.",
        call. = FALSE
      )
    }

    yearly <- v[idx]
    if (anyNA(yearly)) {
      warning(
        "CE_df is missing sys.cap values for some simulation years ",
        "(", key_txt, "); those years are treated as unstressed ",
        "(sys.cap = 1).",
        call. = FALSE
      )
      yearly[is.na(yearly)] <- 1
    }
    return(c(yearly[1], yearly))
  }

  # No year information. Multiple rows that all agree (e.g. the same alias
  # listed twice in a comma list, or duplicated identical rows) are
  # unambiguous: apply the shared value once, everywhere
  if (length(unique(v)) == 1) {
    return(rep(v[1], n_slots))
  }

  # Conflicting values with no year information: refuse rather than guess
  stop(
    "CE_df contains multiple conflicting sys.cap values for ", key_txt,
    " with no 'Year' column to distinguish them. Provide a single row ",
    "per stressor / life stage / parameter, or add a 'Year' column ",
    "(simulation years 1..", n_years, "). If two separate effects should ",
    "apply to the same life stage, use two distinctly named stressors ",
    "(their effects multiply).",
    call. = FALSE
  )
}


#' Map a normalized CE life_stage token to its canonical vital-rate target
#'
#' Several life_stage tokens alias the same underlying vital rate: for
#' example "s1", "stage1" and "surv1" all target stage-1 survival, and
#' "b3", "spawn3" and "spawners3" all target the age-3 spawner capacity.
#' This helper resolves each (token, parameter) pair to a canonical target
#' identifier so duplicate targeting within one stressor can be detected.
#' The alias sets mirror the application blocks in
#' pop_model_ce_apply_year() exactly - if new aliases are added to the
#' blocks, they must be added here too. Returns NA for unrecognized tokens
#' (those are warned about separately and never applied).
#'
#' @param life_stage Character vector of normalized life_stage tokens
#'   (lowercase, spaces and underscores stripped).
#' @param parameter Character vector of normalized parameter values.
#' @param anadromous Logical; "adult" aliases differ by model mode
#'   (anadromous: prespawn survival / spawner capacity).
#' @noRd
#' @keywords internal
canonical_ce_target <- function(life_stage, parameter, anadromous) {
  ss_rng <- 1:12
  rep12 <- function(prefix, target_prefix) {
    stats::setNames(paste0(target_prefix, ss_rng), paste0(prefix, ss_rng))
  }
  surv_map <- c(
    stagee = "sE", se = "sE", egg = "sE", eggs = "sE",
    stageegg = "sE", stageeggs = "sE",
    stage0 = "s0", s0 = "s0", so = "s0", fry = "s0",
    surv = "s_all", all = "s_all",
    u = "u_all", prespawn = "u_all", spawners = "u_all",
    smig = "smig_all", spawnmig = "smig_all",
    adult = if (anadromous) "u_all" else "s_adult",
    rep12("stage", "s"), rep12("s", "s"), rep12("surv", "s"),
    rep12("stagepb", "s"),
    rep12("u", "u"), rep12("prespawn", "u"),
    rep12("smig", "smig"), rep12("spawnmig", "smig")
  )
  fec_map <- c(
    eps = "eps_all", stageb = "eps_all", spawners = "eps_all",
    adults = "eps_all",
    rep12("eps", "eps"), rep12("stageb", "eps")
  )
  cap_map <- c(
    se = "Ke", stagee = "Ke", egg = "Ke", eggs = "Ke",
    stageegg = "Ke", stageeggs = "Ke", ke = "Ke",
    s0 = "K0", so = "K0", stage0 = "K0", fry = "K0", k0 = "K0",
    spawner = "Kspawners", spawners = "Kspawners", stageb = "Kspawners",
    spawn = "Kspawners",
    all = "K_all",
    adult = if (anadromous) "Kspawners" else "K_adult",
    rep12("stage", "K"), rep12("s", "K"), rep12("k", "K"),
    rep12("stagepb", "K"), rep12("p", "K"),
    rep12("spawn", "KB"), rep12("spawners", "KB"), rep12("b", "KB"),
    rep12("stageb", "KB"), rep12("spawner", "KB")
  )
  out <- rep(NA_character_, length(life_stage))
  i <- which(parameter == "survival")
  out[i] <- surv_map[life_stage[i]]
  i <- which(parameter == "fecundity")
  out[i] <- fec_map[life_stage[i]]
  i <- which(parameter == "capacity")
  out[i] <- cap_map[life_stage[i]]
  unname(out)
}


#' Known CE life-stage tags per model mode
#'
#' Internal single source of truth for the accepted (normalized)
#' life_stage tokens per parameter. Salmon-only tokens - pre-spawn (u),
#' migration (smig) survival, pre-breeder (pb) stages and age-specific
#' breeder/spawner capacity - are excluded in non-anadromous mode, where
#' they would be silent no-ops.
#'
#' @noRd
#' @keywords internal
ce_known_tags <- function(anadromous) {
  ss_rng <- 1:12
  known_fecund <- c("eps", "stageb", "spawners", "adults",
                    paste0("eps", ss_rng), paste0("stageb", ss_rng))
  known_surv <- c("stagee", "se", "egg", "eggs", "stageegg", "stageeggs",
                  "stage0", "s0", "so", "fry", "surv", "all", "adult",
                  "u", "prespawn", "spawners", "smig", "spawnmig",
                  paste0("stage", ss_rng), paste0("s", ss_rng),
                  paste0("surv", ss_rng), paste0("stagepb", ss_rng),
                  paste0("u", ss_rng), paste0("prespawn", ss_rng),
                  paste0("smig", ss_rng), paste0("spawnmig", ss_rng))
  known_cap <- c("se", "stagee", "egg", "eggs", "stageegg", "stageeggs", "ke",
                 "s0", "so", "stage0", "fry", "k0", "adult", "all",
                 "spawner", "spawners", "stageb", "spawn",
                 paste0("stage", ss_rng), paste0("s", ss_rng),
                 paste0("k", ss_rng), paste0("stagepb", ss_rng),
                 paste0("p", ss_rng),
                 paste0("spawn", ss_rng), paste0("spawners", ss_rng),
                 paste0("b", ss_rng), paste0("stageb", ss_rng),
                 paste0("spawner", ss_rng))

  if (!anadromous) {
    anad_only_surv <- c("u", "prespawn", "spawners", "smig", "spawnmig",
                        paste0("stagepb", ss_rng),
                        paste0("u", ss_rng), paste0("prespawn", ss_rng),
                        paste0("smig", ss_rng), paste0("spawnmig", ss_rng))
    known_surv <- setdiff(known_surv, anad_only_surv)
    anad_only_cap <- c(paste0("stagepb", ss_rng), paste0("p", ss_rng),
                       paste0("spawn", ss_rng), paste0("spawners", ss_rng),
                       paste0("b", ss_rng), paste0("stageb", ss_rng),
                       paste0("spawner", ss_rng))
    known_cap <- setdiff(known_cap, anad_only_cap)
  }

  list(fecund = known_fecund, surv = known_surv, cap = known_cap)
}


#' Validate CE life-stage / parameter linkages for a model mode
#'
#' @description Checks whether stressor-response Life_stages / Parameters
#' linkages are recognized by the population model in the given mode.
#' Used by the Shiny app to flag badly-coded stressor-response rows
#' before a run (unrecognized linkages are silent no-ops in the model).
#'
#' @param life_stage Character vector of Life_stages values as they appear
#' in the workbook (comma-separated lists allowed, e.g. "stage_1, stage_2").
#' @param parameter Character vector of Parameters values ("survival",
#' "capacity", "fecundity"; "survivorship" is accepted as an alias).
#' @param anadromous Logical. Model mode: salmon-only tags (u/prespawn,
#' smig, pre-breeder stages, age-specific spawner capacity) are only valid
#' when TRUE.
#'
#' @returns A character vector the same length as the inputs: "" when the
#' linkage is recognized, otherwise a short plain-language problem
#' description naming the offending tag.
#'
#' @export
pop_model_ce_validate <- function(life_stage, parameter, anadromous = FALSE) {
  known <- ce_known_tags(isTRUE(anadromous))
  n <- max(length(life_stage), length(parameter))
  life_stage <- rep_len(as.character(life_stage), n)
  parameter <- rep_len(as.character(parameter), n)

  out <- character(n)
  for (i in seq_len(n)) {
    par_i <- tolower(trimws(parameter[i]))
    if (is.na(par_i) || par_i == "" || par_i == "na") {
      next # Joe-Model-only rows (no parameter) are fine
    }
    if (par_i == "survivorship") par_i <- "survival"
    if (!(par_i %in% c("survival", "capacity", "fecundity"))) {
      out[i] <- paste0("unrecognized parameter '", parameter[i], "'")
      next
    }
    known_set <- switch(par_i,
                        survival = known$surv,
                        capacity = known$cap,
                        fecundity = known$fecund)
    tokens <- strsplit(as.character(life_stage[i]), ",")[[1]]
    tokens <- gsub("[ _]", "", tolower(trimws(tokens)))
    tokens <- tokens[!(is.na(tokens)) & tokens != ""]
    if (length(tokens) == 0) {
      out[i] <- paste0("blank life stage tag (parameter '", par_i, "')")
      next
    }
    bad <- tokens[!(tokens %in% known_set)]
    if (length(bad) > 0) {
      out[i] <- paste0(
        "life stage tag '", paste(bad, collapse = "', '"),
        "' is not recognized for ", par_i,
        if (!isTRUE(anadromous)) " in a non-anadromous model" else "",
        ""
      )
    }
  }
  out
}


#' Population Model CE Application to Vital Rates Year-Specific
#'
#' @description Utility function to apply cumulative effects to vital rates.
#'
#' @details Per-year sys.cap values are matched to simulation years via the
#' Year column of \code{CE_df} (slot 1 of each internal rate list is the
#' initialization entry and carries Year 1's value; slot t + 1 carries
#' simulation year t's value). Stressors supplied as a single row (no Year
#' information) are treated as constant and applied to every year. Years
#' missing from \code{CE_df} are treated as unstressed (sys.cap = 1) with
#' a warning. Each vital rate may be targeted at most once per stressor:
#' life_stage tokens are resolved to their underlying vital rate (so
#' aliases such as "s1", "stage_1" and "surv_1" are recognized as the same
#' target), and duplicate targeting - via aliases, a repeated token, or
#' duplicate rows - is an error. To apply two separate effects to the same
#' life stage, create two distinctly named stressors (effects multiply).
#'
#' @param rates Life history vital rates list object.
#' @param CE_df Cumulative effects data frame.
#' @param anadromous Anadromous TRUE/FALSE.
#' @param mat_stage Named numeric vector of mature life stages.
#'
#' @returns a modified dat vital rates object after applying CE effects.
#'
#' @export
#'
pop_model_ce_apply_year <- function(
    rates = NULL,
    CE_df = NULL,
    anadromous = FALSE,
    mat_stage = NULL
    ) {

  # if CE df is null then skip and exit function...
  if (is.null(CE_df)) {
    return(rates)
  }
  if (all(is.na(CE_df))) {
    return(rates)
  }
  if (nrow(CE_df) == 0) {
    return(rates)
  }

  # For testing import temp CE_df
  # filename <- system.file("extdata/simple_test/CE_df_testing.csv", package = "CEMPRA")
  # CE_df <- read.csv(filename, stringsAsFactors = FALSE)


  #-----------------------------------------------
  # Create vector of life stages for application
  #-----------------------------------------------

  # Need to split up life stage linkages
  # many will be comma separated
  CE_df$life_stage <- tolower(CE_df$life_stage)

  # Remove spaces
  CE_df$life_stage <- gsub(" ", "", CE_df$life_stage)

  # and remove underscores throughout
  CE_df$life_stage <- gsub("_", "", CE_df$life_stage)
  CE_df$life_stage <- tolower(CE_df$life_stage)

  # Split CE_df and make unique rows for each life stage
  CE_df_rebuid <- list()

  for(i in 1:nrow(CE_df)) {
    trow <- CE_df[i, ]
    stages <- unlist(strsplit(trow$life_stage, ","))
    brow <- trow[rep(1, length(stages)), ]
    brow$life_stage <- stages
    CE_df_rebuid[[i]] <- brow
  }
  CE_df_rebuid <- do.call("rbind", CE_df_rebuid)

  # overwrite original
  CE_df <- CE_df_rebuid

  # People mix up survival and survivorship
  CE_df$parameter <- ifelse(CE_df$parameter == "survivorship", "survival", CE_df$parameter)

  # Make sure simulation column is in the df
  if (!"simulation" %in% colnames(CE_df)) {
    CE_df$simulation <- 1
  }

  # Make sure HUC and stressor name is in DF and add it if not
  colnames(CE_df)[which(colnames(CE_df) == "Stressor")] <- "stressor"
  if (!"stressor" %in% colnames(CE_df)) {
    CE_df$stressor <- "stressor"
  }
  if (!"HUC" %in% colnames(CE_df)) {
    CE_df$HUC <- 1
  }

  # ============================================================================
  # Optional From_Year / To_Year activation window
  # ============================================================================
  # If From_Year / To_Year columns are supplied, a stressor is only active
  # between those simulation years (inclusive). Outside that window the
  # stressor has no effect, implemented by forcing sys.cap to 1 (the
  # multiplicative identity for survival, fecundity and capacity pathways).
  #   - both NA/missing  -> active all years (current behaviour)
  #   - From only        -> active for Year >= From_Year
  #   - To only          -> active for Year <= To_Year
  #   - From > To        -> empty window, inactive all years (+ warning)
  # When the columns are absent this block is skipped entirely, so existing
  # runs are unchanged.
  has_from <- "From_Year" %in% colnames(CE_df)
  has_to   <- "To_Year" %in% colnames(CE_df)

  if (has_from || has_to) {

    if (!("Year" %in% colnames(CE_df))) {
      # No year information available - cannot window. Leave rates unchanged
      # (stressor applied to all years) but warn so the user is aware.
      warning("From_Year/To_Year supplied but no 'Year' column in CE_df; activation window ignored.")
    } else {

      yr <- suppressWarnings(as.numeric(CE_df$Year))
      fy <- if (has_from) suppressWarnings(as.numeric(CE_df$From_Year)) else rep(NA_real_, nrow(CE_df))
      ty <- if (has_to)   suppressWarnings(as.numeric(CE_df$To_Year))   else rep(NA_real_, nrow(CE_df))

      # Warn on invalid (empty) windows where From_Year > To_Year
      if (any(!is.na(fy) & !is.na(ty) & fy > ty)) {
        warning("From_Year > To_Year for one or more stressors; those stressors are inactive in all years.")
      }

      # Years outside the activation window -> neutralize (no effect)
      out_window <- (!is.na(fy) & yr < fy) | (!is.na(ty) & yr > ty)
      out_window <- ifelse(is.na(out_window), FALSE, out_window)
      CE_df$sys.cap[out_window] <- 1

      # Downstream application matches sys.cap values on the Year column
      # (align_sys_caps), so row order no longer matters; keep the sort for
      # tidiness and for any external consumers of the intermediate object.
      CE_df <- CE_df[order(yr), ]
    }
  }


  # otherwise modify dat with CE stressors
  CE_df$parameter <- tolower(CE_df$parameter)
  # Repeat the survivorship alias fix post-tolower so "Survivorship" (any
  # case) maps to survival
  CE_df$parameter <- ifelse(CE_df$parameter == "survivorship", "survival", CE_df$parameter)

  # ============================================================================
  # Validate parameter and life_stage tokens
  # ============================================================================
  # Unrecognized values are silently skipped by the application blocks below
  # (a typo such as "stge_1" or an unsupported alias such as "age-1" would
  # have NO effect on the model) - warn so the user can fix the workbook.
  # The accepted-tag sets are MODE-DEPENDENT: salmon-only tags (pre-spawn
  # and migration survival, age-specific spawner capacity) are rejected in
  # non-anadromous runs, where they would be silent no-ops.
  known <- ce_known_tags(anadromous)
  known_fecund <- known$fecund
  known_surv <- known$surv
  known_cap <- known$cap

  par_v <- CE_df$parameter
  ls_v <- CE_df$life_stage
  bad <- rep(FALSE, nrow(CE_df))
  idx <- which(par_v == "fecundity")
  bad[idx] <- !(ls_v[idx] %in% known_fecund)
  idx <- which(par_v == "survival")
  bad[idx] <- !(ls_v[idx] %in% known_surv)
  idx <- which(par_v == "capacity")
  bad[idx] <- !(ls_v[idx] %in% known_cap)
  # Unknown (non-NA) parameter values are also silent no-ops
  bad_par <- !(is.na(par_v)) & !(par_v %in% c("fecundity", "survival", "capacity"))
  # Empty tokens from stray commas are benign formatting - ignore
  bad[ls_v == ""] <- FALSE

  if (any(bad) || any(bad_par)) {
    msgs <- unique(c(
      if (any(bad)) paste0("life_stage '", ls_v[bad], "' (parameter '",
                           par_v[bad], "', stressor '", CE_df$stressor[bad], "')"),
      if (any(bad_par)) paste0("parameter '", par_v[bad_par], "' (stressor '",
                               CE_df$stressor[bad_par], "')")
    ))
    warning(
      "CE_df contains unrecognized entries that will have NO effect on the ",
      "population model: ", paste(msgs, collapse = "; "),
      ". Check spelling against the Life_stages tags documented in the ",
      "population model vignette.",
      call. = FALSE
    )
  }

  # ============================================================================
  # Reject duplicate vital-rate targets within a stressor
  # ============================================================================
  # Several life_stage tokens alias the same underlying vital rate (e.g.
  # "s1", "stage_1" and "surv_1" all target stage-1 survival). If one
  # stressor lists the same target twice - through aliases or a repeated
  # token - its effect would silently apply twice (the score squared),
  # which is never intended. Hard error; to apply two separate effects to
  # one life stage, use two distinctly named stressors (effects multiply).
  canon <- canonical_ce_target(ls_v, par_v, anadromous)
  yr_key <- if ("Year" %in% colnames(CE_df)) CE_df$Year else ""
  grp_key <- paste(CE_df$stressor, par_v, CE_df$simulation, CE_df$HUC,
                   yr_key, sep = "\r")
  target_key <- paste(grp_key, canon, sep = "\r")
  dup_rows <- !is.na(canon) & duplicated(target_key)
  if (any(dup_rows)) {
    dup_keys <- unique(target_key[dup_rows])
    dsc <- vapply(utils::head(dup_keys, 5), function(k) {
      rows <- which(target_key == k)
      paste0("stressor '", CE_df$stressor[rows[1]], "' (",
             par_v[rows[1]], "): life stage tags ",
             paste0("'", unique(ls_v[rows]), "'", collapse = ", "))
    }, character(1))
    stop(
      "CE_df targets the same vital rate more than once within a single ",
      "stressor: ", paste(dsc, collapse = "; "),
      ". List each life stage only once per stressor. If the table has ",
      "one row per simulation year, include a 'Year' column. To apply ",
      "two separate effects to the same life stage, create two ",
      "distinctly named stressors (their effects multiply).",
      call. = FALSE
    )
  }

  CE_cap <- CE_df[CE_df$parameter == "capacity", ]
  CE_surv <- CE_df[CE_df$parameter == "survival", ]
  CE_fecund <- CE_df[CE_df$parameter == "fecundity", ]

  CE_cap_type <- CE_cap[, c("stressor", "life_stage", "parameter", "simulation", "HUC")]
  CE_surv_type <- CE_surv[, c("stressor", "life_stage", "parameter", "simulation", "HUC")]
  CE_fecund_type <- CE_fecund[, c("stressor", "life_stage", "parameter", "simulation", "HUC")]

  CE_cap_type    <- CE_cap_type[!(duplicated(CE_cap_type)), ]
  CE_surv_type   <- CE_surv_type[!(duplicated(CE_surv_type)), ]
  CE_fecund_type <- CE_fecund_type[!(duplicated(CE_fecund_type)), ]



  # ============================================================================
  # cumulative effects stressor is acting on fecundity
  # ============================================================================
  if (nrow(CE_fecund_type) > 0) {

    for (i in 1:nrow(CE_fecund_type)) {

      # Multiply effects additively for survival
      index_row <- CE_fecund_type[i, ]

      # Get all matching CE_fecund records from CE_fecund_type index columns
      trow <- CE_fecund[CE_fecund$stressor == index_row$stressor &
                          CE_fecund$simulation == index_row$simulation &
                          CE_fecund$parameter == index_row$parameter &
                          CE_fecund$life_stage == index_row$life_stage &
                          CE_fecund$HUC == index_row$HUC, ]

      if (any(is.na(trow$sys.cap))) {
        # Skip on missing data - warn so direct callers know the
        # stressor is being dropped (once per stressor x life stage)
        warning(
          "CE stressor '", index_row$stressor, "' (fecundity, life stage '",
          index_row$life_stage, "') has NA sys.cap value(s) - this ",
          "stressor x life stage is skipped for all years.",
          call. = FALSE
        )
        next
      }

      # Fecundity is knocked down by a stressor
      # (the "stage_b" spelling can never reach here - underscores are
      # stripped during normalization; "stageb" below covers it)
      if (index_row$life_stage == "eps" | index_row$life_stage == "spawners" | index_row$life_stage == "adults" | index_row$life_stage == "stageb") {

        sys_caps <- align_sys_caps(trow, length(rates$ft))

        # Update values in surv vector
        rates$ft <- mapply(function(fec_vec, sys_caps) {
          fec_vec <- sys_caps * fec_vec
          fec_vec
        }, rates$ft, sys_caps, SIMPLIFY = FALSE)
      }

      # For ss in 1:12, build "stage{ss}" and "s{ss}" and "surv{ss}"
      for (ss in 1:12) {
        if (index_row$life_stage == paste0("eps", ss) |
            index_row$life_stage == paste0("stageb", ss)) {

          sys_caps <- align_sys_caps(trow, length(rates$ft))

          # Update values in surv vector
          rates$ft <- mapply(function(fec_vec, sys_caps) {
            fec_vec[paste0("eps", ss)] <- sys_caps * fec_vec[paste0("eps", ss)]
            fec_vec
          }, rates$ft, sys_caps, SIMPLIFY = FALSE)
        }
      }





    }

  }



  # ============================================================================
  # Cumulative effects stressor is acting on survivorship vital rate
  # ============================================================================
  # apply stressors to survival for eggs, juveniles, adults, or all life stages

  if (nrow(CE_surv_type) > 0) {
    for (i in 1:nrow(CE_surv_type)) {

      # Multiply effects additively for survival
      index_row <- CE_surv_type[i, ]

      # Get all matching CE_surv records from CE_surv_type index columns
      trow <- CE_surv[CE_surv$stressor == index_row$stressor &
                        CE_surv$simulation == index_row$simulation &
                        CE_surv$parameter == index_row$parameter &
                        CE_surv$life_stage == index_row$life_stage &
                        CE_surv$HUC == index_row$HUC, ]

      if (any(is.na(trow$sys.cap))) {
        # Skip on missing data - warn so direct callers know the
        # stressor is being dropped (once per stressor x life stage)
        warning(
          "CE stressor '", index_row$stressor, "' (survival, life stage '",
          index_row$life_stage, "') has NA sys.cap value(s) - this ",
          "stressor x life stage is skipped for all years.",
          call. = FALSE
        )
        next
      }

      # Update Normal Stages for egg
      if (index_row$life_stage == "stagee" | index_row$life_stage == "se" | index_row$life_stage == "egg" | index_row$life_stage == "eggs" | index_row$life_stage == "stageegg" | index_row$life_stage == "stageeggs") {
        sys_caps <- align_sys_caps(trow, length(rates$st))

        # Update values in surv vector
        rates$st <- mapply(function(surv_vec, sys_caps) {
          surv_vec["sE"] <- sys_caps * surv_vec["sE"]
          surv_vec
        }, rates$st, sys_caps, SIMPLIFY = FALSE)
      }

      # Update Normal Stages for fry
      if (index_row$life_stage == "stage0" | index_row$life_stage == "s0" | index_row$life_stage == "so" | index_row$life_stage == "fry") {
        sys_caps <- align_sys_caps(trow, length(rates$st))

        # Update values in surv vector
        rates$st <- mapply(function(surv_vec, sys_caps) {
          surv_vec["s0"] <- sys_caps * surv_vec["s0"]
          surv_vec
        }, rates$st, sys_caps, SIMPLIFY = FALSE)
      }

      # If unspecified apply to all stages
      if (index_row$life_stage == "surv" | index_row$life_stage == "all") {
        sys_caps <- align_sys_caps(trow, length(rates$st))

        # Update values in surv vector
        rates$st <- mapply(function(surv_vec, sys_caps) {
          surv_vec <- sys_caps * surv_vec
          surv_vec
        }, rates$st, sys_caps, SIMPLIFY = FALSE)
      }

      # For ss in 1:12, build "stage{ss}" and "s{ss}" and "surv{ss}"
      # Also match "stagepb{ss}" for anadromous pre-breeder stages
      for (ss in 1:12) {
        if (index_row$life_stage == paste0("stage", ss) |
            index_row$life_stage == paste0("s", ss) |
            index_row$life_stage == paste0("surv", ss) |
            index_row$life_stage == paste0("stagepb", ss)) {

          sys_caps <- align_sys_caps(trow, length(rates$st))

          # Update values in surv vector
          rates$st <- mapply(function(surv_vec, sys_caps) {
            surv_vec[paste0("s", ss)] <- sys_caps * surv_vec[paste0("s", ss)]
            surv_vec
          }, rates$st, sys_caps, SIMPLIFY = FALSE)
        }
      }


      # If generally targeting adult survivorship and not anadromous
      if (index_row$life_stage == "adult" & !(anadromous)) {

        # Get the adult stage position
        mature_n <- as.numeric(gsub("[^0-9]", "", names(mat_stage)))
        adult_stages <- paste0("s", mature_n)

        # Prepare sys_caps
        sys_caps <- align_sys_caps(trow, length(rates$st))

        # Update rates$smigt: multiply the "smigX" column by sys_caps
        rates$st <- mapply(function(surv_vec, sys_caps) {
          surv_vec[adult_stages] <- surv_vec[adult_stages] * sys_caps
          surv_vec
        }, rates$st, sys_caps, SIMPLIFY = FALSE)

      }

      # However, if adult and anadromous, then it is implied that it must
      # be linked to pre-spawn survivorship... adult is too generic included
      # for backwards compatibility
      if (index_row$life_stage == "adult" & (anadromous)) {
        index_row$life_stage <- "u"
      }


      # Prespawn mortality u stages
      # Update Normal Stages for juveniles

      if (index_row$life_stage == "u" | index_row$life_stage == "prespawn"| index_row$life_stage == "spawners") {
        sys_caps <- align_sys_caps(trow, length(rates$ut))

        # Update values in surv vector
        rates$ut <- mapply(function(surv_vec, sys_caps) {
          surv_vec <- sys_caps * surv_vec
          surv_vec
        }, rates$ut, sys_caps, SIMPLIFY = FALSE)
      }

      # Specific spawner stage class

      # For ss in 1:12, build "u{ss}" and "prespawn{ss}"
      for (ss in 1:12) {

        u_stage   <- paste0("u", ss)
        prespawn_stage <- paste0("prespawn", ss)

        # If index_row$life_stage is one of these, do the update
        if (index_row$life_stage == u_stage | index_row$life_stage == prespawn_stage) {
          sys_caps <- align_sys_caps(trow, length(rates$ut))

          # Update values in surv vector
          rates$ut <- mapply(function(surv_vec, sys_caps) {
            surv_vec[u_stage] <- sys_caps * surv_vec[u_stage]
            surv_vec
          }, rates$ut, sys_caps, SIMPLIFY = FALSE)
        }
      }

      # Spawner migration smig stages

      if (index_row$life_stage == "smig" | index_row$life_stage == "spawnmig") {
        sys_caps <- align_sys_caps(trow, length(rates$smigt))

        # Update values in surv vector
        rates$smigt <- mapply(function(surv_vec, sys_caps) {
          surv_vec <- sys_caps * surv_vec
          surv_vec
        }, rates$smigt, sys_caps, SIMPLIFY = FALSE)
      }


      # Stage-specific migration rates

      # For ss in 1:12, build "smig{ss}" and "spawnmig{ss}"
      for (ss in 1:12) {
        smig_stage   <- paste0("smig", ss)
        spawn_stage <- paste0("spawnmig", ss)

        # If index_row$life_stage is one of these, do the update
        if (index_row$life_stage %in% c(smig_stage, spawn_stage)) {
          # Prepare sys_caps
          sys_caps <- align_sys_caps(trow, length(rates$smigt))

          # Update rates$smigt: multiply the "smigX" column by sys_caps
          rates$smigt <- mapply(function(surv_vec, sys_caps) {
            surv_vec[smig_stage] <- surv_vec[smig_stage] * sys_caps
            surv_vec
          }, rates$smigt, sys_caps, SIMPLIFY = FALSE)
        }
      }


    }
  }




  # ============================================================================
  # Cumulative effects stressor is acting on carrying capacity
  # ============================================================================
  # apply stressors to carrying capacity for eggs, juveniles, adults, or all life stages

  if (nrow(CE_cap_type) > 0) {
    for (i in 1:nrow(CE_cap_type)) {

      # Multiply effects additively for survival
      index_row <- CE_cap_type[i, ]

      # Get all matching CE_cap records from CE_cap_type index columns
      trow <- CE_cap[CE_cap$stressor == index_row$stressor &
                       CE_cap$simulation == index_row$simulation &
                       CE_cap$parameter == index_row$parameter &
                       CE_cap$life_stage == index_row$life_stage &
                       CE_cap$HUC == index_row$HUC, ]

      if (any(is.na(trow$sys.cap))) {
        # Skip on missing data - warn so direct callers know the
        # stressor is being dropped (once per stressor x life stage)
        warning(
          "CE stressor '", index_row$stressor, "' (capacity, life stage '",
          index_row$life_stage, "') has NA sys.cap value(s) - this ",
          "stressor x life stage is skipped for all years.",
          call. = FALSE
        )
        next
      }


      # Update capacity for eggs
      if (index_row$life_stage == "se" |
          index_row$life_stage == "stagee" |
          index_row$life_stage == "egg" |
          index_row$life_stage == "eggs" |
          index_row$life_stage == "stageegg" |
          index_row$life_stage == "stageeggs" | index_row$life_stage == "ke") {
        sys_caps <- align_sys_caps(trow, length(rates$Ke_list))

        # Update values in surv vector
        rates$Ke_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec <- sys_caps * cap_vec
          cap_vec
        }, rates$Ke_list, sys_caps, SIMPLIFY = FALSE)
      }

      # Update capacity for fry
      if (index_row$life_stage == "s0" |
          index_row$life_stage == "so" |
          index_row$life_stage == "stage0" |
          index_row$life_stage == "fry" | index_row$life_stage == "k0") {
        sys_caps <- align_sys_caps(trow, length(rates$K0_list))

        # Update values in surv vector
        rates$K0_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec <- sys_caps * cap_vec
          cap_vec
        }, rates$K0_list, sys_caps, SIMPLIFY = FALSE)
      }



      # NA-safe naming-scheme detection (isTRUE guards against unnamed
      # K vectors from direct callers)
      k_prefix <- NA_character_
      if (isTRUE(names(rates$K_list[[1]])[1] == "K1")) {
        k_prefix <- "K"
      }
      if (isTRUE(names(rates$K_list[[1]])[1] == "stage_Pb_1")) {
        k_prefix <- "stage_Pb_"
      }
      if (is.na(k_prefix)) {
        # Previously fell through to "object 'k_prefix' not found"
        stop(
          "Unrecognized capacity (K) column naming scheme in the life ",
          "histories object: expected stage capacity names starting 'K1' ",
          "(non-anadromous) or 'stage_Pb_1' (anadromous), got '",
          paste(utils::head(names(rates$K_list[[1]]), 3), collapse = "', '"),
          "'. Cannot apply capacity stressors.",
          call. = FALSE
        )
      }

      # For ss in 1:12, build "smig{ss}" and "spawnmig{ss}"
      for (ss in 1:12) {

        stage_stage   <- paste0("stage", ss)
        s_stage <- paste0("s", ss)
        k_stage <- paste0("k", ss)
        stage_pb_stage <- paste0("stagepb", ss)
        p_stage <- paste0("p", ss)

        # If index_row$life_stage is one of these, do the update
        if (index_row$life_stage %in% c(stage_stage, s_stage, k_stage, stage_pb_stage, p_stage)) {

          target_stage <- paste0(k_prefix, ss)

          # Guard: only modify an EXISTING capacity entry. Writing to a
          # missing name would append a stray element (growing the K
          # vector and crashing dd.N.bh); warn and skip instead.
          if (is.na(k_prefix) ||
              !(target_stage %in% names(rates$K_list[[1]]))) {
            warning(
              "Capacity stressor '", index_row$stressor,
              "' targets life stage '", index_row$life_stage,
              "' but no matching capacity entry ('", target_stage,
              "') exists in the K vector - target skipped.",
              call. = FALSE
            )
          } else {
            # Prepare sys_caps
            sys_caps <- align_sys_caps(trow, length(rates$K_list))

            # Update rates$K_list: to adjust stage capacity
            rates$K_list <- mapply(function(cap_vec, sys_caps) {
              cap_vec[target_stage] <- cap_vec[target_stage] * sys_caps
              cap_vec
            }, rates$K_list, sys_caps, SIMPLIFY = FALSE)
          }
        }
      }


      # Specific spawner stage capacities...
      # For ss in 1:12, build "{ss}"
      for (ss in 1:12) {

        check1 <- paste0("spawn", ss)
        check2 <- paste0("spawners", ss)
        check3 <- paste0("b", ss)
        check4 <- paste0("stageb", ss)
        check5 <- paste0("stageb", ss)
        check6 <- paste0("spawner", ss)

        # If index_row$life_stage is one of these, do the update
        if (index_row$life_stage %in% c(check1, check2, check3, check4, check5, check6)) {

          target_stage <- paste0("stage_B_", ss)

          # Guard: only modify an EXISTING spawner-class capacity entry.
          # Previously (with the K vector renamed to K1..KN through the
          # override path) this write appended a stray NA element, growing
          # the K vector and crashing dd.N.bh; for non-anadromous models
          # the spawner-age target simply has no matching class.
          if (!(target_stage %in% names(rates$K_list[[1]]))) {
            warning(
              "Capacity stressor '", index_row$stressor,
              "' targets spawner age class '", index_row$life_stage,
              "' but no matching capacity entry ('", target_stage,
              "') exists in the K vector - target skipped.",
              call. = FALSE
            )
          } else {
            # Prepare sys_caps
            sys_caps <- align_sys_caps(trow, length(rates$K_list))

            # Update rates$K_list: to adjust stage capacity
            rates$K_list <- mapply(function(cap_vec, sys_caps) {
              cap_vec[target_stage] <- cap_vec[target_stage] * sys_caps
              cap_vec
            }, rates$K_list, sys_caps, SIMPLIFY = FALSE)
          }
        }
      }



      # If generally targeting adult CAPACITY and not anadromous
      if (index_row$life_stage == "adult" & !(anadromous)) {

        # Get the adult stage position
        mature_n <- as.numeric(gsub("[^0-9]", "", names(mat_stage)))
        adult_stages <- paste0("K", mature_n)

        # Prepare sys_caps
        sys_caps <- align_sys_caps(trow, length(rates$K_list))

        # Update rates$smigt: multiply the "smigX" column by sys_caps
        rates$K_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec[adult_stages] <- cap_vec[adult_stages] * sys_caps
          cap_vec
        }, rates$K_list, sys_caps, SIMPLIFY = FALSE)

      }

      # However, if adult and anadromous, then it is implied that it must
      # be linked to spawner capacity... adult is too generic included
      # for backwards compatibility
      if (index_row$life_stage == "adult" & (anadromous)) {
        index_row$life_stage <- "spawners"
      }


      # Update capacity for ALL spawners
      if (index_row$life_stage == "spawner" |
          index_row$life_stage == "spawners" |
          index_row$life_stage == "stageb" |
          index_row$life_stage == "spawn") {

        sys_caps <- align_sys_caps(trow, length(rates$Kspawners_list))

        # Update values in Kspawners_list vector
        rates$Kspawners_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec <- sys_caps * cap_vec
          cap_vec
        }, rates$Kspawners_list, sys_caps, SIMPLIFY = FALSE)

        # Also update values in the K_list vector
        rates$K_list <- mapply(function(cap_vec, sys_caps) {
          cap_vec[grepl("^stage_B_", names(cap_vec))] <- sys_caps * cap_vec[grepl("^stage_B_", names(cap_vec))]
          cap_vec
        }, rates$K_list, sys_caps, SIMPLIFY = FALSE)

      }

      if (index_row$life_stage == "all") {
        message("index_row$life_stage == 'all'")
        stop("all stage not yet implemented")
      }

    }

  } # end of capacity section



  # Return the modified life history vital rate data
  return(rates)


}
