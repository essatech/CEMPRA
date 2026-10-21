#' Load Interaction Matrices
#'
#' @description Load interaction Excel Matrices
#'
#' @details Load 2-factor interaction matrices from
#' Excel workbook (if they exist). This function is run from
#' within StressorResponseWorkbook().
#'
#' Only the "Mean System Capacity (%)" block is required on each MInt_
#' sheet (and must be completely filled). The "SD", "low.limit" and
#' "up.limit" blocks are optional: an absent block - or blank cells
#' within one - defaults to SD = 0 (deterministic), low.limit = 0 and
#' up.limit = 100.
#'
#' @param mint_names Character vector of interaction matrix names.
#' @param filename Character string of Excel file.
#'
#' @returns a list object of interaction matrix objects.
#'
#' @export
#'
interaction_matrix_load <-
  function(mint_names = NA,
           filename = NA) {


    # Build single matrix loader function...
    load_matrix <- function(this_matrix = NA,
                            filename = NA) {

      # this_matrix <- mint_names

      # Trim a single metadata cell value (NA-safe)
      trim1 <- function(x) {
        x <- as.character(x)
        x <- trimws(x)
        if (length(x) == 0) return(NA_character_)
        x[1]
      }

      # Read from Excel - find start points
      mat <- suppressMessages(readxl::read_excel(filename, sheet = this_matrix))
      # Index reference column
      index_col1 <- mat[, 2]

      Matrix_Name <- trim1(colnames(index_col1))
      Columns <- trim1(index_col1[1, ][[1]])
      Rows <- trim1(index_col1[2, ][[1]])
      Main_Effect <- trim1(index_col1[3, ][[1]])

      # Index reference column
      index_col2 <- mat[, 4]

      Life_stages <- trim1(colnames(index_col2))
      Parameters <- trim1(index_col2[1, ][[1]])
      Model <- trim1(index_col2[2, ][[1]])

      # A blank Life_stages header comes back as readxl's repaired
      # "...N" placeholder - treat it as the default "adult" (with a
      # message) instead of silently disabling the matrix in
      # adult-only runs
      if (is.na(Life_stages) || Life_stages == "" ||
          grepl("^\\.\\.\\.[0-9]+$", Life_stages)) {
        message("Interaction matrix sheet '", this_matrix,
                "': Life_stages header is blank - assuming 'adult'.")
        Life_stages <- "adult"
      }

      if (is.na(Columns) || is.na(Rows)) {
        stop("Interaction matrix sheet '", this_matrix,
             "': the Columns and/or Rows stressor name cells are blank. ",
             "Both axis stressor names are required.")
      }

      # Get reference points for the four blocks. Only the mean block is
      # required: SD, low.limit and up.limit are optional and default to
      # all-0, all-0 and all-100 respectively when their block is absent.
      mheck <- unlist(index_col1)
      names(mheck) <- NULL

      find_label <- function(lbl, required = FALSE) {
        p <- which(mheck == lbl)
        if (length(p) > 1) {
          stop("Interaction matrix sheet '", this_matrix,
               "': expected at most one '", lbl, "' block label in column B ",
               "(found ", length(p), ").")
        }
        if (length(p) == 0) {
          if (required) {
            stop("Interaction matrix sheet '", this_matrix,
                 "': the required 'Mean System Capacity (%)' block label ",
                 "was not found in column B.")
          }
          return(NA_integer_)
        }
        p
      }

      tab1s <- find_label("Mean System Capacity (%)", required = TRUE)
      tab2s <- find_label("SD")
      tab3s <- find_label("low.limit")
      tab4s <- find_label("up.limit")

      mat1 <- suppressMessages(
        readxl::read_excel(filename, sheet = this_matrix, skip = tab1s + 1)
      )

      # Check width
      col_width <- ncol(mat1)

      read_block <- function(row_start, row_end) {
        suppressMessages(readxl::read_excel(
          filename,
          sheet = this_matrix,
          range = readxl::cell_limits(c(row_start, 2), c(row_end, 1 + col_width))
        ))
      }

      # Drop stray trailing columns: the width probe above is unbounded,
      # so a user note anywhere to the right widens every block with
      # blank-header, all-NA columns
      strip_stray_cols <- function(block) {
        if (ncol(block) <= 2) return(block)
        keep <- rep(TRUE, ncol(block))
        for (cc in seq(ncol(block), 3)) {
          nm <- colnames(block)[cc]
          stray_name <- is.na(nm) || nm == "" || grepl("^\\.\\.\\.[0-9]+$", nm)
          if (stray_name && all(is.na(block[[cc]]))) {
            keep[cc] <- FALSE
          } else {
            break
          }
        }
        block[, keep, drop = FALSE]
      }

      # Drop trailing all-blank rows (e.g., the spacer row before the
      # next block, or padding at the bottom of the sheet)
      strip_blank_rows <- function(block) {
        keep <- rep(TRUE, nrow(block))
        for (rr in seq(nrow(block), 1)) {
          if (all(is.na(unlist(block[rr, ])))) {
            keep[rr] <- FALSE
          } else {
            break
          }
        }
        block[keep, , drop = FALSE]
      }

      # Each block spans from its label row to the row before the next
      # present label (the last block runs to the end of the sheet)
      label_pos <- c(tab1s, tab2s, tab3s, tab4s)
      sheet_end <- nrow(mat) + 2 # mat excludes the header row
      block_end <- function(pos) {
        later <- label_pos[!is.na(label_pos) & label_pos > pos]
        if (length(later) == 0) sheet_end - 1 else min(later) - 1
      }
      get_block <- function(pos) {
        if (is.na(pos)) return(NULL)
        strip_blank_rows(strip_stray_cols(read_block(pos + 1, block_end(pos))))
      }

      # MSC matrix (required)
      msc <- get_block(tab1s)
      # SD / low.limit / up.limit matrices (optional)
      msd <- get_block(tab2s)
      mll <- get_block(tab3s)
      mul <- get_block(tab4s)

      # Synthesize any absent optional block from the mean block's shape
      default_block <- function(fill, label) {
        b <- msc
        colnames(b)[1] <- label
        for (cc in 2:ncol(b)) {
          b[[cc]] <- fill
        }
        b
      }
      if (is.null(msd)) msd <- default_block(0, "SD")
      if (is.null(mll)) mll <- default_block(0, "low.limit")
      if (is.null(mul)) mul <- default_block(100, "up.limit")

      # Blank/non-numeric cells inside the optional blocks take the same
      # defaults (SD 0, low.limit 0, up.limit 100)
      fill_na_cells <- function(block, fill) {
        for (cc in 2:ncol(block)) {
          v <- suppressWarnings(as.numeric(as.character(block[[cc]])))
          v[is.na(v)] <- fill
          block[[cc]] <- v
        }
        block
      }
      msd <- fill_na_cells(msd, 0)
      mll <- fill_na_cells(mll, 0)
      mul <- fill_na_cells(mul, 100)

      # All four blocks must have identical dimensions - mismatched blank
      # rows between blocks silently truncate/pad the later surfaces
      dims <- vapply(list(msc, msd, mll, mul),
                     function(b) paste(dim(b), collapse = "x"), "")
      if (length(unique(dims)) != 1) {
        stop("Interaction matrix sheet '", this_matrix,
             "': the four blocks have differing dimensions (mean ", dims[1],
             ", SD ", dims[2], ", low.limit ", dims[3], ", up.limit ",
             dims[4], "). Check for extra/missing blank rows between the ",
             "blocks and stray cells to the right of the matrices.")
      }

      # The mean matrix must be complete: a blank cell interpolates to NA
      # and silently nullifies the matrix over that dose region
      msc_vals <- suppressWarnings(
        as.numeric(unlist(msc[, 2:ncol(msc), drop = FALSE]))
      )
      if (any(is.na(msc_vals))) {
        stop("Interaction matrix sheet '", this_matrix,
             "': the Mean System Capacity block contains ",
             sum(is.na(msc_vals)),
             " blank or non-numeric cell(s). All cells must be filled.")
      }

      # Build export
      ret_obj <- list()

      # Add attributes
      ret_obj$Matrix_Name <- Matrix_Name
      ret_obj$Columns <- Columns
      ret_obj$Rows <- Rows
      ret_obj$Main_Effect <- Main_Effect
      ret_obj$Life_stages <- Life_stages
      ret_obj$Parameters <- Parameters
      ret_obj$Model <- Model

      ret_obj$mat_msc <- msc
      ret_obj$mat_sd <- msd
      ret_obj$mat_ll <- mll
      ret_obj$mat_ul <- mul

      return(ret_obj)

    }


    # load matrices
    mat_all <- lapply(mint_names, load_matrix, filename = filename)

    names(mat_all) <- mint_names

    length(mat_all)


    return(mat_all)



  }
