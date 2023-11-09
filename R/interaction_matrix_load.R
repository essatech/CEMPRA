#' Load Interaction Matrices
#'
#' @description Load interaction Excel Matrices
#'
#' @details Load 2-factor interaction matrices from
#' Excel workbook (if they exist). This function is run from
#' within StressorResponseWorkbook().
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

      # Read from Excel - find start points
      mat <- readxl::read_excel(filename, sheet = this_matrix)
      # Index reference column
      index_col1 <- mat[, 2]

      Matrix_Name <- colnames(index_col1)
      Columns <- index_col1[1,][[1]]
      Rows <- index_col1[2,][[1]]
      Main_Effect <- index_col1[3,][[1]]

      # Index reference column
      index_col2 <- mat[, 4]

      Life_stages <- colnames(index_col2)
      Parameters <- index_col2[1,][[1]]
      Model <- index_col2[2,][[1]]

      # Get reference points for other tables
      mheck <- unlist(index_col1)
      names(mheck) <- NULL
      tab1s <- which(mheck == "Mean System Capacity (%)")
      if (length(tab1s) != 1) {
        stop("Matrix import error 45...")
      }
      if (is.na(tab1s)) {
        stop("Matrix import error 78...")
      }

      tab2s <- which(mheck == "SD")
      if (length(tab2s) != 1) {
        stop("Matrix import error 48...")
      }
      if (is.na(tab2s)) {
        stop("Matrix import error 77...")
      }

      tab3s <- which(mheck == "low.limit")
      if (length(tab3s) != 1) {
        stop("Matrix import error 48...")
      }
      if (is.na(tab3s)) {
        stop("Matrix import error 77...")
      }

      tab4s <- which(mheck == "up.limit")
      if (length(tab4s) != 1) {
        stop("Matrix import error 48...")
      }
      if (is.na(tab4s)) {
        stop("Matrix import error 77...")
      }

      mat1 <-
        readxl::read_excel(filename, sheet = this_matrix, skip = tab1s + 1)

      # Check width
      col_width <- ncol(mat1)

      # MSC matrix
      msc <-
        readxl::read_excel(filename,
                           sheet = this_matrix,
                           range = readxl::cell_limits(c(tab1s + 1, 2), c(tab2s - 1, 1 + col_width)))

      # SD matrix
      msd <-
        readxl::read_excel(filename,
                           sheet = this_matrix,
                           range = readxl::cell_limits(c(tab2s + 1, 2), c(tab3s - 1, 1 + col_width)))

      # ll matrix
      mll <-
        readxl::read_excel(filename,
                           sheet = this_matrix,
                           range = readxl::cell_limits(c(tab3s + 1, 2), c(tab4s - 1, 1 + col_width)))

      # ul matrix
      mul <-
        readxl::read_excel(filename,
                           sheet = this_matrix,
                           range = readxl::cell_limits(c(tab4s + 1, 2), c(tab4s + (tab4s - tab3s) - 1, 1 + col_width)))


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
