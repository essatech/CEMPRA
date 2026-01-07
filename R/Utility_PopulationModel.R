#' proc utility function
#' @keywords internal
get_transition_positions <- function(matrix_size) {
  # Create an empty vector to store the positions
  positions <- c()
  # Loop through the columns of the matrix
  for (i in 1:(matrix_size - 1)) {
    # Calculate the position for the stage-to-stage+1 transition
    position <- (i - 1) * matrix_size + (i + 1)
    # Append the position to the vector
    positions <- c(positions, position)
  }
  return(positions)
}

#' proc utility function
#' @keywords internal
proc <- function(e, env = parent.frame()) {
  for (nm in all.vars(e)) {
    if (exists(nm, env) && is.language(g <- get(nm, env))) {
      if (is.expression(g)) g <- g[[1]]
      g <- Recall(g, env)
      L <- list(g)
      names(L) <- nm
      e <- esub(e, L)
    }
  }
  e
}



#' Project Matrix Forward in Time
#' @keywords internal
pmx_eval <- function(mx, vars, byrow = TRUE) {
  matrix(sapply(mx, eval, vars),
    sqrt(length(mx)),
    sqrt(length(mx)),
    byrow = byrow
  )
}



#' rbeta equivelent from TruncatedDistributions package
#' @keywords internal
rbeta2 <- function(n, shape1, shape2, ncp = 0) {
  C_rbeta <- NULL
  if (missing(ncp)) {
    .Call(C_rbeta, n, shape1, shape2)
  } else {
    X <- stats::rchisq(n, 2 * shape1, ncp = ncp)
    X / (X + stats::rchisq(n, 2 * shape2))
  }
}
