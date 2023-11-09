#' esub
#'
#' @keywords internal
esub <-
  function(expr, sublist) {
    do.call("substitute", list(expr, sublist))
  }

#' proc
#'
#' @keywords internal
proc <- function(e, env = parent.frame()) {
  for (nm in all.vars(e)) {
    if (exists(nm, env) && is.language(g <- get(nm, env))) {
      if (is.expression(g)) {
        g <- g[[1]]
      }
      g <- Recall(g, env)
      L <- list(g)
      names(L) <- nm
      e <- esub(e, L)
    }
  }
  e
}





#' dd_stage
#'
#' @keywords internal
dd_stage <- function() {
  # Returns an expression TODO: change all dd_stage to dd_stage()
  return(expression(cr / (1 + (cr - 1) * (x / k))))
}


#' stage_probs
#'
#' @keywords internal
stage_probs <- function(x, y) {
  # probability of staying within a life stage is the
  # product of surviving the life stage for each year 'y',
  # divided by the amount of those who stay within the
  # life stage (i.e., the + group)
  x * (1 - x^(y - 1)) / (1 - x^y)
}


#' dd_calc
#'
#' @keywords internal
dd_calc <- function(dat, varNames, Nmat) {
  Nstage <- NULL
  temp <- list()
  for (i in 1:Nstage)
  {
    temp[[i]] <-
      eval(as.expression(sapply(dd_stage(), proc)), c(dat, x = Nmat[, i]))
  }
  surv <- unlist(temp)
  names(surv) <- varNames
  return(surv)
}


#' E_est
#'
#' @details  Egg count - number of egg produced by pop annually
#'
#' @keywords internal
E_est <- function(N, dat) {
  E <- with(
    dat,
    sum(N * mat * events * eps * sR / int)
  )
  names(E) <- NULL
  E
}


#' s0_optim.f
#'
#' @details  Optimization fucntion - find YOY survival that gives a lambda
#'
#' @keywords internal
s0_optim.f <- function(s0, mx, dat, target.lambda) {
  dat$S["s0"] <- s0

  pmx <- pmx_eval(mx, c(dat, dat$S, dat$nYrs, dat$mat))

  lambda <- popbio::lambda(pmx)
  (lambda - target.lambda)^2
}


#' K_adj_old
#'
#' @keywords internal
K_adj_old <- function(replicates = 100,
                  dat, # life histories
                  ncores,
                  mx,
                  dx,
                  Nyears = 250,
                  enviro) {

  env <- environment()
  new_ls <- ls(envir = env)
  no_cores <- ncores # number of cores
  cl <- parallel::makeCluster(no_cores) # create clusters
  parallel::clusterExport(cl, enviro) # send data to clusters
  parallel::clusterExport(cl, new_ls, envir = env)
  #parallel::clusterEvalQ(cl, library(popbio)) # load library in clusters
  pop_data <- parallel::parLapply(cl, 1:replicates, function(r) {

    res <- Projection_DD(
      M.mx = mx,
      # projection matrix expression
      D.mx = dx,
      # dencity-depence matrix
      dat = dat,
      # life history data
      K = dat$Ka,
      # initial pop size as stage-structure vector
      Nyears = 51 + Nyears,
      # years to run simulation
      p.cat = 0 # Probability of castastrophe
    )

    res$pop[-(1:51), ]

  })

  parallel::stopCluster(cl)
  K <- dat$Ka
  # mean pop size (adult density) in each replicate
  meanK <- sapply(pop_data, function(x) {
    mean((x$N))
  })
  mean(meanK)
  round(K / mean(meanK), 3) # K.adj

  # Variance (inter-annual) in adult density in each replicate
  varK <- sapply(pop_data, function(x) {
    stats::var((x$N))
  })
  mean(varK)
  sqrt(mean(varK)) / mean(meanK) # CV - target 15% (Paul et al. 2003)
  return(list(
    "K.adj" = round(K / mean(meanK), 3),
    "CV" = sqrt(mean(varK)) / mean(meanK)
  ))
}



#' init_pop
#'
#' @param mx Population Matrix
#' @param Nt Desire adult density
#' @param p.rep maturity vector - 1: mature; 0: Juvenle; length Tmax + 1
#'
#' @keywords internal
init_pop <- function(mx, Nt, p.rep) {
  SS <-
    popbio::stable.stage(mx) # Stable stage distn
  Na <-
    SS * p.rep / sum(SS * p.rep) * Nt # Calc adult pop
  Nj <-
    SS * (1 - p.rep) / sum(SS * p.rep) * Nt # Calc juv pop
  N <- Nj + Na

  names(N) <- paste0("N", 1:length(N))

  N
}


#' d.vec.f
#' @description create vector of density dependence effects using compensation ratios
#'
#' @keywords internal
d.vec.f <- function(df, Ks, N) {

  sN <- rep(NA, length(df$S))

  for (i in 1:length(sN))
  {
    sN[i] <-
      eval(
        dd_stage(),
        list(
          cr = as.numeric(df$cr[i]),
          k = as.numeric(Ks[i]),
          x = as.numeric(N[i])
        )
      )
  }

  names(sN) <- names(df$S)
  return(sN)
}



#' Nb_est
#'
#' @keywords internal
Nb_est <- function(N, mat) {
  Nt <- sum(N * mat)
  return(Nt)
}


#' f_rand
#'
#' @keywords internal
f_rand <- function(mn, sigma, rho) {
  # Correlation matrix
  corr <-
    cor.CompSym(length(mn), rho) # compound symmetry correlation

  # number of variables
  vr_num <- length(mn)

  # Generate correlated random standard values
  X <-
    MASS::mvrnorm(1, mu = rep(0, vr_num), Sigma = corr)

  # Convert to probabilities
  pX <- stats::pnorm(X, mean = 0, sd = 1)

  # Use probability vectors to produce random
  # variables with log-normal distribution
  # mean and sd adjust for log-normal dist.such
  # that mean and sd of output match input values
  qX <-
    stats::qlnorm(pX, log(mn / sqrt(1 + sigma^2 / mn^2)), sqrt(log(1 + sigma^
      2 / mn^2)))
  # log normal distribution

  names(qX) <- names(mn)

  qX
}


#' s_rand
#'
#' @description Function to generate random survival rates
# variability added to Instantaneous mortality based on a constant CV
# Variance increased with M.
#'
#' @keywords internal
s_rand <- function(mn, cv, rho) {

  # Correlation matrix
  corr <-
    cor.AR1(length(mn), rho) # AR1 structure
  corr[1, 2:ncol(corr)] <-
    corr[2:nrow(corr), 1] <-
    0 # Make egg survival independnet

  # number of variables
  vr_num <- length(mn)

  # Generate correlated random standard values
  X <-
    MASS::mvrnorm(1, mu = rep(0, vr_num), Sigma = corr)

  # Convert to probabilities
  pX <- stats::pnorm(X, mean = 0, sd = 1)

  # Use probability vectors to prodice random variabiles with stretched beta distribution
  # calc params for streched-beta distribution
  params <-
    beta_stretch_val(-log(mn) + (-log(mn) * cv)^2 / 2, sigma = -log(mn) * cv)

  # convert probabilities into values to qbeta
  # based on stretched peta distribution
  qX <- stats::qbeta(pX, shape1 = params$a, shape2 = params$b) * (params$max - params$min) + params$min

  names(qX) <- names(mn)

  exp(-qX)

  rev_vals <- exp(-qX)


  # START MJB: Fix issue if surv 1.0 returns NA then 0
  # allow 100% survival with no variability
  check_nas <- function(x, y) {
    if(is.na(x)) {
      # check if sample is NA
      if(y > 0 & y <= 1.0) {
        return(y)
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  }
  rev_vals_fix <- mapply(check_nas, x = rev_vals, y = mn)
  # END MJB FIX...

  return(rev_vals_fix)


}


#' cor.CompSym
#'
#' @keywords internal
cor.CompSym <- function(n, rho, names = NA) {
  # create of matrix of rho
  C <- matrix(rho, nrow = n, ncol = n)
  # set diagonal to 1
  diag(C) <- 1
  if (is.character(names)) {
    colnames(C) <- names
  }
  C
}


#' cor.AR1
#'
#' @keywords internal
cor.AR1 <- function(n, rho, names = NA) {
  # create matric of 1s on diagonal
  C <- diag(n)
  # set off diagonal values to rho^(t - s)
  C <- rho^abs(row(C) - col(C))

  if (is.character(names)) {
    colnames(C) <- names
  }

  C
}


#' beta_stretch_val
#'
#' @description Calculate parameters for a stretched beta distribution.
#'
#' @keywords internal
beta_stretch_val <- function(mn,
                             variance = NA,
                             sigma = NA) {
  if (is.na(variance[1])) {
    variance <- sigma^2
  }
  # calc variance if not given
  if (is.na(sigma[1])) {
    sigma <- sqrt(variance)
  }
  # calc sd if not given

  # estimate mins and max of distribution
  min <- stats::qnorm(1e-6, mn, sd = sigma)
  max <- stats::qnorm(1 - 1e-6, mn, sd = sigma)

  # Scale means and variance to between 0 and 1
  mean.b <- (mn - min) / (max - min)
  var.b <- variance * (1 / (max - min))^2

  # estimate beta dist'n shape parameters
  a <-
    mean.b * ((mean.b * (1 - mean.b)) / var.b - 1)
  b <-
    (1 - mean.b) * ((mean.b * (1 - mean.b)) / var.b - 1)

  # output
  list(
    a = a,
    b = b,
    min = min,
    max = max
  )
}


#' harm_k
#'
#' @keywords internal
harm_k <- function(dat, stressors, Nstage, Korig) {
  names(K_impacted) <- names(Korig)
  return(K_impacted)
}
