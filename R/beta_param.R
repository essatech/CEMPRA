#' Estimate Beta Parameter
#'
#' @description Estimates beta parameter given mean (rates) and SD for the
#' the "untruncated" beta distribution.
#'
#' @details Updated using analytical solution from Kyle Wilson.
#'
#' @returns A list with alpha and beta parameters along with messages about mean and cv
#'
#' @param mean Numeric. The mean.
#' @param sd Numeric. The standard deviation.
#'
#' @examples
#' \dontrun{
#' library(CEMPRA)
#' CEMPRA::beta_param(0.8, 0.2)$beta
#' }
#'
#' @export
#'
beta_param <- function(mean, sd) {

  #can't have a mean for the beta at 0 or 1, if at boundary
  #shift away by 0.001
  message.mean <- ifelse(
    mean == 0 | mean == 1,
    "Warning - Mean for beta at 0 or 1, shifted from boundary by 0.001",
    "Mean for beta okay"
  ) # mean warning
  mean <-
    ifelse(mean == 0, 0.001, ifelse(mean == 1, 0.999, mean)) #crappy nested ifelse statement
  #the sd can't be too large either, use CV to check and use Kyle's truncation of CV
  #if needed
  cv <- sd / mean
  message.sd <-
    ifelse(((mean * cv) ^ 2) > (mean * (1 - mean)),
           "; Warning - truncating CV for beta at sqrt(mean*(1-mean))/mean",
           "; CV for beta okay"
    ) # sd warning
  cv <-
    ifelse(((mean * cv) ^ 2) > (mean * (1 - mean)),
           0.8 * sqrt(mean * (1 - mean)) / mean, cv) # apply a truncation, if needed
  sd <- mean * cv
  alpha <-
    -((mean * (mean ^ 2 + sd ^ 2 - mean)) / sd ^ 2)
  beta <- alpha / mean - alpha

  return(list(
    alpha = alpha,
    beta = beta,
    message.mean = message.mean,
    message.sd = message.sd
  ))

}
