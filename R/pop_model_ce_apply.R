#' Population Model CE Application to Vital Rates
#'
#' @description Utility function to apply cumulative effects to vital rates.
#'
#' @param dat Life history vital rates list object.
#' @param CE_df Cumulative effects data frame.
#' @param alevin_stage Alevin stage name.
#' @param all_juv All juvenile stage names.
#' @param fry_stages Fry stage names.
#' @param fry_parr_stages Fry and parr stage names.
#' @param parr_stages Parr stage names.
#' @param subadult_stages Subadult stage names.
#' @param adult_stages Adult stage names.
#'
#' @returns a modified dat vital rates object after applying CE effects.
#'
#' @export
#'
pop_model_ce_apply <- function(dat = NA, CE_df = NA, alevin_stage = NA, all_juv = NA, fry_stages = NA, fry_parr_stages = NA, parr_stages = NA, subadult_stages = NA, adult_stages = NA) {

  if (!is.null(CE_df)) {

        CE_cap <- CE_df[CE_df$parameter == "capacity", ]
        CE_surv <- CE_df[CE_df$parameter == "survival", ]
        CE_fecund <- CE_df[CE_df$parameter == "fecundity", ]

        # Cumulative effects stressor is acting on fecundity
        if (nrow(CE_fecund) > 0) {
            for (i in 1:nrow(CE_fecund)) {
                # Multiply effects additive for fecundity
                trow <- CE_fecund[i, ]
                if(is.na(trow$sys.cap)) {
                    # Skip is missing data
                    next
                }
                # eps = eggs per female
                dat$eps <- dat$eps * trow$sys.cap
            }
        }


        # Cumulative effects stressor is acting on survivorship vital rate
        # apply stressors to survival for eggs, juveniles, adults, or all life stages
        if (nrow(CE_surv) > 0) {
            for (i in 1:nrow(CE_surv)) {
                # Multiply effects additively for survival
                trow <- CE_surv[i, ]

                if(is.na(trow$sys.cap)) {
                    # Skip is missing data
                    next
                }

                # Check that life stage is valid
                valid_stage <- c("egg", "alevin", "all_juv", "fry", "parr", "fry_parr", "juv", "adult", "sub_adult", "all", "stage_E", "stage_0", "stage_1", "stage_2", "stage_3", "stage_4")

                if (!(trow$life_stage %in% valid_stage)) {
                    stop("Invalid life stage for cumulative effects stressor linked to survivorship. Valid survivorship life stages are: ", paste(valid_stage, collapse = ", "))
                }


                # Update Normal Stages
                if (trow$life_stage == "stage_E") {
                  dat$S["sE"] <- dat$S["sE"] * trow$sys.cap[trow$life_stage == "stage_E"]
                }
                if (trow$life_stage == "stage_0") {
                  dat$S["s0"] <- dat$S["s0"] * trow$sys.cap[trow$life_stage == "stage_0"]
                }
                if (trow$life_stage == "stage_1") {
                  dat$S["s1"] <- dat$S["s1"] * trow$sys.cap[trow$life_stage == "stage_1"]
                }
                if (trow$life_stage == "stage_2") {
                  dat$S["s2"] <- dat$S["s2"] * trow$sys.cap[trow$life_stage == "stage_2"]
                }
                if (trow$life_stage == "stage_3") {
                  dat$S["s3"] <- dat$S["s3"] * trow$sys.cap[trow$life_stage == "stage_3"]
                }
                if (trow$life_stage == "stage_4") {
                  dat$S["s4"] <- dat$S["s4"] * trow$sys.cap[trow$life_stage == "stage_4"]
                }

                # Apply to other nick-name stages...

                if (trow$life_stage == "egg") {
                    dat$S["sE"] <- dat$S["sE"] * trow$sys.cap[trow$life_stage == "egg"]
                }
                if (trow$life_stage == "alevin") {
                    dat$S[alevin_stage] <- dat$S[alevin_stage] * trow$sys.cap[trow$life_stage == "alevin"]
                }
                if (trow$life_stage == "all_juv") {
                    dat$S[all_juv] <- dat$S[all_juv] * trow$sys.cap[trow$life_stage == "all_juv"]
                }
                if (trow$life_stage == "fry") {
                    dat$S[fry_stages] <- dat$S[fry_stages] * trow$sys.cap[trow$life_stage == "fry"]
                }
                if (trow$life_stage == "fry_parr") {
                    dat$S[fry_parr_stages] <- dat$S[fry_parr_stages] * trow$sys.cap[trow$life_stage == "fry_parr"]
                }
                if (trow$life_stage == "parr") {
                    dat$S[parr_stages] <- dat$S[parr_stages] * trow$sys.cap[trow$life_stage == "parr"]
                }
                if (trow$life_stage == "sub_adult") {
                    dat$S[subadult_stages] <- dat$S[subadult_stages] * trow$sys.cap[trow$life_stage == "sub_adult"]
                }
                if (trow$life_stage == "adult") {
                    dat$S[adult_stages] <- dat$S[adult_stages] * trow$sys.cap[trow$life_stage == "adult"]
                }
                if (trow$life_stage == "all") {
                    dat$S <- dat$S * trow$sys.cap[trow$life_stage == "all"]
                }
            }
        }



        # Cumulative effects stressor is acting on carrying capacity
        # apply stressors to carrying capacity for eggs, juveniles, adults, or all life stages

        if (nrow(CE_cap) > 0) {
            for (i in 1:nrow(CE_cap)) {
                # Multiply effects additively for cc
                trow <- CE_cap[i, ]

                if(is.na(trow$sys.cap)) {
                    # Skip is missing data
                    next
                }

                # Check that life stage is valid
                valid_stage <- c("egg", "alevin", "all_juv", "fry", "parr", "fry_parr", "juv", "adult", "sub_adult", "all", "stage_E", "stage_0", "stage_1", "stage_2", "stage_3", "stage_4")

                if (!(trow$life_stage %in% valid_stage)) {
                    stop("Invalid life stage for cumulative effects stressor linked to survivorship. Valid survivorship life stages are: ", paste(valid_stage, collapse = ", "))
                }

                # Update Normal Stages
                if (trow$life_stage == "stage_E") {
                  dat$Ke <- dat$Ke * trow$sys.cap[trow$life_stage == "stage_E"]
                }
                if (trow$life_stage == "stage_0") {
                  dat$K0 <- dat$K0 * trow$sys.cap[trow$life_stage == "stage_0"]
                }
                if (trow$life_stage == "stage_1") {
                  dat$K["K1"] <- dat$K["K1"] * trow$sys.cap[trow$life_stage == "stage_1"]
                }
                if (trow$life_stage == "stage_2") {
                  dat$K["K2"] <- dat$K["K2"] * trow$sys.cap[trow$life_stage == "stage_2"]
                }
                if (trow$life_stage == "stage_3") {
                  dat$K["K3"] <- dat$K["K3"] * trow$sys.cap[trow$life_stage == "stage_3"]
                }
                if (trow$life_stage == "stage_4") {
                  dat$K["K4"] <- dat$K["K4"] * trow$sys.cap[trow$life_stage == "stage_4"]
                }

                # Apply to other nick-name stages...

                if (trow$life_stage == "egg") {
                    dat$Ke <- dat$Ke * trow$sys.cap[trow$life_stage == "egg"]
                }
                if (trow$life_stage == "alevin") {
                    dat$K0 <- dat$K0 * trow$sys.cap[trow$life_stage == "alevin"]
                }
                if (trow$life_stage == "all_juv") {
                    dat$K[all_juv - 2] <- dat$K[all_juv - 2] * trow$sys.cap[trow$life_stage == "all_juv"]
                }
                if (trow$life_stage == "fry") {
                    dat$K[fry_stages - 2] <- dat$K[fry_stages - 2] * trow$sys.cap[trow$life_stage == "fry"]
                }
                if (trow$life_stage == "fry_parr") {
                    dat$K[fry_parr_stages - 2] <- dat$K[fry_parr_stages - 2] * trow$sys.cap[trow$life_stage == "fry_parr"]
                }
                if (trow$life_stage == "parr") {
                    dat$K[parr_stages - 2] <- dat$K[parr_stages - 2] * trow$sys.cap[trow$life_stage == "parr"]
                }
                if (trow$life_stage == "juv") {
                    dat$K0 <- dat$K0 * trow$sys.cap[trow$life_stage == "juv"]
                }
                if (trow$life_stage == "sub_adult") {
                    dat$K[subadult_stages - 2] <- dat$K[subadult_stages - 2] * trow$sys.cap[trow$life_stage == "sub_adult"]
                }
                if (trow$life_stage == "adult") {
                    dat$K[adult_stages - 2] <- dat$K[adult_stages - 2] * trow$sys.cap[trow$life_stage == "adult"]
                }
                if (trow$life_stage == "all") {
                     dat$Ke <- dat$Ke * trow$sys.cap[trow$life_stage == "all"]
                     dat$K0 <- dat$K0 * trow$sys.cap[trow$life_stage == "all"]
                     dat$K <- dat$K * trow$sys.cap[trow$life_stage == "all"]
                }
            }

        }


        # Return the modified life history vital rate data
        return(dat)


    }
}
