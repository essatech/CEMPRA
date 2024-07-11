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
pop_model_ce_apply <- function(dat = NULL,
                               CE_df = NULL) {

  # if CE df is null then skip and exit function...
  if (is.null(CE_df)) {
    return(dat)
  }
  if (all(is.na(CE_df))) {
    return(dat)
  }
  if (nrow(CE_df) == 0) {
    return(dat)
  }

  # For testing import temp CE_df
  # filename <- system.file("extdata/simple_test/CE_df_testing.csv", package = "CEMPRA")
  # CE_df <- read.csv(filename, stringsAsFactors = FALSE)


  #-----------------------------------------------
  # Create vector of life stages for application
  #-----------------------------------------------

  # Define nicknames for stages --- legacy code --- do not expand on these
  alevin_stage <- 2
  all_juv <- 3:5
  fry_stages <- 3
  fry_parr_stages <- 3:4
  parr_stages <- 4
  juv_stages <- 1:3 #1:(3 + max(which(dat$mat == 0)))
  adult_stages <- (3 + max(which(dat$mat > 0)))
  subadult_stages <- adult_stages - 1


  # Need to split up life stage linkages
  # many will be comma separated
  CE_df$life_stage <- tolower(CE_df$life_stage)

  # Remove spaces
  CE_df$life_stage <- gsub(" ", "", CE_df$life_stage)

  # and remove underscores throughout
  CE_df$life_stage <- gsub("_", "", CE_df$life_stage)
  CE_df$life_stage <- tolower(CE_df$life_stage)


  # Use a reference ID for CE stressor to life stage linkages
  CE_df$ref_id <- 1:nrow(CE_df)

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


  # otherwise modify dat with CE stressors
  CE_df$parameter <- tolower(CE_df$parameter)
  CE_cap <- CE_df[CE_df$parameter == "capacity", ]
  CE_surv <- CE_df[CE_df$parameter == "survival", ]
  CE_fecund <- CE_df[CE_df$parameter == "fecundity", ]

  # cumulative effects stressor is acting on fecundity
  if (nrow(CE_fecund) > 0) {
    for (i in 1:nrow(CE_fecund)) {
      # Multiply effects additive for fecundity
      trow <- CE_fecund[i, ]
      if (is.na(trow$sys.cap)) {
        # Skip is missing data
        next
      }
      # eps = eggs per female
      if(trow$life_stage == "eps") {
        dat$eps <- dat$eps * trow$sys.cap
      }
      if(grepl("^eps", trow$life_stage) & nchar(trow$life_stage) > 3) {
        dat$eps[trow$life_stage] <- dat$eps[trow$life_stage] * trow$sys.cap
      }
    }
    # Drop any NAs that might have been created from non-existent classes
    dat$eps <- dat$eps[!(is.na(dat$eps))]
  }



  # Cumulative effects stressor is acting on survivorship vital rate
  # apply stressors to survival for eggs, juveniles, adults, or all life stages
  if (nrow(CE_surv) > 0) {
    for (i in 1:nrow(CE_surv)) {
      # Multiply effects additively for survival
      trow <- CE_surv[i, ]

      if (is.na(trow$sys.cap)) {
        # Skip is missing data
        next
      }

      # Update Normal Stages for egg
      if (trow$life_stage == "stagee" | trow$life_stage == "se") {
        dat$S["sE"] <- dat$S["sE"] * trow$sys.cap
      }
      if (trow$life_stage == "stage0" | trow$life_stage == "s0" | trow$life_stage == "so") {
        dat$S["s0"] <- dat$S["s0"] * trow$sys.cap
      }


      # If unspecifed apply to all stages
      if (trow$life_stage == "surv") {
        dat$S <- dat$S * trow$sys.cap
      }

      # Update Normal Stages for juveniles
      if (trow$life_stage == "stage1" | trow$life_stage == "s1" | trow$life_stage == "surv1") {
        dat$S["s1"] <- dat$S["s1"] * trow$sys.cap
      }
      if (trow$life_stage == "stage2" | trow$life_stage == "s2" | trow$life_stage == "surv2") {
        dat$S["s2"] <- dat$S["s2"] * trow$sys.cap
      }
      if (trow$life_stage == "stage3" | trow$life_stage == "s3" | trow$life_stage == "surv3") {
        dat$S["s3"] <- dat$S["s3"] * trow$sys.cap
      }
      if (trow$life_stage == "stage4" | trow$life_stage == "s4" | trow$life_stage == "surv4") {
        dat$S["s4"] <- dat$S["s4"] * trow$sys.cap
      }
      if (trow$life_stage == "stage5" | trow$life_stage == "s5" | trow$life_stage == "surv5") {
        dat$S["s5"] <- dat$S["s5"] * trow$sys.cap
      }
      if (trow$life_stage == "stage6" | trow$life_stage == "s6" | trow$life_stage == "surv6") {
        dat$S["s6"] <- dat$S["s6"] * trow$sys.cap
      }
      if (trow$life_stage == "stage7" | trow$life_stage == "s7" | trow$life_stage == "surv7") {
        dat$S["s7"] <- dat$S["s7"] * trow$sys.cap
      }
      if (trow$life_stage == "stage8" | trow$life_stage == "s8" | trow$life_stage == "surv8") {
        dat$S["s8"] <- dat$S["s8"] * trow$sys.cap
      }
      if (trow$life_stage == "stage9" | trow$life_stage == "s9" | trow$life_stage == "surv9") {
        dat$S["s9"] <- dat$S["s9"] * trow$sys.cap
      }
      if (trow$life_stage == "stage10" | trow$life_stage == "s10" | trow$life_stage == "surv10") {
        dat$S["s10"] <- dat$S["s10"] * trow$sys.cap
      }
      if (trow$life_stage == "stage11" | trow$life_stage == "s11" | trow$life_stage == "surv11") {
        dat$S["s11"] <- dat$S["s11"] * trow$sys.cap
      }
      if (trow$life_stage == "stage12" | trow$life_stage == "s12" | trow$life_stage == "surv12") {
        dat$S["s12"] <- dat$S["s12"] * trow$sys.cap
      }

      # Prespawn mortality u stages
      # Update Normal Stages for juveniles

      # If unspecifed apply to all stages
      if (trow$life_stage == "u" | trow$life_stage == "prespawn") {
        dat$u <- dat$u * trow$sys.cap
      }


      if (trow$life_stage == "prespawn1" | trow$life_stage == "u1") {
        dat$u["u1"] <- dat$u["u1"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn2" | trow$life_stage == "u2") {
        dat$u["u2"] <- dat$u["u2"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn3" | trow$life_stage == "u3") {
        dat$u["u3"] <- dat$u["u3"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn4" | trow$life_stage == "u4") {
        dat$u["u4"] <- dat$u["u4"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn5" | trow$life_stage == "u5") {
        dat$u["u5"] <- dat$u["u5"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn6" | trow$life_stage == "u6") {
        dat$u["u6"] <- dat$u["u6"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn7" | trow$life_stage == "u7") {
        dat$u["u7"] <- dat$u["u7"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn8" | trow$life_stage == "u8") {
        dat$u["u8"] <- dat$u["u8"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn9" | trow$life_stage == "u9") {
        dat$u["u9"] <- dat$u["u9"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn10" | trow$life_stage == "u10") {
        dat$u["u10"] <- dat$u["u10"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn11" | trow$life_stage == "u11") {
        dat$u["u11"] <- dat$u["u11"] * trow$sys.cap
      }
      if (trow$life_stage == "prespawn12" | trow$life_stage == "u12") {
        dat$u["u12"] <- dat$u["u12"] * trow$sys.cap
      }

      # Spawner migration smig stages

      # To all substages
      if (trow$life_stage == "smig" | trow$life_stage == "spawnmig") {
        dat$smig <- dat$smig * trow$sys.cap
      }

      if (trow$life_stage == "smig1" | trow$life_stage == "spawnmig1") {
        dat$smig["smig1"] <- dat$smig["smig1"] * trow$sys.cap
      }
      if (trow$life_stage == "smig2" | trow$life_stage == "spawnmig2") {
        dat$smig["smig2"] <- dat$smig["smig2"] * trow$sys.cap
      }
      if (trow$life_stage == "smig3" | trow$life_stage == "spawnmig3") {
        dat$smig["smig3"] <- dat$smig["smig3"] * trow$sys.cap
      }
      if (trow$life_stage == "smig4" | trow$life_stage == "spawnmig4") {
        dat$smig["smig4"] <- dat$smig["smig4"] * trow$sys.cap
      }
      if (trow$life_stage == "smig5" | trow$life_stage == "spawnmig5") {
        dat$smig["smig5"] <- dat$smig["smig5"] * trow$sys.cap
      }
      if (trow$life_stage == "smig6" | trow$life_stage == "spawnmig6") {
        dat$smig["smig6"] <- dat$smig["smig6"] * trow$sys.cap
      }
      if (trow$life_stage == "smig7" | trow$life_stage == "spawnmig7") {
        dat$smig["smig7"] <- dat$smig["smig7"] * trow$sys.cap
      }
      if (trow$life_stage == "smig8" | trow$life_stage == "spawnmig8") {
        dat$smig["smig8"] <- dat$smig["smig8"] * trow$sys.cap
      }
      if (trow$life_stage == "smig9" | trow$life_stage == "spawnmig9") {
        dat$smig["smig9"] <- dat$smig["smig9"] * trow$sys.cap
      }
      if (trow$life_stage == "smig10" | trow$life_stage == "spawnmig10") {
        dat$smig["smig10"] <- dat$smig["smig10"] * trow$sys.cap
      }
      if (trow$life_stage == "smig11" | trow$life_stage == "spawnmig11") {
        dat$smig["smig11"] <- dat$smig["smig11"] * trow$sys.cap
      }
      if (trow$life_stage == "smig12" | trow$life_stage == "spawnmig12") {
        dat$smig["smig12"] <- dat$smig["smig12"] * trow$sys.cap
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

  if("u" %in% names(dat)) {
    dat$u <- dat$u[!(is.na(dat$u))]
  }
  if("S" %in% names(dat)) {
    dat$S <- dat$S[!(is.na(dat$S))]
  }
  if("smig" %in% names(dat)) {
    dat$smig <- dat$smig[!(is.na(dat$smig))]
  }

  # Cumulative effects stressor is acting on carrying capacity
  # apply stressors to carrying capacity for eggs, juveniles, adults, or all life stages

  if (nrow(CE_cap) > 0) {
    for (i in 1:nrow(CE_cap)) {

      # Multiply effects additively for cc
      trow <- CE_cap[i, ]

      if (is.na(trow$sys.cap)) {
        # Skip is missing data
        next
      }

      # Update Normal Stages
      if (trow$life_stage == "se" | trow$life_stage == "stagee" | trow$life_stage == "ke") {
        dat$Ke <- dat$Ke * trow$sys.cap
      }
      if (trow$life_stage == "stage0" | trow$life_stage == "s0" | trow$life_stage == "so" | trow$life_stage == "k0") {
        dat$K0 <- dat$K0 * trow$sys.cap
      }

      if(names(dat$K)[1] == "K1") {
        k_prefix <- "K"
      }
      if(names(dat$K)[1] == "stage_Pb_1") {
        k_prefix <- "stage_Pb_"
      }


      if (trow$life_stage == "stage1" | trow$life_stage == "s1" | trow$life_stage == "k1" | trow$life_stage == "stage_pb_1" | trow$life_stage == "p1") {
        dat$K[paste0(k_prefix, 1)] <- dat$K[paste0(k_prefix, 1)] * trow$sys.cap
      }
      if (trow$life_stage == "stage2" | trow$life_stage == "s2" | trow$life_stage == "k2" | trow$life_stage == "stage_pb_2" | trow$life_stage == "p2") {
        dat$K[paste0(k_prefix, 2)] <- dat$K[paste0(k_prefix, 2)] * trow$sys.cap
      }
      if (trow$life_stage == "stage3" | trow$life_stage == "s3" | trow$life_stage == "k3" | trow$life_stage == "stage_pb_3" | trow$life_stage == "p3") {
        dat$K[paste0(k_prefix, 3)] <- dat$K[paste0(k_prefix, 3)] * trow$sys.cap
      }
      if (trow$life_stage == "stage4" | trow$life_stage == "s4" | trow$life_stage == "k4" | trow$life_stage == "stage_pb_4" | trow$life_stage == "p4") {
        dat$K[paste0(k_prefix, 4)] <- dat$K[paste0(k_prefix, 4)] * trow$sys.cap
      }
      if (trow$life_stage == "stage5" | trow$life_stage == "s5" | trow$life_stage == "k5" | trow$life_stage == "stage_pb_5" | trow$life_stage == "p5") {
        dat$K[paste0(k_prefix, 5)] <- dat$K[paste0(k_prefix, 5)] * trow$sys.cap
      }
      if (trow$life_stage == "stage6" | trow$life_stage == "s6" | trow$life_stage == "k6" | trow$life_stage == "stage_pb_6" | trow$life_stage == "p6") {
        dat$K[paste0(k_prefix, 6)] <- dat$K[paste0(k_prefix, 6)] * trow$sys.cap
      }
      if (trow$life_stage == "stage7" | trow$life_stage == "s7" | trow$life_stage == "k7" | trow$life_stage == "stage_pb_7" | trow$life_stage == "p7") {
        dat$K[paste0(k_prefix, 7)] <- dat$K[paste0(k_prefix, 7)] * trow$sys.cap
      }
      if (trow$life_stage == "stage8" | trow$life_stage == "s8" | trow$life_stage == "k8" | trow$life_stage == "stage_pb_8" | trow$life_stage == "p8") {
        dat$K[paste0(k_prefix, 8)] <- dat$K[paste0(k_prefix, 8)] * trow$sys.cap
      }
      if (trow$life_stage == "stage9" | trow$life_stage == "s9" | trow$life_stage == "k9" | trow$life_stage == "stage_pb_9" | trow$life_stage == "p9") {
        dat$K[paste0(k_prefix, 9)] <- dat$K[paste0(k_prefix, 9)] * trow$sys.cap
      }
      if (trow$life_stage == "stage10" | trow$life_stage == "s10" | trow$life_stage == "k10" | trow$life_stage == "stage_pb_10" | trow$life_stage == "p10") {
        dat$K[paste0(k_prefix, 10)] <- dat$K[paste0(k_prefix, 10)] * trow$sys.cap
      }
      if (trow$life_stage == "stage11" | trow$life_stage == "s11" | trow$life_stage == "k11" | trow$life_stage == "stage_pb_11" | trow$life_stage == "p11") {
        dat$K[paste0(k_prefix, 11)] <- dat$K[paste0(k_prefix, 11)] * trow$sys.cap
      }
      if (trow$life_stage == "stage12" | trow$life_stage == "s12" | trow$life_stage == "k12" | trow$life_stage == "stage_pb_12" | trow$life_stage == "p12") {
        dat$K[paste0(k_prefix, 12)] <- dat$K[paste0(k_prefix, 12)] * trow$sys.cap
      }


      # Reduce K for all spawner classes combined
      if (trow$life_stage == "spawners") {
        if(!(is.null(dat$stage_k_spawners))) {
          dat$stage_k_spawners <- dat$stage_k_spawners * trow$sys.cap
        }
      }

      # Then run for specific spawner classes
      if (trow$life_stage == "spawn1" | trow$life_stage == "spawners1" | trow$life_stage == "b1" | trow$life_stage == "stage_b_1") {
        dat$K["stage_B_1"] <- dat$K["stage_B_1"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn2" | trow$life_stage == "spawners2" | trow$life_stage == "b2" | trow$life_stage == "stage_b_2") {
        dat$K["stage_B_2"] <- dat$K["stage_B_2"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn3" | trow$life_stage == "spawners3" | trow$life_stage == "b3" | trow$life_stage == "stage_b_3") {
        dat$K["stage_B_3"] <- dat$K["stage_B_3"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn4" | trow$life_stage == "spawners4" | trow$life_stage == "b4" | trow$life_stage == "stage_b_4") {
        dat$K["stage_B_4"] <- dat$K["stage_B_4"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn5" | trow$life_stage == "spawners5" | trow$life_stage == "b5" | trow$life_stage == "stage_b_5") {
        dat$K["stage_B_5"] <- dat$K["stage_B_5"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn6" | trow$life_stage == "spawners6" | trow$life_stage == "b6" | trow$life_stage == "stage_b_6") {
        dat$K["stage_B_6"] <- dat$K["stage_B_6"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn7" | trow$life_stage == "spawners7" | trow$life_stage == "b7" | trow$life_stage == "stage_b_7") {
        dat$K["stage_B_7"] <- dat$K["stage_B_7"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn8" | trow$life_stage == "spawners8" | trow$life_stage == "b8" | trow$life_stage == "stage_b_8") {
        dat$K["stage_B_8"] <- dat$K["stage_B_8"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn9" | trow$life_stage == "spawners9" | trow$life_stage == "b9" | trow$life_stage == "stage_b_9") {
        dat$K["stage_B_9"] <- dat$K["stage_B_9"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn10" | trow$life_stage == "spawners10" | trow$life_stage == "b10" | trow$life_stage == "stage_b_10") {
        dat$K["stage_B_10"] <- dat$K["stage_B_10"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn11" | trow$life_stage == "spawners11" | trow$life_stage == "b11" | trow$life_stage == "stage_b_11") {
        dat$K["stage_B_11"] <- dat$K["stage_B_11"] * trow$sys.cap
      }
      if (trow$life_stage == "spawn12" | trow$life_stage == "spawners12" | trow$life_stage == "b12" | trow$life_stage == "stage_b_12") {
        dat$K["stage_B_12"] <- dat$K["stage_B_12"] * trow$sys.cap
      }

      # Then run for all spawner classes
      if (trow$life_stage == "spawn" | trow$life_stage == "spawners") {
        dat$K[grepl("^stage_B_", names(dat$K))] <- dat$K[grepl("^stage_B_", names(dat$K))] * trow$sys.cap
      }

      # Clear out any trailing NA value
      dat$K <- dat$K[!is.na(dat$K)]

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




  if("K" %in% names(dat)) {
    dat$K <- dat$K[!(is.na(dat$K))]
  }


  # Return the modified life history vital rate data
  return(dat)


}
