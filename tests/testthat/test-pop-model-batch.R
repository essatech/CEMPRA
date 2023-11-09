test_that("Test that batch population model runs successfully...", {

    # rm(list = ls())

    # Load the stress-response workbook
    filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "CEMPRA")
    sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

    # Load the stress-magnitude workbook
    filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "CEMPRA")
    dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")

    # Load the life cycle parameters
    filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
    life_cycle_params <- read.csv(filename_lc)

    # File to read and view stressor-response relations
    filename_hab <- system.file("extdata", "habitat_dd_k.xlsx", package = "CEMPRA")
    habitat_dd_k <- readxl::read_excel(filename_hab, sheet = 1)
    colnames(habitat_dd_k)


    # Choose target ID
    HUC_ID <- dose$HUC_ID[1]

    habitat_dd_k$HUC_ID <- HUC_ID
    habitat_dd_k <- habitat_dd_k[1, ]

    # Load in the habitat data optional

    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 50,
        MC_sims = 5,
        stressors = NA,
        habitat_dd_k = habitat_dd_k
    )

    expect_true(!(is.null(data)))
    expect_true(typeof(data) == "list")
    expect_true(all(names(data) == c("ce", "baseline", "MC_sims")))
    expect_true(length((data[["ce"]])) == 5)
    expect_true(length((data[["baseline"]])) == 5)



    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 50,
        MC_sims = 20,
        stressors = NA,
        output_type = "adults"
    )
    expect_true(!(is.null(data)))
    expect_true(nrow(data) == ((50 + 1) * 2) * 20)
    expect_true(length(unique(data$MC_sim)) == 20)

    m_ce <- median(data$N[which(data$group == "ce")])
    m_no_ce <- median(data$N[which(data$group == "baseline")])

    expect_true(!(is.na(m_ce)))
    expect_true(!(is.na(m_no_ce)))


    if(FALSE) {

        library(ggplot2)
            ggplot(data, aes(x = year, y = N, color = group)) +
                stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill = group), alpha=0.3) +
                theme_bw() +
                theme(
                    legend.position = "bottom",
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8),
                    legend.key = element_blank(),
                    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
                    axis.text.y = element_text(size = 8),
                    #axis.title.x = element_blank(),
                    #axis.title.y = element_blank(),
                    strip.text = element_text(size = 8))
    }



    # ===================================================
    # Try different environmental ranges
    dose$Mean <- stats::rnorm(n = nrow(dose), 0, 0.1)
    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 50,
        MC_sims = 5,
        stressors = NA,
        output_type = "adults"
    )
    m_ce <- median(data$N[which(data$group == "ce")])
    m_no_ce <- median(data$N[which(data$group == "baseline")])
    expect_true(m_ce < m_no_ce)



    # ===================================================
    # Try different environmental ranges
    dose$Mean <- stats::rnorm(n = nrow(dose), 10000, 100)
    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 70,
        MC_sims = 5,
        stressors = NA,
        output_type = "adults"
    )
    m_ce <- median(data$N[which(data$group == "ce")], na.rm = TRUE)
    m_no_ce <- median(data$N[which(data$group == "baseline")], na.rm = TRUE)
    expect_true(m_ce < m_no_ce)






    # ===================================================
    # Set all responses to NA - should be closer to baseline
    dose$Mean <- NA
    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 70,
        MC_sims = 5,
        stressors = NA,
        output_type = "adults"
    )

    m_ce <- median(data$N[which(data$group == "ce")], na.rm = TRUE)
    m_no_ce <- median(data$N[which(data$group == "baseline")], na.rm = TRUE)
    diff <- abs(m_ce / m_no_ce - 1)
    # within 20% of baseline
    expect_true(diff < 0.2)




    if(FALSE) {

        library(ggplot2)
            ggplot(data, aes(x = year, y = N, color = group)) +
                stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill = group), alpha=0.3) +
                theme_bw() +
                theme(
                    legend.position = "bottom",
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8),
                    legend.key = element_blank(),
                    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
                    axis.text.y = element_text(size = 8),
                    #axis.title.x = element_blank(),
                    #axis.title.y = element_blank(),
                    strip.text = element_text(size = 8))
    }





    # ===================================================
    # Try setting k values with actual inputs

    # Load the life cycle parameters
    filename_lc <- system.file("extdata", "./species_profiles/life_cycles_test_dd2.csv", package = "CEMPRA")
    life_cycle_params <- read.csv(filename_lc)

    # File to read and view stressor-response relations
    filename_hab <- system.file("extdata", "habitat_dd_k.xlsx", package = "CEMPRA")
    habitat_dd_k <- readxl::read_excel(filename_hab, sheet = 1)
    colnames(habitat_dd_k)

    # Choose target ID
    HUC_ID <- dose$HUC_ID[1]

    habitat_dd_k$HUC_ID <- HUC_ID
    habitat_dd_k <- habitat_dd_k[1, ]

    # Load in the habitat data optional
    data <- PopulationModel_Run(
      dose = dose,
      sr_wb_dat = sr_wb_dat,
      life_cycle_params = life_cycle_params,
      HUC_ID = HUC_ID,
      n_years = 250,
      MC_sims = 2,
      output_type = "adults",
      stressors = NA,
      habitat_dd_k = habitat_dd_k
    )


    if(FALSE) {
      library(ggplot2)
      data <- data[which(data$year > 100), ]
      ggplot(data, aes(x = year, y = N, color = group)) +
        stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill = group), alpha=0.3) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          strip.text = element_text(size = 8))
    }






    # ===================================================
    # Try running with simple SM interactions
    # ===================================================

    # Load the life cycle parameters
    filename_lc <- system.file("extdata", "life_cycles.csv", package = "CEMPRA")
    life_cycle_params <- read.csv(filename_lc)

    # Load the stress-response workbook
    filename_sr <- system.file("extdata", "./simple_test/stressor_response_simple.xlsx", package = "CEMPRA")
    sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

    # Load the stress-magnitude workbook
    filename_rm <- system.file("extdata", "./simple_test/stressor_magnitude_simple.xlsx", package = "CEMPRA")
    dose <- StressorMagnitudeWorkbook(filename = filename_rm)

    # Choose target ID
    HUC_ID <- dose$HUC_ID[1]

    # Load in the habitat data optional
    data <- PopulationModel_Run(
      dose = dose,
      sr_wb_dat = sr_wb_dat,
      life_cycle_params = life_cycle_params,
      HUC_ID = HUC_ID,
      n_years = 100,
      MC_sims = 3,
      output_type = "adults",
      stressors = sr_wb_dat$stressor_names
    )

    if(FALSE) {
      library(ggplot2)
      data <- data[which(data$year > 50), ]
      ggplot(data, aes(x = year, y = N, color = group)) +
        stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill = group), alpha=0.3) +
        theme_bw() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 8),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          strip.text = element_text(size = 8))
    }

    no_ce <- median(data$N[which(data$group == "baseline")], na.rm = TRUE)
    with_ce <- median(data$N[which(data$group == "ce")], na.rm = TRUE)

    # Expect fewer with CE
    expect_true(!(is.na(with_ce)))
    expect_true(with_ce > 0)
    expect_true(with_ce < no_ce)




})
