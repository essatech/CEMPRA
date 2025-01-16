test_that("test-pop-capacity-adults", {

  # --------------------------------------------------------------------
  # Setup Automated Run with PopulationModel_Run()
  # --------------------------------------------------------------------

  # Stressor Magnitude
  fname <- system.file("extdata/nanaimo/stressor_magnitude_nanaimo.xlsx",
                       package = "CEMPRA")
  dose <- StressorMagnitudeWorkbook(filename = fname)

  # Stressor Response
  fname <- system.file("extdata/nanaimo/stressor_response_nanaimo.xlsx",
                       package = "CEMPRA")
  sr_wb_dat <- StressorResponseWorkbook(filename = fname)
  sr_wb_dat$stressor_names

  # Load location and habitat capacities
  filename <- system.file("extdata/nanaimo/habitat_capacities_nanaimo.csv",
                          package = "CEMPRA")
  habitat_dd_k <- read.csv(filename, stringsAsFactors = FALSE)

  # Load the anadromous life cycle profile
  fname <- system.file("extdata/nanaimo/species_profiles/nanaimo_comp_ocean_summer.csv",
                       package = "CEMPRA")
  life_cycle_params <- read.csv(fname)


  # Simple inputs
  HUC_ID = 1
  n_years = 200
  MC_sims = 3
  output_type = "full"
  stressors = NA

  # --------------------------------------------------------------------
  # Try running the ltre without any other inputs
  # --------------------------------------------------------------------

  ret <- pop_model_ltre(step_size = 0.05,
                             dose = dose,
                             sr_wb_dat = sr_wb_dat,
                             life_cycle_params = life_cycle_params,
                             HUC_ID = HUC_ID,
                             n_years = 3,
                             MC_sims = 1,
                             stressors = stressors,
                             habitat_dd_k = habitat_dd_k)

  # make sure this runs...
  names(ret)
  # Expect baseline to be higher than CE
  expect_true(ret$mean_ce_lambda < ret$mean_baseline_lambda)

  head(ret$ltre_summary_ce)
  tail(ret$ltre_summary_ce)

  head(ret$ltre_summary_baseline)
  tail(ret$ltre_summary_baseline)

  if(FALSE) {

    library(ggplot2)
    df <- ret$all_dat_stochastic
    df$ce <- ifelse(df$ce == "ce", "w/Stressors (ce)", df$ce)
    df$ce <- ifelse(df$ce == "baseline", "wo/Stressors (baseline)", df$ce)

    # Plot: Horizontal boxplots ----------------------------------------
    ggplot(df, aes(x = direction, y = lambda, fill = direction)) +
      # Boxplots with some transparency
      geom_boxplot(alpha = 0.5, outlier.shape = NA) +
      # Facet: this_param in rows, ce in columns
      facet_grid(rows = vars(this_param), cols = vars(ce)) +
      # Reference line at y = 1.0 (note: horizontal in this orientation)
      geom_hline(
        yintercept = 1.0,
        linetype   = "dotted",
        color      = "grey50",
        size       = 1.2
      ) +
      # Flip coordinates to make boxplots horizontal
      coord_flip() +
      # Manually set fill colors for direction
      scale_fill_manual(
        values = c("increase" = "lightpink",
                   "decrease" = "lightblue"),
        name   = "Direction"
      ) +
      labs(
        y = "Lambda Value"
      ) +
      theme_bw(base_size = 14) +
      theme(legend.position = "none") +
      theme(
        # Make 'this_param' labels (the y-strip labels) horizontal
        strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank()
      )

  }


#   getwd()
#   ggsave(filename = "testplot.png",
#          width = 7,
#          height = 13,
#          dpi = 300,
#          units = "in")


  # --------------------------------------------------------------------
  # Run some general tests
  # --------------------------------------------------------------------

  result <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycle_params,
    HUC_ID = HUC_ID,
    n_years = n_years,
    MC_sims = MC_sims,
    output_type = output_type,
    stressors = NA,
    habitat_dd_k = habitat_dd_k
  )

  spawners <- result$ce[[1]]$pop[1:200, ]
  spawners <- spawners[50:200, ]
  plot(spawners$N, type = 'l')
  m1 <- median(spawners$N)

  spawners <- result$baseline[[1]]$pop[1:200, ]
  spawners <- spawners[50:200, ]
  plot(spawners$N, type = 'l')
  m2 <- median(spawners$N)


  # Expect CE to be less than baseline
  expect_true(m1 < m2)


  # --------------------------------------------------------------------
  # Try returning matrices
  # --------------------------------------------------------------------

  result <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycle_params,
    HUC_ID = HUC_ID,
    n_years = n_years,
    MC_sims = MC_sims,
    output_type = "qa_matrices",
    stressors = stressors,
    habitat_dd_k = habitat_dd_k
  )
  names(result)
  expect_true(class(result$ce[[1]])[1] == "matrix")

  popbio::lambda(result$ce[[1]])
  popbio::lambda(result$baseline[[1]])

  # Function to calculate lambda for a list of matrices
  calculate_lambdas <- function(matrix_list) {
    # Apply lambda function to each matrix in the list
    lapply(matrix_list, function(A) {
      tryCatch(
        {
          popbio::lambda(A)  # Calculate lambda
        },
        error = function(e) {
          NA  # Return NA if there's an error
        }
      )
    })
  }

  m1 <- median(unlist(calculate_lambdas(result$ce)))
  m2 <- median(unlist(calculate_lambdas(result$baseline)))

  # Lambda should be lower when cumulative effects are included
  expect_true(m2 > m1)






})
