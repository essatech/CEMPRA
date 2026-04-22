# -------------------------------------------------------
# Joe Model Flowchart for CEMPRA Vignette a01
# Creates a big-picture diagram of the Joe Model workflow
#
# Requirements: DiagrammeR, htmlwidgets, webshot2
# (or DiagrammeRsvg + rsvg as an alternative export path)
#
# Usage:
#   Rscript joe_model_flowchart.R
#   # produces joe_model_flowchart.png in the working directory
# -------------------------------------------------------

library(DiagrammeR)
library(htmlwidgets)
library(webshot2)

graph_spec <- '
digraph joe_model_flow {

  # Graph settings
  graph [
    rankdir = TB,
    fontname = Helvetica,
    bgcolor = white,
    nodesep = 0.6,
    ranksep = 0.7,
    margin = 0.2
  ]

  node [
    fontname = Helvetica,
    fontsize = 11,
    style = filled,
    penwidth = 1.2
  ]

  edge [
    fontname = Helvetica,
    fontsize = 9,
    color = "#555555",
    arrowsize = 0.8
  ]

  # --- Input files (Excel workbooks) ---
  subgraph cluster_inputs {
    label = "Excel Workbook Inputs"
    labeljust = l
    fontsize = 12
    fontcolor = "#444444"
    style = dashed
    color = "#AAAAAA"
    margin = 15

    sr_file [
      label = "Stressor-Response\nWorkbook (.xlsx)",
      shape = note,
      fillcolor = "#FFF3CD",
      color = "#D4A017"
    ]
    sm_file [
      label = "Stressor-Magnitude\nWorkbook (.xlsx)",
      shape = note,
      fillcolor = "#FFF3CD",
      color = "#D4A017"
    ]
  }

  # --- Import functions ---
  subgraph cluster_import {
    label = "Step 1: Import Data"
    labeljust = l
    fontsize = 12
    fontcolor = "#444444"
    style = dashed
    color = "#AAAAAA"
    margin = 15

    sr_func [
      label = "StressorResponseWorkbook()",
      shape = box,
      fillcolor = "#D1ECF1",
      color = "#0C7B93",
      fontcolor = "#0C5460"
    ]
    sm_func [
      label = "StressorMagnitudeWorkbook()",
      shape = box,
      fillcolor = "#D1ECF1",
      color = "#0C7B93",
      fontcolor = "#0C5460"
    ]
  }

  # --- Intermediate objects ---
  subgraph cluster_objects {
    label = "R Objects"
    labeljust = l
    fontsize = 12
    fontcolor = "#444444"
    style = dashed
    color = "#AAAAAA"
    margin = 15

    sr_obj [
      label = "sr_wb_dat\n(list: main_sheet,\nsr_dat, stressor_names,\npretty_names, MInt)",
      shape = folder,
      fillcolor = "#E8E8E8",
      color = "#777777"
    ]
    dose_obj [
      label = "dose\n(data.frame: HUC_ID,\nStressor, Mean, SD, ...)",
      shape = folder,
      fillcolor = "#E8E8E8",
      color = "#777777"
    ]
  }

  # --- Internal processing (inside JoeModel_Run) ---
  subgraph cluster_joe {
    label = "Step 2: JoeModel_Run()"
    labeljust = l
    fontsize = 12
    fontcolor = "#444444"
    style = rounded
    color = "#28A745"
    bgcolor = "#F0FFF0"
    penwidth = 2
    margin = 15

    mean_resp [
      label = "mean_Response()\nBuild interpolation\nfunctions for each\ndose-response curve",
      shape = box,
      fillcolor = "#C3E6CB",
      color = "#28A745",
      fontcolor = "#155724"
    ]

    sys_cap [
      label = "SystemCapacity()\nFor each HUC x Stressor:\n1. Sample random dose\n2. Evaluate dose-response\n3. Return system capacity (0-1)",
      shape = box,
      fillcolor = "#C3E6CB",
      color = "#28A745",
      fontcolor = "#155724"
    ]

    ce_func [
      label = "Cumulative Effect\nMultiply system capacities\nacross stressors per HUC\n(handle linked stressors)",
      shape = box,
      fillcolor = "#C3E6CB",
      color = "#28A745",
      fontcolor = "#155724"
    ]

    mc_label [
      label = "Repeat for\nMC_sims\niterations",
      shape = plaintext,
      fillcolor = transparent,
      fontsize = 9,
      fontcolor = "#666666"
    ]
  }

  # --- Outputs ---
  subgraph cluster_outputs {
    label = "Step 3: Model Outputs"
    labeljust = l
    fontsize = 12
    fontcolor = "#444444"
    style = dashed
    color = "#AAAAAA"
    margin = 15

    ce_df [
      label = "ce.df\nCumulative system capacity\nper HUC x simulation",
      shape = cylinder,
      fillcolor = "#D6D8FF",
      color = "#4A4EC7",
      fontcolor = "#2D2F7F"
    ]
    sc_dose [
      label = "sc.dose.df\nPer-stressor capacity\nand dose values",
      shape = cylinder,
      fillcolor = "#D6D8FF",
      color = "#4A4EC7",
      fontcolor = "#2D2F7F"
    ]
  }

  # --- Edges ---
  sr_file -> sr_func
  sm_file -> sm_func

  sr_func -> sr_obj
  sm_func -> dose_obj

  sr_obj -> mean_resp
  sr_obj -> sys_cap
  dose_obj -> sys_cap

  mean_resp -> sys_cap [label = "  response\n  functions"]
  sys_cap -> ce_func [label = "  per-stressor\n  capacity"]
  mc_label -> sys_cap [style = dotted, arrowhead = none]

  ce_func -> ce_df
  ce_func -> sc_dose

}
'

# Render the graph
g <- grViz(graph_spec)

# Save as HTML widget then screenshot to PNG
tmp_dir <- tempdir()
tmp_html <- file.path(tmp_dir, "flowchart_joe.html")
saveWidget(g, tmp_html, selfcontained = FALSE)
webshot(tmp_html, "joe_model_flowchart.png",
        vwidth = 900, vheight = 1200, zoom = 3, delay = 2)

cat("Flowchart saved to:", normalizePath("joe_model_flowchart.png"), "\n")
