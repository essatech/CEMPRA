#----------------------------------------------------
# Build and Tests
#----------------------------------------------------

library(usethis)
library(testthat)
library(rhub)
library(devtools)
library(qpdf)
library(kableExtra)
library(testthat)

# Loading unfinished package to memory...
rm(list = ls())
devtools::load_all()
devtools::document()
devtools::test()  # Run tests - all passed Nov 19 2024
devtools::check() # Operating system test


remove.packages("CEMPRA")
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::install_github("essatech/CEMPRA")
# library(CEMPRA)
# Installing unfinished package to computer...


list.files('./inst/extdata/matrix_test/')

# Check on cran against LTR version of R
# devtools::check_win_release()




# citation("CEMPRA")



#-------------------------------
# Check External Function Calls
#-------------------------------
library(tidyverse)
library(rgdal)
library(plyr)
library(shiny)
library(DT)
library(readxl)
library(leaflet)
library(tidyr)
library(tidyselect)
library(reshape2)
library(rmapshaper)
library(popbio)
library(parallel)
library(MASS)
library(pracma)
library(ggplot2)
library(plotly)
library(NCmisc)
list.functions.in.file("./R/Utility_JoeModel.R")
list.functions.in.file("./R/Utility_CumulativeEffect.R")
list.functions.in.file("./R/Utility_PopulationModel.R")
list.functions.in.file("./R/SystemCapacity.R")
list.functions.in.file("./R/StressorMagnitudeWorkbook.R")
list.functions.in.file("./R/StressorResponseWorkbook.R")
list.functions.in.file("./R/Projection_DD.R")
list.functions.in.file("./R/mean_Response.R")
list.functions.in.file("./R/beta_param.R")
list.functions.in.file("./R/JoeModel_Run.R")
lintr::lint("./R/Utility_JoeModel.R")


# For Shiny App
library(NCmisc)
fnames <- list.files("./R")
blist <- list()
for(f in 1:length(fnames)) {
  print(fnames[f])
  fsource <- list.functions.in.file(paste0("./R/", fnames[f]))
  blist[[f]] <- names(fsource)
}
unique(unlist(blist))



#----------------------------------------------------
# More Serious Tests
#----------------------------------------------------
# Generate cran-comments.md file
# results <- rhub::check_for_cran()
# Get the summary of your results
# results$cran_summary()

# Check for good practices
library(goodpractice)
goodpractice::gp()

# Build Manual
# install.packages('tinytex')
# library(tinytex)
# tinytex::install_tinytex()
# tinytex::tlmgr_install("makeindex")
# tinytex::reinstall_tinytex()
# devtools::build_manual(pkg = ".", path = "../")
# devtools::build_manual(pkg = ".")
# library(devtools)
# ?build_manual()

library(inteRgrate)
inteRgrate::check_pkg()



#----------------------------------------------------
# pkgdown Creation
#----------------------------------------------------
# https://pkgdown.r-lib.org/
# install.packages("pkgdown")
library(pkgdown)
usethis::use_pkgdown_github_pages()
pkgdown::clean_site()
pkgdown::build_site()
pkgdown::build_site(new_process = FALSE)


# Steps to build pages.
# 1. build pages...
# 2. push to github...
# 3. go to settings and set the github pages to the docs folder...
# Main branch and launching from the docs folder...

























#----------------------------------------------------
# Make logo
#----------------------------------------------------
# See: https://github.com/GuangchuangYu/hexSticker
library(hexSticker)
# image12.png
imgurl <- "image12.png"
sticker(imgurl, package="JoeModelCE", p_size=20, s_x=1, s_y=.65, s_width=.7,
        filename="logo.png")


#----------------------------------------------------
# Data Prep Notes
#----------------------------------------------------
# Setting Up polygons
# In QGIS open up polygon layer - filter to target basin.
# no more than 200 units..
# Then processing, Toolbox, v.generalize... set max toloerange around 50 - 100
# Reduce so file size is less than 5MB otherwise App performnace will suffer
# Remove all columns except one called HUC_10 and NAME... if you do not have HUC_10 codes
# dont worrry just create a unique ID column and label is as HUC_10.
# Save as geopacakge
# right click final layer; turn off edit mode. click Export > Save Feature As
# then in pop up box. Change format to GeoPackage;
# CRS EPSG:3857 - WGS 84 / Pseudo-Mercator
# the default projections for Leaflet.js
# engsure the Layer Options GEOMETRY_NAME is set to geom
# save as a e.g., watersheds.gpkg

