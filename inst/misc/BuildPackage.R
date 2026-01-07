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
devtools::test()  # Run tests - all passed Jan 6 2025
devtools::check() # Operating system test

remove.packages("CEMPRA")
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::install_github("essatech/CEMPRA")
# library(CEMPRA)
# Installing unfinished package to computer...
# Check on cran against LTR version of R
# devtools::check_win_release()
# citation("CEMPRA")



#----------------------------------------------------
# pkgdown Creation
#----------------------------------------------------
# Ensure all tests pass
devtools::test()
# Must pass with no notes or warnings
devtools::check() # Operating system test
devtools::spell_check() # Spell check
# Windows Checks
devtools::check_win_devel()
# Run rhub::check_for_cran() for multi-platform validation
rhub::check_for_cran()

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

