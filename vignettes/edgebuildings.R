## ----setup, echo = FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::opts_knit$set(root.dir = rprojroot::find_package_root_file())

## ----eval = FALSE----------------------------------------------------------------------------------------------------------------
#  renv::restore() # install dependencies into ../renv-libraries
#  devtools::install(dependencies = FALSE) # install edgebuildings
#  targets::tar_make() # run all available calculations, generate all files
#  targets::tar_visnetwork() # get overview, show status, check if everything worked

## ----eval = FALSE----------------------------------------------------------------------------------------------------------------
#  renv::restore()

## ----eval = FALSE----------------------------------------------------------------------------------------------------------------
#  targets::tar_visnetwork()

## ----eval = FALSE----------------------------------------------------------------------------------------------------------------
#  targets::tar_make("remindInputData")

