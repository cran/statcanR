## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(statcanR)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("warint/statcanR")

## -----------------------------------------------------------------------------
library(statcanR)

## ---- eval=FALSE--------------------------------------------------------------
#  # Get data with statcan_data function
#  mydata <- statcan_data("27-10-0014-01", "eng")
#  # sqs_statcandata will be depricated
#  mydata <- sqs_statcan_data("27-10-0014-01","eng")

