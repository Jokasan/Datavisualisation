######################################
## Author: Nils Indreiten           ##
## Date: 2026-01-29                 ##
## Description: This script         ##
## explores companies in Braxil.    ##
## TidyTuesday submission,          ##
## 2026-01-27 dataset.              ##
######################################

# retrieve the data:
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2026-01-27')
## OR

companies <- tuesdata$companies
