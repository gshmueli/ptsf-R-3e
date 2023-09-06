#####################################
# Practical Time Series Forecasting 
# Load packages
# Created by: Julia Polak
####################################

library(here)  # Works only inside of a project. Set the pathway to find your files in the project.
library(rlang)
library(fabletools) #  Provides tools, helpers and data structures for developing models and time series functions for 'fable' and extension packages.
library(fable) # provides common forecasting methods for tsibble, such as ARIMA and ETS. 
library(tsibble) # provides a data infrastructure for tidy temporal data with wrangling tools.
library(feasts) # provides support for visualizing data and extracting time series features.
library(slider) 
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra) # for arranging multi-panel plots
library(readr)  # for saving csv
library(scales) # for changing time x-axis format
library(stringr) # for replacing strings in text (e.g., Problem 9 Ch 5)

######################################
sessionInfo()  # to see which package version is loaded
#####################################

#  R version 4.2.3 (2023-03-15 ucrt)
#  [1] scales_1.2.1     readr_2.1.4      gridExtra_2.3    ggplot2_3.4.2    lubridate_1.9.2  tidyr_1.3.0     
#  [7] dplyr_1.1.1      slider_0.3.0     feasts_0.3.1     tsibble_1.1.3    fable_0.3.3      fabletools_0.3.3
# [13] rlang_1.1.0      here_1.0.1  
