install.packages("tidyverse")
install.packages("fixest")
install.packages("ggtext")
install.packages("binsreg")
install.packages("gridExtra")
install.packages("grid")

library(tidyverse)
library(fixest)
library(ggtext)
library(binsreg)
library(gridExtra)
library(grid)

rm(list = ls())
set.seed(123)
options(max.print = 250)

# This code returns the main estimates.
#
# NOTE:
# mop: modern system operational
# pmq: prescriber must query

# Set the path
file_location <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(file_location)) # set path to location

# Auxiliary functions
source(paste(getwd(), "code/data_analysis/utilities.R", sep = "/"))
source(paste(getwd(), "code/data_analysis/plots.R", sep = "/"))
source(paste(getwd(), "code/data_construction/data_merge.R", sep = "/"))
source(paste(getwd(), "code/data_analysis/results.R", sep = "/"))

# Obtain the results
datasets(getwd())
regressions(getwd())

