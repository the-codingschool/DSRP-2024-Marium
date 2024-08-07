library(readr)
library(tidyr) 
library(tidyverse) 
library(dplyr) 
library(ggplot2) 
library(data.table)

fileName <- list.files("DSRP Arush/data")
data <- read.csv(paste0("DSRP Arush/data",fileName))