getwd()

## read in dataset
fileName <- list.files("data")
data <- read.csv(paste0("data/",fileName))


library(readr)
library(data.table) #this library contains the fread() function
data <- fread(paste0("data/",fileName))

head(data)
