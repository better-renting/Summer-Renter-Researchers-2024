#Install packages
#Uncomment to install

# Libraries used
library(ggplot2)
library(viridis)
library(gridExtra)
library(ggforce)
#library(xpose4)
library(MASS)
library(dplyr)
library(tidyr)
library(ggpubr)

# Density function so we can sort/colour by density on scatter plots
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#=========================================
#   Setup output folders
#=========================================

# Function to create directory if it doesn't exist
create_directory <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  } else {
    cat("Directory already exists:", dir_path, "\n")
  }
}

# Create directory for Device data summaries
create_directory("data_summaries/Device/")

# Create directory for State data summaries
create_directory("data_summaries/State/")

# Create directory for Device plots (PDF format)
create_directory("plots/Device/PDF/")

# Create directory for Device plots (PNG format)
create_directory("plots/Device/PNG/")

# Create directory for State plots (PDF format)
create_directory("plots/State/PDF/")

# Create directory for State plots (PNG format)
create_directory("plots/State/PNG/")



