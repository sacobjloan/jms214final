# How will you get the data into R?
# above easy mode
# How will you clean up the data?
# probably set an NA value, then take those values out
# Are any processing or modeling steps required?
# Lots of summarizing, creating a moving average function
# What are your outputs?
# Graph of the moving 9 week average

# read in data and set packages ------------------------------------------

library(tidyverse)

bisley1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")
bisley2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")
bisley3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")
prm <- read_csv("data/RioMameyesPuenteRoto.csv")

# clean up data ----------------------------------------------------------

# remove irrelevant columns
#
