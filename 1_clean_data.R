library(tidyverse)
source("R/moving-average.R")

bis1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")
bis2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")
bis3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")
prm <- read_csv("data/RioMameyesPuenteRoto.csv")


# creating dataframes for each site in analysis, including a site ID for future 
# data sorting
valuesb1 <- moving_average(bis1,"1988-01-05", "1994-12-27", "9 weeks", "b1")
valuesb2 <-moving_average(bis2,"1988-01-05", "1994-12-27", "9 weeks", "b2")
valuesb3 <-moving_average(bis3,"1988-01-05", "1994-12-27", "9 weeks", "b3")
valuesrmp <-moving_average(prm,"1988-01-05", "1994-12-27", "9 weeks", "rmp")

bigdata <- bind_rows(valuesb1,valuesb2,valuesb3,valuesrmp) |> 
    pivot_longer( #pivot longer for easy plotting later
    cols = 2:6,
    names_to = "Chem",
    values_to = "conc"
  )

write_csv(bigdata, "output/cleandata.csv")
