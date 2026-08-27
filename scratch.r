library(tidyverse)

# The input to this function should be a data frame containing stream chemistry data
moving_average <- function(df, datefrom, dateto, interval) {
  # Initialize a tibble to contain the results
  result <- tibble(
    window_start = seq(from = as.Date(datefrom), to = as.Date(dateto), by = interval),
    k_mgl = NA,
    mg_mgl = NA,
    no3 = NA,
    Ca = NA,
    NH4 = NA

    # Fill in the rest of the ions
  )

  # Fill in the iterator and sequence
  for (x in 1:(length(result$window_start))) {
    # Create variables for the start and end of the current window
    w1 <- result$window_start[x]
    w2 <- result$window_start[x + 1]

    # Create a logical vector, called "in_window", that says which samples are inside the window
    # Hint: you'll compare sample dates to the start and end of the window
    in_window <- df[["Sample_Date"]] >= w1 & df[["Sample_Date"]] <= w2

    # Use indexing to pull out the ion concentrations that fall inside the window
    k_window <- df$K[in_window]
    mg_window <- df$Mg[in_window]
    no3_window <- df$`NO3-N`[in_window]
    Ca_window <- df$Ca[in_window]
    nh4_window <- df$`NH4-N`[in_window]
    # The line above gets potassium in the window. Get the rest of the ions too

    # Calculate the mean of each ion concentration and fill in the result
    result$k_mgl[x] <- mean(k_window)
    result$mg_mgl[x] <- mean(mg_window)
    result$no3[x] <- mean(no3_window)
    result$Ca[x] <- mean(Ca_window)
    result$NH4[x] <- mean(nh4_window)
  }
  
  # Return the result
  return(result)
}

bis1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")
bis2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")
bis3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")
prm <- read_csv("data/RioMameyesPuenteRoto.csv")

valuesb1 <- moving_average(bis1,"1988-01-05", "1994-12-27", "9 weeks")
valuesb2 <-moving_average(bis2,"1988-01-05", "1994-12-27", "9 weeks")
valuesb3 <-moving_average(bis3,"1988-01-05", "1994-12-27", "9 weeks")
valuesrmp <-moving_average(prm,"1988-01-05", "1994-12-27", "9 weeks")

valuesb1<- valuesb1 |> 
  mutate(site = "b1")

valuesb2 <- valuesb2 |> 
  mutate(site = "b2")

valuesb3 <- valuesb3 |> 
  mutate(site = "b3")

valuesrmp<- valuesrmp |> 
  mutate(site = "prm")

bigdata <- bind_rows(valuesb1,valuesb2,valuesb3,valuesrmp)

finisheddata <- bigdata |> 
  group_by(window_start, site) |>
  summarise_all(mean, na.rm = TRUE) |>
  arrange(site, window_start) |>
  pivot_longer(
    cols = 3:7,
    names_to = "Chem",
    values_to = "conc"
  )

finisheddata |> 
    ggplot(
    mapping = aes(x = window_start, y = conc, color = site)
  ) +
  geom_line() +
  facet_wrap(vars(Chem), scales = "free", ncol = 1)
