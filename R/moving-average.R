moving_average <- function(df, datefrom, dateto, interval, samplesite) {
  # Initialize a tibble to contain the results
  result <- tibble(
    window_start = seq(from = as.Date(datefrom), to = as.Date(dateto), by = interval),
    k_mgl = NA,
    mg_mgl = NA,
    no3 = NA,
    Ca = NA,
    NH4 = NA,
    site = samplesite

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
    result$k_mgl[x] <- mean(k_window, na.rm = TRUE)
    result$mg_mgl[x] <- mean(mg_window, na.rm =TRUE)
    result$no3[x] <- mean(no3_window, na.rm= TRUE)
    result$Ca[x] <- mean(Ca_window, na.rm =TRUE)
    result$NH4[x] <- mean(nh4_window, na.rm = TRUE)
  }
  
  # Return the result
  return(result)
}