
library(tidyverse)

bisley1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")

rollavg <- function (df, datecol, valuecol, datefrom, dateto, interval){
  window_start <- seq(from = as.Date(datefrom), to = as.Date(dateto), by = interval)
  for (x in 1:(length(window_start))){
    w1 <- result$window_start[x]
    w2 <- result$window_start[x + 1]
    help <- mean(df[[valuecol]][df[[datecol]] >= w1 & df[[datecol]] <= w2], na.rm = TRUE)
    print(help)
    } 
}


tibble(data = rollavg(bisley1, "Sample_Date", "K", "1990-06-26", "1993-03-02", "9 weeks"))
x <- rollavg(bisley1, "Sample_Date", "K", "1990-06-26", "1993-03-02", "9 weeks")
result$k_mgl <- rollavg(bisley1, "Sample_Date", "K", "1990-06-26", "1993-03-02", "9 weeks")
result$mg_mgl <- rollavg(bisley1, "Sample_Date", "Mg", "1990-06-26", "1993-03-02", "9 weeks")



rollavgdate <- function (df, datecol, valuecol, datefrom, dateto, interval){
  window_start <- seq(from = as.Date(datefrom), to = as.Date(dateto), by = interval)
  for (x in 1:(length(window_start))){
    w1 <- result$window_start[x]
    w2 <- result$window_start[x + 1]

    conc <- tibble(date = NA, values = NA, window = window_start)

    conc[x,2] <- mean(df[[valuecol]][df[[datecol]] >= w1 & df[[datecol]] <= w2], na.rm = TRUE)
    } 
}











  result <- tibble(
    window_start = seq(from = as.Date("1988-01-05"), to = as.Date("1994-12-27"), by = "9 weeks"),
    k_mgl = NA,
    mg_mgl = NA,
    NH4 = NA,
    NO3 = NA,
    windownum = NA)

for (___ in ___) {
    # Create variables for the start and end of the current window
    w1 <- result$window_start[x]
    w2 <- result$window_start[x + 1]

    # Create a logical vector, called "in_window", that says which samples are inside the window
    # Hint: you'll compare sample dates to the start and end of the window
    in_window <- (bisley1$Sample_Date >= w1 bisley1$Sample_Date <= w2)

    # Use indexing to pull out the ion concentrations that fall inside the window
    k_window <- bisley1$K[___]
    # The line above gets potassium in the window. Get the rest of the ions too

    # Calculate the mean of each ion concentration and fill in the result
    result$k_mgl[___] <- mean(___)
  }
    
  }

  new_column1 <- function(df,col_name,col1,col2){
    #Create new column col_name as sum of col1 and col2
    df[[col_name]] <- df[[col1]] + df[[col2]]
    df
}



rollavg <- function (df, datecol, valuecol, datefrom, dateto, interval){

  window_start <- seq(from = as.Date(datefrom), to = as.Date(dateto), by = interval)

  for (x in 1:(length(window_start))){
    w1 <- result$window_start[x]
    w2 <- result$window_start[x + 1]

    help <- mean(df[[valuecol]][df[[datecol]] >= w1 & df[[datecol]] <= w2], na.rm = TRUE)
    print(help)
    } 
}

for (x in 1:41){
    w1 <- result$window_start[x]
    w2 <- result$window_start[x + 1]

    result$k_mgl[x] <- mean(bisley1$K[bisley1$Sample_Date >= w1 & bisley1$Sample_Date <= w2], na.rm = TRUE)
    } 

rollavg(bisley1, "Sample_Date", "K")
rollavg(bisley1, "Sample_Date", "K", "1990-06-26", "1993-03-02", "9 weeks")