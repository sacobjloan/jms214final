rollavg <- function (df, datecol, valuecol, datefrom, dateto, interval){
  window_start <- seq(from = as.Date(datefrom), to = as.Date(dateto), by = interval)
  for (x in 1:(length(window_start))){
    w1 <- result$window_start[x]
    w2 <- result$window_start[x + 1]
    help <- mean(df[[valuecol]][df[[datecol]] >= w1 & df[[datecol]] <= w2], na.rm = TRUE)
    print(help)
    } 
}