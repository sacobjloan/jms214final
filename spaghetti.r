# filter data to include correct chems, date range, and nothing else
# find the summaries for each chemical by site
# combine all findings into a master sheet
# plot by site / chem


library(tidyverse)



bisley1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")
bisley2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")
bisley3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")
prm <- read_csv("data/RioMameyesPuenteRoto.csv")

bigbigdata <- bind_rows(bisley1,bisley2,bisley3, prm)

bigdata <- bigdata |> 
  select(Sample_Date, `NO3-N`, Mg, K, `NH4-N`, Sample_ID) |> 
  filter(year(Sample_Date)>= 1988 & year(Sample_Date) <= 1994) |> 
  arrange(Sample_ID, Sample_Date)


# just bisley 1 ----------------------------------------------------------

cleanbisley <- bisley1 |> 
  select(Sample_Date, `NO3-N`, Mg, K, `NH4-N`, Sample_ID) |> 
  filter(year(Sample_Date)>= 1988 & year(Sample_Date) <= 1994) |> 
  arrange(Sample_ID, Sample_Date) |> 
  mutate(window = 0) |> 
  add_row(window = 1:7) |> 
  mutate(window = rep(1:47, each = 9))

values <- cleanbisley |> 
  group_by(window) |> 
  summarise_all(mean, na.rm = TRUE) |> 
  pivot_longer(
    cols = 3:6,
    names_to = "Chem",
    values_to = "conc"
  ) |> 
  arrange(Chem)

values |> 
  ggplot(
    mapping = aes(x = Sample_Date, y = conc)
  )+
  geom_line() +
  facet_wrap(vars(Chem))



