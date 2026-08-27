library(tidyverse)

bisley1 <- read_csv("data/QuebradaCuenca1-Bisley.csv")
bisley2 <- read_csv("data/QuebradaCuenca2-Bisley.csv")
bisley3 <- read_csv("data/QuebradaCuenca3-Bisley.csv")
prm <- read_csv("data/RioMameyesPuenteRoto.csv")


# clean each sheet -------------------------------------------------------

cleanbisley1 <- bisley1 |>
  select(Sample_Date, `NO3-N`, Mg, K, `NH4-N`, Sample_ID) |>
  filter(year(Sample_Date) >= 1988 & year(Sample_Date) <= 1994) |>
  arrange(Sample_ID, Sample_Date) |>
  mutate(window = 0) |>
  add_row(window = 1:7) |>
  mutate(window = rep(1:47, each = 9))

cleanbisley2 <- bisley2 |>
  select(Sample_Date, `NO3-N`, Mg, K, `NH4-N`, Sample_ID) |>
  filter(year(Sample_Date) >= 1988 & year(Sample_Date) <= 1994) |>
  arrange(Sample_ID, Sample_Date) |>
  mutate(window = 0) |>
  add_row(window = 1:2) |>
  mutate(window = rep(1:46, each = 9))

cleanbisley3 <- bisley3 |>
  select(Sample_Date, `NO3-N`, Mg, K, `NH4-N`, Sample_ID) |>
  filter(year(Sample_Date) >= 1988 & year(Sample_Date) <= 1994) |>
  arrange(Sample_ID, Sample_Date) |>
  mutate(window = 0) |>
  add_row(window = 1:4) |>
  mutate(window = rep(1:46, each = 9))

cleanprm <- prm |>
  select(Sample_Date, `NO3-N`, Mg, K, `NH4-N`, Sample_ID) |>
  filter(year(Sample_Date) >= 1988 & year(Sample_Date) <= 1994) |>
  arrange(Sample_ID, Sample_Date) |>
  mutate(window = 0) |>
  mutate(window = rep(1:32, each = 9))

bigdata <- bind_rows(cleanbisley1, cleanbisley3, cleanbisley2, cleanprm)

values <- bigdata |>
  group_by(window, Sample_ID) |>
  summarise_all(mean, na.rm = TRUE) |>
  arrange(Sample_ID, window) |>
  pivot_longer(
    cols = 4:7,
    names_to = "Chem",
    values_to = "conc"
  )

values |>
  ggplot(
    mapping = aes(x = Sample_Date, y = conc, color = Sample_ID)
  ) +
  geom_line() +
  facet_wrap(vars(Chem), scales = "free", ncol = 1)
