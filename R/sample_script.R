library(CalledStrike)
library(tidyverse)

statcast2017 %>%
  setup_sc() %>%
  sample_n(size = 50000) %>%
  gam_fit() %>%
  cplot()
