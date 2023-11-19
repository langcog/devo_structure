library(tidyverse)
set.seed(7)

survey <-
    read_csv("data-raw/survey.csv") %>%
    filter(age >= 2)

wide <- survey %>% select(-age)

ages <- survey %>% select(age) %>% mutate(age_bucket = floor(age / 4))

source("study1/R/folds.R")
folds <- make_folds(wide)

folds %>% write_rds("study1/data/folds.rds")
wide %>% write_rds("study1/data/wide.rds")
ages %>% write_rds("study1/data/ages.rds")
