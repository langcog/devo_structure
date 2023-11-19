library(tidyverse)
library(mirt)
set.seed(1)
R.utils::sourceDirectory("study1/R")

folds  <- read_rds("study1/data/folds.rds")
wide <- read_rds("study1/data/wide.rds")
ages <- read_rds("study1/data/ages.rds")

# fit models --------------------------------------------------------------
kinedu_survey_models_with_priors <- fit_with_priors(wide, folds, 1:8, 999)
kinedu_survey_models_without_priors <- fit_without_priors(wide, folds, 1:8, 999)
kinedu_survey_models_rasch <- fit_rasch(wide, folds, 1:8, 999)

# save --------------------------------------------------------------------
kinedu_survey_models_with_priors %>% write_rds("study1/data/full-with-priors.rds")
kinedu_survey_models_without_priors %>% write_rds("study1/data/full-without-priors.rds")
kinedu_survey_models_rasch %>% write_rds("study1/data/full-rasch.rds")
