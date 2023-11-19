library(tidyverse)
library(mirt)
R.utils::sourceDirectory("study1/R")
set.seed(1)
kinedu <- read_rds("study1/data/partition-list.rds")

kinedu_models_with_priors <- map2(kinedu$partitions, kinedu$folds, fit_with_priors, 1:8, 999)
kinedu_models_without_priors <- map2(kinedu$partitions, kinedu$folds, fit_without_priors, 1:8, 999)
kinedu_models_rasch <- map2(kinedu$partitions, kinedu$folds, fit_rasch, 1:8, 999)

# save --------------------------------------------------------------------
kinedu_models_with_priors %>% write_rds("study1/data/kinedu_models_with_priors.rds")
kinedu_models_without_priors %>% write_rds("study1/data/kinedu_models_without_priors.rds")
kinedu_models_rasch %>% write_rds("study1/data/kinedu_models_rasch.rds")