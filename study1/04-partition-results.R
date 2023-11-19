add_zero <- function(x){
    ifelse(nchar(x) == 1, paste0("0", x), x)
}

library(tidyverse)
library(mirt)
set.seed(1)
R.utils::sourceDirectory("study1/R")

kinedu <- read_rds("study1/data/partition-list.rds")
kinedu_models_with_priors <- read_rds("study1/data/kinedu_models_with_priors.rds")
kinedu_models_without_priors <- read_rds("study1/data/kinedu_models_without_priors.rds")
kinedu_models_rasch <- read_rds("study1/data/kinedu_models_rasch.rds")

out_kinedu <-
    f_age(
        NAMES = c("part1", "part2", "part3", "part4"),
        partitions = kinedu$partitions,
        folds = kinedu$folds,
        ages = kinedu$ages,
        models_rasch = kinedu_models_rasch,
        models_without_priors = kinedu_models_without_priors,
        models_with_priors = kinedu_models_with_priors
    )

out_kinedu$results %>% write_rds("data-for-paper/study1-partition-results.rds")
