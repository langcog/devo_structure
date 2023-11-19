library(tidyverse)
library(mirt)
set.seed(1)
R.utils::sourceDirectory("study1/R")

folds  <- read_rds("study1/data/folds.rds")
wide <- read_rds("study1/data/wide.rds")
ages <- read_rds("study1/data/ages.rds") %>% mutate(age_bucket = age)

kinedu_survey_models_with_priors <- read_rds("study1/data/full-with-priors.rds")
kinedu_survey_models_without_priors <- read_rds("study1/data/full-without-priors.rds")
kinedu_survey_models_rasch <- read_rds("study1/data/full-rasch.rds")

# SEE OVERALL PERFORMANCE -------------------------------------------------
baseline <- calc_baselines(wide, folds, ages)

rasch_perf <-
    kinedu_survey_models_rasch %>%
    mutate(dim = "Rasch") %>%
    list() %>%
    clean_models_without_priors() %>%
    rename(Rasch = Raschf)

without_perf <-
    kinedu_survey_models_without_priors %>%
    list() %>%
    clean_models_without_priors()

with_perf <-
    kinedu_survey_models_with_priors %>%
    list() %>%
    clean_models_with_priors()

bind_cols(rasch_perf, without_perf, with_perf) %>%
    select(-starts_with("name")) %>%
    gather(Model, Accuracy) %>%
    arrange(desc(Accuracy)) %>%
    bind_rows(
        baseline %>%
            filter(baseline != "milestone_age") %>%
            mutate(baseline = ifelse(baseline == "milestone_age_rm_missing", "milestone_age", baseline)) %>%
            mutate(baseline = paste0("Baseline: ", baseline)) %>%
            rename(Model = baseline, Accuracy = correct) %>%
            arrange(desc(Accuracy))
    ) %>%
    write_rds("data-for-paper/study1-full-results-overall.rds")

# PERF BY AGE - GET READY -------------------------------------------------
b1 <- get_long_output(kinedu_survey_models_with_priors %>% bind_rows() %>% filter(dim == 1), folds, wide, ages, "dim 1f")
b2 <- get_long_output(kinedu_survey_models_with_priors %>% bind_rows() %>% filter(dim == 2), folds, wide, ages, "dim 2f")
b3 <- get_long_output(kinedu_survey_models_with_priors %>% bind_rows() %>% filter(dim == 3), folds, wide, ages, "dim 3f")
b4 <- get_long_output(kinedu_survey_models_with_priors %>% bind_rows() %>% filter(dim == 4), folds, wide, ages, "dim 4f")
b5 <- get_long_output(kinedu_survey_models_with_priors %>% bind_rows() %>% filter(dim == 5), folds, wide, ages, "dim 5f")

# BIG BUCKETS -------------------------------------------------------------
all_year_buckets <-
    list(b1, b2, b3, b4, b5) %>%
    map(~ select(., r, milestone, age, starts_with("dim"))) %>%
    reduce(left_join) %>%
    mutate(
        age_bucket_xaxis =
            case_when(
                age %in% 2:11 ~ "02-11",
                age %in% 12:23 ~ "12-23",
                age %in% 24:35 ~ "24-35",
                age %in% 36:55 ~ "36-55",
                TRUE ~ "ERROR"
            )
    )

results <-
    all_year_buckets %>%
    rename(
        # Rasch = `dim 0f`,
        `1f w/ prior` = `dim 1f`,
        `2f w/ prior` = `dim 2f`,
        `3f w/ prior` = `dim 3f`,
        `4f w/ prior` = `dim 4f`,
        `5f w/ prior` = `dim 5f`
    ) %>%
    select(-r, -milestone, -age) %>%
    gather(model, right, -age_bucket_xaxis) %>%
    group_by(age_bucket_xaxis, model) %>%
    summarize(mean = mean(right)) %>%
    ungroup() %>%
    spread(model, mean)

results %>% write_rds("data-for-paper/study1-full-results-by-partition.rds")
