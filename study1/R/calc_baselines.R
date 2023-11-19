calc_baselines <- function(partition, folds, ages){

    ages <- ages %>% mutate(r = row_number())
    folds_long <- folds %>% as_tibble() %>% mutate(r = row_number()) %>% gather(milestone, fold, -r)
    wide_long <- partition %>% as_tibble() %>% mutate(r = row_number()) %>% gather(milestone, yes, -r)

    long <-
        wide_long %>%
        left_join(folds_long) %>%
        left_join(ages) %>%
        na.omit() %>%
        filter(fold %in% 1:8)

    f_age <- function(held_out_fold){
        predictions <-
            long %>%
            filter(fold != held_out_fold) %>%
            group_by(age_bucket) %>%
            summarize(p = mean(yes), n = n()) %>%
            ungroup()

        tmp <-
            long %>%
            filter(fold == held_out_fold) %>%
            left_join(predictions) %>%
            mutate(correct = (p > 0.5 & yes == 1) | (p < 0.5 & yes == 0))

        tibble(held_out_fold = held_out_fold, n = nrow(tmp), correct = sum(tmp$correct))
    }

    f_milestone <- function(held_out_fold){
        predictions <-
            long %>%
            filter(fold != held_out_fold) %>%
            group_by(milestone) %>%
            summarize(p = mean(yes), n = n()) %>%
            ungroup()

        tmp <-
            long %>%
            filter(fold == held_out_fold) %>%
            left_join(predictions) %>%
            mutate(correct = (p > 0.5 & yes == 1) | (p < 0.5 & yes == 0))

        tibble(held_out_fold = held_out_fold, n = nrow(tmp), correct = sum(tmp$correct))
    }

    f_milestone_age <- function(held_out_fold){

        # browser() # WHAT THIS DO

        predictions <-
            long %>%
            filter(fold != held_out_fold) %>%
            group_by(milestone, age_bucket) %>%
            summarize(p = mean(yes), n = n()) %>%
            ungroup()

        tmp <-
            long %>%
            filter(fold == held_out_fold) %>%
            left_join(predictions) %>%
            mutate(correct = (p > 0.5 & yes == 1) | (p < 0.5 & yes == 0))

        tibble(held_out_fold = held_out_fold, n = nrow(tmp), correct = sum(tmp$correct), correct_rm_missing = mean(tmp$correct, na.rm = TRUE))
    }

    milestone <- map_df(1:8, f_milestone)
    age <- map_df(1:8, f_age)
    milestone_age <- map_df(1:8, f_milestone_age)

    baselines <-
        tibble(
            baseline = c("yes_rate", "age", "milestone", "milestone_age", "milestone_age_rm_missing"),
            correct = c(mean(as.matrix(partition), na.rm = TRUE), sum(age$correct) / sum(age$n), sum(milestone$correct) / sum(milestone$n), sum(milestone_age$correct) / sum(milestone_age$n), mean(milestone_age$correct_rm_missing))
        )

    baselines
}

calc_baselines_no_age <- function(partition, folds){

    folds_long <- folds %>% as_tibble() %>% mutate(r = row_number()) %>% gather(milestone, fold, -r)
    wide_long <- partition %>% as_tibble() %>% mutate(r = row_number()) %>% gather(milestone, yes, -r)

    long <-
        wide_long %>%
        left_join(folds_long) %>%
        na.omit() %>%
        filter(fold %in% 1:8)

    f_milestone <- function(held_out_fold){
        predictions <-
            long %>%
            filter(fold != held_out_fold) %>%
            group_by(milestone) %>%
            summarize(p = mean(yes), n = n()) %>%
            ungroup()

        tmp <-
            long %>%
            filter(fold == held_out_fold) %>%
            left_join(predictions) %>%
            mutate(correct = (p > 0.5 & yes == 1) | (p < 0.5 & yes == 0))

        tibble(held_out_fold = held_out_fold, n = nrow(tmp), correct = sum(tmp$correct))
    }

    milestone <- map_df(1:8, f_milestone)

    baselines <-
        tibble(
            baseline = c("yes_rate", "milestone"),
            correct = c(mean(as.matrix(partition), na.rm = TRUE), sum(milestone$correct) / sum(milestone$n))
        )

    baselines
}
