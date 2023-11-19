i <<- 1
fit_rasch <- function(partition, folds, use_folds, hold_folds){
    models_rasch <-
        tibble(the_fold = use_folds) %>%
        mutate(
            model = the_fold %>% map(~ mirt(get_train(., folds, partition, hold_folds), 1, itemtype = "Rasch")),
            fscores = model %>% map(~ fscores(., rotate = "none")),
            p = map2(model, fscores, model_fscores_to_p),
            acc = map2_dbl(the_fold, p, ~ mean(((.y > 0.5) + 0) == as.matrix(get_test(.x, folds, partition)), na.rm = TRUE))
        )

    print(paste0(i, "TH PARTITION COMPLETE"))
    i <<- i + 1

    models_rasch
}
