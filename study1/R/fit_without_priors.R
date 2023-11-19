i <<- 1
fit_without_priors <- function(partition, folds, use_folds, hold_folds){
    models_multidim <-
        crossing(dim = 1:5, the_fold = use_folds) %>%
        mutate(
            model = map2(dim, the_fold, ~ mirt(get_train(.y, folds, partition, hold_folds), .x, method = ifelse(.x >= 3, "QMCEM", "EM"))),
            fscores = map2(dim, model, ~ fscores(.y, rotate = "none", QMC = ifelse(.x >= 3, TRUE, FALSE))),
            p = map2(model, fscores, model_fscores_to_p),
            acc = map2_dbl(the_fold, p, ~ mean(((.y > 0.5) + 0) == as.matrix(get_test(.x, folds, partition)), na.rm = TRUE))
        )

    print(paste0(i, "TH PARTITION COMPLETE"))
    i <<- i + 1

    models_multidim
}
