i <<- 1
fit_with_priors <- function(partition, folds, use_folds, hold_folds, PRIOR_SD = 3){

    # 1f
    print("______________________STARTING 1F___________________________________")
    models_with_priors_1f <-
        crossing(
            n_items = ncol(partition),
            dim = 1,
            the_fold = use_folds,
            d_mean = c(0),
            d_sd = c(PRIOR_SD)
        ) %>%
        mutate(prior = pmap_chr(select(., -dim, -the_fold), make_prior_1f)) %>%
        mutate(
            model = map2(prior, the_fold, ~ mirt(get_train(.y, folds, partition, hold_folds), mirt.model(.x), method = "EM")),
            fscores = map(model, ~ fscores(., rotate = "none", QMC = FALSE)),
            p = map2(model, fscores, model_fscores_to_p),
            acc = map2_dbl(the_fold, p, ~ mean(((.y > 0.5) + 0) == as.matrix(get_test(.x, folds, partition)), na.rm = TRUE))
        )

    #2f
    print("______________________STARTING 2F___________________________________")
    models_with_priors_2f <-
        crossing(
            n_items = ncol(partition),
            dim = 2,
            the_fold = use_folds,
            d_mean = c(0),
            d_sd = c(PRIOR_SD)
        ) %>%
        mutate(prior = pmap_chr(select(., -dim, -the_fold), make_prior_2f)) %>%
        mutate(
            model = map2(prior, the_fold, ~ mirt(get_train(.y, folds, partition, hold_folds), mirt.model(.x), method = "EM")),
            fscores = map(model, ~ fscores(., rotate = "none", QMC = TRUE)),
            p = map2(model, fscores, model_fscores_to_p),
            acc = map2_dbl(the_fold, p, ~ mean(((.y > 0.5) + 0) == as.matrix(get_test(.x, folds, partition)), na.rm = TRUE))
        )

    # 3f
    print("______________________STARTING 3F___________________________________")
    models_with_priors_3f <-
        crossing(
            n_items = ncol(partition),
            dim = 3,
            the_fold = use_folds,
            d_mean = c(0),
            d_sd = c(PRIOR_SD)
        ) %>%
        mutate(prior = pmap_chr(select(., -dim, -the_fold), make_prior_3f)) %>%
        mutate(
            model = map2(prior, the_fold, ~ mirt(get_train(.y, folds, partition, hold_folds), mirt.model(.x), method = "QMCEM")),
            fscores = map(model, ~ fscores(., rotate = "none", QMC = FALSE)),
            p = map2(model, fscores, model_fscores_to_p),
            acc = map2_dbl(the_fold, p, ~ mean(((.y > 0.5) + 0) == as.matrix(get_test(.x, folds, partition)), na.rm = TRUE))
        )

    # 4f
    print("______________________STARTING 4F___________________________________")
    models_with_priors_4f <-
        crossing(
            n_items = ncol(partition),
            dim = 4,
            the_fold = use_folds,
            d_mean = c(0),
            d_sd = c(PRIOR_SD)
        ) %>%
        mutate(prior = pmap_chr(select(., -dim, -the_fold), make_prior_4f)) %>%
        mutate(
            model = map2(prior, the_fold, ~ mirt(get_train(.y, folds, partition, hold_folds), mirt.model(.x), method = "QMCEM")),
            fscores = map(model, ~ fscores(., rotate = "none", QMC = TRUE)),
            p = map2(model, fscores, model_fscores_to_p),
            acc = map2_dbl(the_fold, p, ~ mean(((.y > 0.5) + 0) == as.matrix(get_test(.x, folds, partition)), na.rm = TRUE))
        )

    # 5f
    print("______________________STARTING 5F___________________________________")
    models_with_priors_5f <-
        crossing(
            n_items = ncol(partition),
            dim = 5,
            the_fold = use_folds,
            d_mean = c(0),
            d_sd = c(PRIOR_SD)
        ) %>%
        mutate(prior = pmap_chr(select(., -dim, -the_fold), make_prior_5f)) %>%
        mutate(
            model = map2(prior, the_fold, ~ mirt(get_train(.y, folds, partition, hold_folds), mirt.model(.x), method = "QMCEM")),
            fscores = map(model, ~ fscores(., rotate = "none", QMC = TRUE)),
            p = map2(model, fscores, model_fscores_to_p),
            acc = map2_dbl(the_fold, p, ~ mean(((.y > 0.5) + 0) == as.matrix(get_test(.x, folds, partition)), na.rm = TRUE))
        )

    print(paste0(i, "TH PARTITION COMPLETE"))
    i <<- i + 1

    # output
    list(
        models_with_priors_1f = models_with_priors_1f,
        models_with_priors_2f = models_with_priors_2f,
        models_with_priors_3f = models_with_priors_3f,
        models_with_priors_4f = models_with_priors_4f,
        models_with_priors_5f = models_with_priors_5f
    )
}
