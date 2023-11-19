one_baby_resids <- function(one_baby_df){
    model_f1 <- lm(F1_est ~ months + I(months^2), data = one_baby_df)
    model_f2 <- lm(F2_est ~ months + I(months^2), data = one_baby_df)
    model_f3 <- lm(F3_est ~ months + I(months^2), data = one_baby_df)

    resids <-
        one_baby_df %>%
        add_predictions(model_f1, "F1_pred") %>%
        add_predictions(model_f2, "F2_pred") %>%
        add_predictions(model_f3, "F3_pred") %>%
        add_residuals(model_f1, "F1_resid") %>%
        add_residuals(model_f2, "F2_resid") %>%
        add_residuals(model_f3, "F3_resid")

    resids
}

nilam_method <- function(babies_resids_df){

    big_model_f1_f2 <-
        lmer(
            F1_resid ~ F2_resid + I(F2_resid * months) + I(F2_resid * months^2) + (0 + F2_resid | baby_id) - 1,
            data = babies_resids_df
        )

    big_model_f2_f1 <-
        lmer(
            F2_resid ~ F1_resid + I(F1_resid * months) + I(F1_resid * months^2) + (0 + F1_resid | baby_id) - 1,
            data = babies_resids_df
        )

    big_model_f1_f3 <-
        lmer(
            F1_resid ~ F3_resid + I(F3_resid * months) + I(F3_resid * months^2) + (0 + F3_resid | baby_id) - 1,
            data = babies_resids_df
        )

    big_model_f3_f1 <-
        lmer(
            F3_resid ~ F1_resid + I(F1_resid * months) + I(F1_resid * months^2) + (0 + F1_resid | baby_id) - 1,
            data = babies_resids_df
        )

    big_model_f2_f3 <-
        lmer(
            F2_resid ~ F3_resid + I(F3_resid * months) + I(F3_resid * months^2) + (0 + F3_resid | baby_id) - 1,
            data = babies_resids_df
        )

    big_model_f3_f2 <-
        lmer(
            F3_resid ~ F2_resid + I(F2_resid * months) + I(F2_resid * months^2) + (0 + F2_resid | baby_id) - 1,
            data = babies_resids_df
        )

    list(
        big_model_f1_f2 = big_model_f1_f2,
        big_model_f2_f1 = big_model_f2_f1,
        big_model_f1_f3 = big_model_f1_f3,
        big_model_f3_f1 = big_model_f3_f1,
        big_model_f2_f3 = big_model_f2_f3,
        big_model_f3_f2 = big_model_f3_f2
    )
}

clean_coef <- function(model){
    summary(model)$coefficients %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        as_tibble()
}

coef_df_to_grid <- function(coef_df, name = "noname"){
    tibble(
        age = 2:18
    ) %>%
        mutate(
            name = name,
            coupling = coef_df$Estimate[1] + coef_df$Estimate[2] * age + coef_df$Estimate[3] * age * age
        )
}
