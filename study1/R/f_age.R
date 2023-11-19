# AGE ---------------------------------------------------------------------
f_age <- function(NAMES, partitions, folds, ages, models_rasch, models_without_priors, models_with_priors){

    # CALC BASELINES
    baselines <- list()
    for (i in 1:length(partitions)){
        baselines[[i]] <- calc_baselines(partitions[[i]], folds[[i]], ages[[i]])
    }
    baselines <- map2(baselines, NAMES, ~ mutate(.x, name = .y))
    baselines <- bind_rows(baselines) %>% spread(baseline, correct) %>% select(name, yes_rate, everything())

    # RESULTS TABLE
    results <-
        baselines %>%
        left_join(models_rasch %>% map(~ mutate(., dim = "Rasch")) %>% clean_models_without_priors() %>% rename(Rasch = Raschf) %>% mutate(name = NAMES)) %>%
        left_join(models_without_priors %>% clean_models_without_priors() %>% mutate(name = NAMES)) %>%
        left_join(models_with_priors %>% clean_models_with_priors() %>% mutate(name = NAMES)) %>%
        rename(naive = yes_rate) %>%
        mutate(naive = ifelse(naive < 0.5, 1 - naive, naive))

    # GRAPHING COLORS AND ORDER
    levs <-
        c(
            "Naive", "Milestone", "Milestone Age",
            "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior"
        )

    cols <-
        c(
            "5f w/ prior" = "#FF9900",
            "4f w/ prior" = "#FF0000",
            "3f w/ prior" = "#CC00FF",
            "2f w/ prior" = "#3300FF",
            "1f" = "#0099FF",
            "Rasch" = "black",
            "Milestone Age" = "blue",
            "Naive" = "darkgray"
        )


    graph_models <-
        results %>%
        select(-naive, -milestone_age, -milestone_age_rm_missing, -`1f w/ prior`, -`3f`, -`5f`) %>%
        gather(var, val, -name) %>%
        filter(!var %in% c("age", "milestone")) %>%
        mutate(var = factor(var, levels = levs) %>% fct_rev()) %>%
        ggplot(aes(x = val, y = name, color = var)) +
        geom_point(size = 4) +
        geom_path(aes(group = var), alpha = 0.5) +
        coord_flip() +
        labs(title = "Model performance", x = "Out-of-sample accuracy", y = "", color = "") +
        scale_x_continuous(labels = scales::percent) +
        cowplot::theme_minimal_hgrid() +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_color_manual(values = cols)

    graph_models_baselines <-
        results %>%
        select(name, Rasch, Naive = naive, `Milestone Age` = milestone_age_rm_missing) %>%
        gather(var, val, -name) %>%
        mutate(Model = factor(var, levels = levs) %>% fct_rev()) %>%
        ggplot(aes(x = val, y = name, color = Model)) +
        geom_point(size = 4) +
        geom_path(aes(group = var), alpha = 0.5) +
        coord_flip() +
        labs(title = "Baselines", x = "Out-of-sample accuracy", y = "", color = "") +
        scale_x_continuous(labels = scales::percent) +
        cowplot::theme_minimal_hgrid() +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_color_manual(values = cols)

    graph_milestone_age <-
        results %>%
        select(name, milestone_age, Rasch, `1f`, `2f w/ prior`, `3f w/ prior`, `4f w/ prior`, `5f w/ prior`) %>%
        gather(var, val, -name, -milestone_age) %>%
        mutate(var = factor(var, levels = c("naive", "age", "milestone", "milestone_age", "milestone_age_rm_missing", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%
        mutate(gain = gain(val, milestone_age)) %>%
        mutate(label = "Gain over Milestone Age")

    graph_rasch <-
        results %>%
        select(name, Rasch, `1f`, `2f w/ prior`, `3f w/ prior`, `4f w/ prior`, `5f w/ prior`) %>%
        gather(var, val, -name, -Rasch) %>%
        mutate(var = factor(var, levels = c("naive", "age", "milestone", "milestone_age", "milestone_age_rm_missing", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%
        mutate(gain = gain(val, Rasch))  %>%
        mutate(label = "Gain over Rasch")

    graph_1f <-
        results %>%
        select(name, `1f`, `2f w/ prior`, `3f w/ prior`, `4f w/ prior`, `5f w/ prior`) %>%
        gather(var, val, -name, -`1f`) %>%
        mutate(var = factor(var, levels = c("naive", "age", "milestone", "milestone_age", "milestone_age_rm_missing", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%
        mutate(gain = gain(val, `1f`)) %>%
        mutate(label = "Gain over 1f")

    graph_final <-
        bind_rows(graph_milestone_age, graph_rasch, graph_1f) %>%
        mutate(label = factor(label, levels = c("Gain over naive", "Gain over milestone", "Gain over Milestone Age","Gain over Rasch", "Gain over 1f")) %>% fct_rev()) %>%
        rename(Model = var) %>%
        ggplot(aes(x = name, y = gain, color = Model)) +
        geom_point(size = 3) +
        geom_path(aes(group = Model), alpha = 0.5) +
        facet_wrap(~ label, scales = "free_y", ncol = 1) +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_color_manual(values = cols) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "", y = "Gain")

    list(results = results, graph_models_baselines = graph_models_baselines, graph_models = graph_models, graph_final = graph_final)
}
