# NO AGE ------------------------------------------------------------------
f_no_age <- function(NAMES, partitions, folds, models_rasch, models_without_priors, models_with_priors){
    baselines <- list()
    for (i in 1:length(partitions)){
        baselines[[i]] <- calc_baselines_no_age(partitions[[i]], folds[[i]])
    }
    baselines <- map2(baselines, NAMES, ~ mutate(.x, name = .y))
    baselines <- bind_rows(baselines) %>% spread(baseline, correct) %>% select(name, yes_rate, everything())

    results <-
        baselines %>%
        left_join(models_rasch %>% map(~ mutate(., dim = "Rasch")) %>% clean_models_without_priors() %>% rename(Rasch = Raschf) %>% mutate(name = NAMES)) %>%
        left_join(models_without_priors %>% clean_models_without_priors() %>% mutate(name = NAMES)) %>%
        left_join(models_with_priors %>% clean_models_with_priors() %>% mutate(name = NAMES)) %>%
        rename(naive = yes_rate) %>%
        mutate(naive = ifelse(naive < 0.5, 1 - naive, naive))

    graph_models <-
        results %>%
        select(-`1f w/ prior`, -`3f`, -`5f`) %>%
        gather(var, val, -name) %>%
        filter(!var %in% c("age", "milestone_age")) %>%
        mutate(Model = factor(var, levels = c("naive", "milestone", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%
        ggplot(aes(x = val, y = name, color = Model)) +
        geom_point() +
        geom_path(aes(group = var), alpha = 0.2) +
        coord_flip() +
        labs(title = "Model performance", x = "Out-of-sample accuracy", y = "") +
        scale_x_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 90))  +

        scale_color_discrete(drop = FALSE)

    graph_models_just_models <-
        results %>%
        select(-`1f w/ prior`, -`3f`, -`5f`) %>%
        gather(var, val, -name) %>%
        filter(!var %in% c("age", "milestone_age")) %>%
        mutate(Model = factor(var, levels = c("naive", "milestone", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%

        filter(!Model %in% c("milestone", "naive")) %>%

        ggplot(aes(x = val, y = name, color = Model)) +
        geom_point() +
        geom_path(aes(group = var), alpha = 0.2) +
        coord_flip() +
        labs(title = "Model performance", x = "Out-of-sample accuracy", y = "") +
        scale_x_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 90))  +

        scale_color_discrete(drop = FALSE)

    graph_models_baselines <-
        results %>%
        select(-`1f w/ prior`, -`3f`, -`5f`) %>%
        gather(var, val, -name) %>%
        filter(!var %in% c("age", "milestone_age")) %>%
        mutate(Model = factor(var, levels = c("naive", "milestone", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%

        filter(Model %in% c("milestone", "naive")) %>%

        ggplot(aes(x = val, y = name, color = Model)) +
        geom_point() +
        geom_path(aes(group = var), alpha = 0.2) +
        coord_flip() +
        labs(title = "Baselines", x = "Out-of-sample accuracy", y = "") +
        scale_x_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 90)) +

        scale_color_discrete(drop = FALSE)

    graph_naive <-
        results %>%
        select(name, naive, Rasch, `1f`, `2f w/ prior`, `3f w/ prior`, `4f w/ prior`, `5f w/ prior`) %>%
        gather(var, val, -name, -naive) %>%
        mutate(Model = factor(var, levels = c("naive", "milestone", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%
        mutate(gain = gain(val, naive)) %>%
        mutate(label = "Gain over naive")

    graph_milestone <-
        results %>%
        select(name, milestone, Rasch, `1f`, `2f w/ prior`, `3f w/ prior`, `4f w/ prior`, `5f w/ prior`) %>%
        gather(var, val, -name, -milestone) %>%
        mutate(Model = factor(var, levels = c("naive", "milestone", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%
        mutate(gain = gain(val, milestone)) %>%
        mutate(label = "Gain over milestone")

    graph_rasch <-
        results %>%
        select(name, Rasch, `1f`, `2f w/ prior`, `3f w/ prior`, `4f w/ prior`, `5f w/ prior`) %>%
        gather(var, val, -name, -Rasch) %>%
        mutate(Model = factor(var, levels = c("naive", "milestone", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%
        mutate(gain = gain(val, Rasch))  %>%
        mutate(label = "Gain over Rasch")

    graph_1f <-
        results %>%
        select(name, `1f`, `2f w/ prior`, `3f w/ prior`, `4f w/ prior`, `5f w/ prior`) %>%
        gather(var, val, -name, -`1f`) %>%
        mutate(Model = factor(var, levels = c("naive", "milestone", "Rasch", "1f", "2f w/ prior","3f w/ prior", "4f w/ prior", "5f w/ prior")) %>% fct_rev()) %>%
        mutate(gain = gain(val, `1f`)) %>%
        mutate(label = "Gain over 1f")

    graph_final <-
        bind_rows(graph_naive, graph_milestone, graph_rasch, graph_1f) %>%
        mutate(label = factor(label, levels = c("Gain over naive", "Gain over milestone", "Gain over Rasch", "Gain over 1f"))) %>%
        # rename(Model = var) %>%
        ggplot(aes(x = name, y = gain, color = Model)) +
        geom_point() +
        geom_path(aes(group = Model), alpha = 0.2) +
        facet_wrap(~ label, scales = "free_y") +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x = "", y = "Gain") +
        scale_y_continuous(labels = scales::percent) +

        scale_color_discrete(drop = FALSE)

    list(results = results, graph_models = graph_models, graph_models_just_models = graph_models_just_models, graph_models_baselines = graph_models_baselines, graph_final = graph_final)
}
