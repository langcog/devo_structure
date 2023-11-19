to_long <- function(p, names, name){
    p %>% as_tibble() %>% set_names(names) %>% mutate(r = row_number()) %>% gather(milestone, p, -r) %>%
        set_names("r", "milestone", name)
}

get_long_output <- function(models, folds, wide, ages, NAME){
    tmp <-
        models %>%
        select(the_fold, p) %>%
        mutate(p_long = p %>% map(to_long, names(wide), "p") %>% map(left_join, folds %>% to_long(names(wide), "fold"))) %>%
        mutate(p_long_the_fold = map2(p_long, the_fold, ~ filter(.x, fold == .y)))

    what <-
        tmp$p_long_the_fold %>%
        bind_rows() %>%
        left_join(wide %>% to_long(names(wide), "yes")) %>%
        left_join(ages %>% mutate(r = row_number()))

    what[[NAME]] <- round(what$p) == what$yes

    what
}

the_f <- function(new, wide_split, folds_split, ages_split){

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
            "Milestone Age" = "#33CC00",
            "Naive" = "darkgray"
        )

    baselines <- list()
    for (i in 1:length(wide_split)){
        baselines[[i]] <- calc_baselines(wide_split[[i]], folds_split[[i]], ages_splits[[i]])
    }

    baselines2 <-
        baselines %>%
        map2(names(wide_split), ~ mutate(.x, name = .y)) %>%
        bind_rows() %>%
        spread(baseline, correct) %>%
        select(name, yes_rate, everything()) %>%
        select(-milestone_age) %>%
        rename(milestone_age = milestone_age_rm_missing)

    results <-
        baselines2 %>%
        left_join(new %>% rename(name = age_bucket_xaxis)) %>%
        rename(naive = yes_rate) %>%
        mutate(naive = ifelse(naive < 0.5, 1 - naive, naive))

    graph_models <-
        results %>%
        select(-naive, -age, -milestone_age) %>%
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
        select(name, Rasch, Naive = naive, `Milestone Age` = milestone_age) %>%
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
        scale_color_manual(values = cols) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "", y = "Gain") +
        theme_classic(base_size = 14) +
        theme(axis.text.x = element_text(angle = 90))

    list(
        results = results,
        graph_models = graph_models,
        graph_models_baselines = graph_models_baselines,
        graph_final = graph_final
    )
}


