add_names <- function(.list){imap(.list, ~ mutate(.x, what = .y))}

gain <- function(flex_acc, base_acc){(flex_acc - base_acc) / (1 - base_acc)}

clean_models_with_priors <- function(models_with_priors){
    map(models_with_priors, bind_rows) %>%
        imap(~ mutate(.x, name = .y)) %>%
        bind_rows() %>%
        group_by(name, dim, prior) %>%
        summarize(min = min(acc), med = median(acc), max = max(acc)) %>%
        filter(med == max(med)) %>%
        slice(1) %>%
        ungroup() %>%
        select(-prior, -min, -max) %>%
        mutate(dim = paste0(dim, "f w/ prior")) %>%
        spread(dim, med)
}

clean_models_without_priors <- function(models_without_priors){
    models_without_priors %>%
        imap(~ mutate(.x, name = .y)) %>%
        bind_rows() %>%
        group_by(name, dim) %>%
        summarize(min = min(acc), med = median(acc), max = max(acc)) %>%
        ungroup() %>%
        select(-min, -max) %>%
        mutate(dim = paste0(dim, "f")) %>%
        spread(dim, med)
}
