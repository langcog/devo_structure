model_to_bootstrap_ci <- function(model){
    f <- function(m){
        m %>% clean_coef() %>% coef_df_to_grid() %>% pull(coupling)
    }

    out <- lme4::bootMer(model, f, nsim=1000, use.u=FALSE, type="parametric")

    tibble(
        age = 2:18,
        low = apply(out$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
        high = apply(out$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
}

get_no_missing <- function(THE_PART){
    # get_no_missing(kinedu_database$partitions[[10]])
    # find the most common number of questions answered
    row_sums <- rowSums(!is.na(THE_PART))
    row_sums_table <- sort(table(row_sums), decreasing = TRUE)

    # subtle logic to get the 2nd amount if it is close to the 1st and has more than 3 times the items
    if ((row_sums_table[2] >= row_sums_table[1] * 0.9) & (as.numeric(names(row_sums_table[2])) >= as.numeric(names(row_sums_table[1])) * 3)) {
        row_sum_to_focus_on <- as.numeric(names(row_sums_table[2]))
    } else {
        row_sum_to_focus_on <- as.numeric(names(row_sums_table[1]))
    }

    # get just those rows to figure out those columns
    THE_PART_to_focus_on <- THE_PART[row_sums == row_sum_to_focus_on, ]
    columns_to_focus_on <- names(sort(colMeans(!is.na(THE_PART_to_focus_on)), decreasing = TRUE))[1:row_sum_to_focus_on]

    # consider only people who answered ALL OF THESE QUESTIONS
    THE_PART[ , columns_to_focus_on] %>% na.omit()
}

get_no_missing_with_age <- function(THE_PART){

    resp <- THE_PART %>% select(-1, -2, -3)
    demo <- THE_PART %>% select(1, 2, 3)

    # find the most common number of questions answered
    row_sums <- rowSums(!is.na(resp))
    row_sums_table <- sort(table(row_sums), decreasing = TRUE)

    # subtle logic to get the 2nd amount if it is close to the 1st and has more than 3 times the items
    if ((row_sums_table[2] >= row_sums_table[1] * 0.9) & (as.numeric(names(row_sums_table[2])) >= as.numeric(names(row_sums_table[1])) * 3)) {
        row_sum_to_focus_on <- as.numeric(names(row_sums_table[2]))
    } else {
        row_sum_to_focus_on <- as.numeric(names(row_sums_table[1]))
    }

    # get just those rows to figure out those columns
    resp_to_focus_on <- resp[row_sums == row_sum_to_focus_on, ]
    demo_to_focus_on <- demo[row_sums == row_sum_to_focus_on, ]


    columns_to_focus_on <- names(sort(colMeans(!is.na(resp_to_focus_on)), decreasing = TRUE))[1:row_sum_to_focus_on]

    # consider only people who answered ALL OF THESE QUESTIONS
    bind_cols(demo, resp[ , columns_to_focus_on]) %>%
        na.omit()
}

need_atleast_x_percent <- function(df, x){
    df[ , (colMeans(!is.na(df)) >= x) & (colMeans(df, na.rm = TRUE) > 0.01) & (colMeans(df, na.rm = TRUE) < 0.99)]
}

partition_to_cor <- function(partition, use){
    partition %>%
        cor(use = use) %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        as_tibble() %>%
        gather(var, val, -rowname) %>%
        filter(var > rowname) %>%
        set_names(c("milestone1", "milestone2", "corr"))
}

partition_to_cor_mean <- function(partition, use){
    partition %>%
        partition_to_cor(use) %>%
        pull(corr) %>%
        mean()
}

partition_to_eig_df <- function(partition, use){
    tmp <- cor(partition, use = use)
    ev <- eigen(tmp)

    tibble(
        eig = ev$values
    ) %>%
        mutate(p = eig / sum(eig)) %>%
        mutate(factor = row_number()) %>%
        select(factor, everything())
}

partition_to_prop_1f <- function(partition, use){
    partition %>%
        partition_to_eig_df(use) %>%
        pull(p) %>%
        first()
}

summarize_descriptives <- function(partition_list, labels = paste0("Timepoint ", 1:length(partition_list))){
    tibble(
        kids = partition_list %>% map_dbl(nrow),
        milestones = partition_list %>% map_dbl(ncol),
        `% missing` = partition_list %>% map_dbl(~ mean(is.na(as.matrix(.)))),
        `% correct` = partition_list %>% map_dbl(~ mean(as.matrix(.), na.rm = TRUE)),
        `r = Mean Item Corr` = map_dbl(partition_list, partition_to_cor_mean, "everything"),
        `α = Cronbach Alpha` = map_dbl(partition_list, DescTools::CronbachAlpha),
        `V = Prop Var 1st Eig` = map_dbl(partition_list, partition_to_prop_1f, "everything"),
    ) %>%
        mutate(Dataset = labels) %>%
        select(Dataset, everything())
}
