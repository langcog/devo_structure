understand_model <- function(model, rot){
    model@Options$exploratory <- TRUE
    cfs <- coef(model, rotate = rot)
    sv <- mod2values(model)
    sv$est <- FALSE
    tmp <- unlist(cfs)
    sv$value <- tmp
    out <- as.data.frame(cbind(summary(model, rotate=rot)$rotF, summary(model,rotate=rot)$h2))

    propvar <- apply(out^2/nrow(out), 2, sum)
    propvar[length(propvar)] <- sum(propvar[-length(propvar)])

    out$milestone <- row.names(out)

    out <-
        out %>%
        as_tibble()

    list(load = out, propvar = propvar, fcor = summary(model, rotate=rot)$fcor)
}

loadings_none <- function(mod, name){
    coef(mod, simplify = TRUE, rotate = "none")$items %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        as_tibble() %>%
        select(-g, -u, -d) %>%
        rename(paste = rowname) %>%
        mutate(model = name)
}

loadings <- function(mod, name){
    coef(mod, simplify = TRUE, rotate = "oblimin")$items %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        as_tibble() %>%
        select(-g, -u, -d) %>%
        rename(paste = rowname) %>%
        mutate(model = name)
}