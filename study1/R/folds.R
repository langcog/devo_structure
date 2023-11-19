model_fscores_to_p <- function(m, f){
    1:length(m@Model$itemtype) %>% map(~ probtrace(extract.item(m, .), f)[ , 2]) %>% bind_cols() %>% as.matrix()
}

make_folds <- function(w){
    vector_to_fold <- function(x){
        out <- x
        notna <- which(!is.na(as.numeric(x)))
        one_to_eight <- rerun(100, sample(1:8)) %>% unlist()
        out[notna] <- one_to_eight[1:length(notna)]
        out
    }

    w %>% apply(1, vector_to_fold) %>% t()
}

get_train <- function(the_fold, folds, partition, hold_fold){
    wide_train <- partition

    for (x in c(the_fold, hold_fold)){
        wide_train[folds == x] <- NA
    }

    wide_train
}

get_test <- function(the_fold, folds, partition){
    wide_test <- partition
    wide_test[folds != the_fold] <- NA
    wide_test
}
