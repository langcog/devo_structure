library(tidyverse)
set.seed(7)
source("study1/R/folds.R")

wide <- read_rds("study1/data/wide.rds")
ages <- read_rds("study1/data/ages.rds")

age1 <- ages %>% filter(age %in% 2:11)
age2 <- ages %>% filter(age %in% 12:23)
age3 <- ages %>% filter(age %in% 24:35)
age4 <- ages %>% filter(age %in% 36:55) # check: (nrow(age1) + nrow(age2) + nrow(age3) + nrow(age4)) == nrow(wide)

# MAKE PARTITIONS ---------------------------------------------------------
partition1 <- wide[ages$age %in% 2:11, ]
partition1 <- partition1[ , map_dbl(partition1, mean) > 0.025 & map_dbl(partition1, mean) < 0.975]
dim(partition1)
mean(!is.na(partition1))

partition2 <- wide[ages$age %in% 12:23, ]
partition2 <- partition2[ , map_dbl(partition2, mean) > 0.025 & map_dbl(partition2, mean) < 0.975]
dim(partition2)
mean(!is.na(partition2))

partition3 <- wide[ages$age %in% 24:35, ]
partition3 <- partition3[ , map_dbl(partition3, mean) > 0.025 & map_dbl(partition3, mean) < 0.975]
dim(partition3)
mean(!is.na(partition3))

partition4 <- wide[ages$age %in% 36:55, ]
partition4 <- partition4[ , map_dbl(partition4, mean) > 0.025 & map_dbl(partition4, mean) < 0.975]
dim(partition4)
mean(!is.na(partition4))

# MAKE FOLDS --------------------------------------------------------------
folds1 <- make_folds(partition1)
folds2 <- make_folds(partition2)
folds3 <- make_folds(partition3)
folds4 <- make_folds(partition4)

# OUTPUT ------------------------------------------------------------------
list(
    partitions = list(partition1 = partition1, partition2 = partition2, partition3 = partition3, partition4 = partition4),
    folds = list(folds1 = folds1, folds2 = folds2, folds3 = folds3, folds4 = folds4),
    ages = list(age1 = age1, age2 = age2, age3 = age3, age4 = age4)
) %>%
    write_rds("study1/data/partition-list.rds")
