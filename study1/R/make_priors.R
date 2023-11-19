make_prior_1f <- function(n_items, d_mean, d_sd){
    str_glue(
        "F1 = 1-{n_items}
		PRIOR =
		(1-{n_items}, d, norm, {d_mean}, {d_sd})"
    )
}

make_prior_2f <- function(n_items, d_mean, d_sd){
    str_glue(
        "F1 = 1-{n_items}
		F2 = 1-{n_items}
		PRIOR =
		(1-{n_items}, d, norm, {d_mean}, {d_sd})"
    )
}

make_prior_3f <- function(n_items, d_mean, d_sd){
    str_glue(
        "F1 = 1-{n_items}
		F2 = 1-{n_items}
		F3 = 1-{n_items}
		PRIOR =
		(1-{n_items}, d, norm, {d_mean}, {d_sd})"
    )
}

make_prior_4f <- function(n_items, d_mean, d_sd){
    str_glue(
        "F1 = 1-{n_items}
		F2 = 1-{n_items}
		F3 = 1-{n_items}
		F4 = 1-{n_items}
		PRIOR =
		(1-{n_items}, d, norm, {d_mean}, {d_sd})"
    )
}

make_prior_5f <- function(n_items, d_mean, d_sd){
    str_glue(
        "F1 = 1-{n_items}
		F2 = 1-{n_items}
		F3 = 1-{n_items}
		F4 = 1-{n_items}
		F5 = 1-{n_items}
		PRIOR =
		(1-{n_items}, d, norm, {d_mean}, {d_sd})"
    )
}


