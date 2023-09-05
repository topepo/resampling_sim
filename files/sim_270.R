library(tidymodels)
library(doMC)
library(sessioninfo)
library(glue)

# ------------------------------------------------------------------------------

tidymodels_prefer()
registerDoMC(cores = parallel::detectCores(logical = TRUE))

# ------------------------------------------------------------------------------

set.seed(270)
chr_seed <- format(1:1000)[270]
chr_seed <- gsub(" ", "0", chr_seed)

f <- expr(-1 - 4 * A - 2 * B - 0.2 * A^2 + 1 * B^2)

set.seed(270 + 1)
sim_tr  <- modeldata::sim_logistic(200, f)
sim_new <- modeldata::sim_logistic(1000, f)
sim_te  <- modeldata::sim_logistic(50, f)
sim_big <- modeldata::sim_logistic(10^6, f)

# ------------------------------------------------------------------------------

lr_spline_rec <-
  recipe(class ~ A + B, data = sim_tr) %>%
  step_spline_natural(A, B, deg_free = 4)
lr_spline_wflow <- workflow(lr_spline_rec, logistic_reg())
lr_spline_fit <- fit(lr_spline_wflow, data = sim_tr)

lr_spline_te_pred <- augment(lr_spline_fit, sim_te)
lr_spline_big_pred <- augment(lr_spline_fit, sim_big)

# ------------------------------------------------------------------------------

test_res <-
  lr_spline_te_pred %>%
  brier_class(class, .pred_one) %>%
  mutate(method = "Test Set", n = 1, std_err = NA_real_) %>%
  select(brier = .estimate, n, std_err, method) %>%
  mutate(seed = 270)

big_res <-
  lr_spline_big_pred %>%
  brier_class(class, .pred_one) %>%
  mutate(method = "Large Sample", n = 1, std_err = NA_real_) %>%
  select(brier = .estimate, n, std_err, method) %>%
  mutate(seed = 270)

save(test_res, big_res, file = file.path(glue("holdout_{chr_seed}.RData")))

# ------------------------------------------------------------------------------

re_run <- function(rs, times = 10) {
  require(foreach)
  set.seed(270 + 2)
  cl <- match.call()$rs
  cl_chr <- rlang::expr_deparse(cl)

  # parallel over longest loop
  res <-
    foreach(i = 1:times, .combine = dplyr::bind_rows) %dopar%
    measure_brier(i, cl, lr_spline_wflow)
  res <- res %>%
    dplyr::mutate(
      large_est = big_res$brier,
      seed = chr_seed,
      replicate = format(replicate),
      replicate = gsub(" ", "0", replicate)
      ) %>%
    select(seed, replicate, starts_with("id"), .estimate, large_est)
}
measure_brier <- function(i, rs, wflow) {
  rs <- rlang::eval_tidy(rs)
  fit_resamples(
    wflow,
    resamples = rs,
    metrics = metric_set(brier_class),
    control = control_resamples(allow_par = TRUE)
  ) %>%
    collect_metrics(summarize = FALSE) %>% 
    mutate(replicate = i)
}

# ------------------------------------------------------------------------------
# V-Fold CV

rs_v_fold <- NULL
for (reps in 1:10) {
  for (v_val in c(5, 10, 15, 20)) {
    rs_v_fold <-
      rs_v_fold %>%
      bind_rows(
        re_run(
          vfold_cv(sim_tr, repeats = reps, v = v_val, strata = class)
        ) %>%
          mutate(repeats = reps, folds = v_val)
      )
  }
}

save(rs_v_fold, big_res, file = file.path(glue("v_fold_{chr_seed}.RData")))

# ------------------------------------------------------------------------------
# Bootstrapping

B <- (1:10) * 10

rs_boot <- NULL
for (iters in B) {
  rs_boot <-
    rs_boot %>%
    bind_rows(
      re_run(
        bootstraps(sim_tr, times = iters)
      ) %>%
        mutate(times = iters)
    )
}

save(rs_boot, big_res, file = file.path(glue("boot_{chr_seed}.RData")))

# ------------------------------------------------------------------------------
# Monte-Carlo CV

retain_pct <- c(0.5, 0.6, 0.7, 0.8, 0.9)

rs_mc_cv <- NULL
for (iters in B) {
  for (pct in retain_pct) {
    rs_mc_cv <-
      rs_mc_cv %>%
      bind_rows(
        re_run(mc_cv(sim_tr, times = iters, prop = pct, strata = class)) %>%
          mutate(retain = pct, times = iters)
      )
  }
}

save(rs_mc_cv, big_res, file = file.path(glue("mc_cv_{chr_seed}.RData")))

# ------------------------------------------------------------------------------
# Validation Set

rs_val <- NULL
for (pct in retain_pct) {
  rs_val <-
    rs_val %>%
    bind_rows(
      re_run(
        validation_split(sim_tr, prop = pct, strata = class)
      ) %>%
        mutate(retain = pct)
    )
}

save(rs_val, big_res, file = file.path(glue("val_{chr_seed}.RData")))


# ------------------------------------------------------------------------------

resub <- re_run(apparent(sim_tr), times = 1)
save(resub, file = file.path(glue("resub_{chr_seed}.RData")))

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if(!interactive()) {
  q("no")
}
