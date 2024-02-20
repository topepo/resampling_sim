library(tidymodels)
library(doMC)
library(sessioninfo)
library(glue)

# ------------------------------------------------------------------------------

tidymodels_prefer()
registerDoMC(cores = parallel::detectCores(logical = TRUE))

# ------------------------------------------------------------------------------

set.seed(998)
chr_seed <- format(1:1000)[998]
chr_seed <- gsub(" ", "0", chr_seed)

f <- expr(-1 - 4 * A - 2 * B - 0.2 * A^2 + 1 * B^2)

set.seed(998 + 1)
sim_tr  <- modeldata::sim_logistic(200, f)
sim_new <- modeldata::sim_logistic(1000, f)
sim_te  <- modeldata::sim_logistic(50, f)
sim_big <- modeldata::sim_logistic(10^6, f)

# ------------------------------------------------------------------------------

cls_met <- metric_set(brier_class, roc_auc, accuracy)

# ------------------------------------------------------------------------------
# Fit the model to the entire training set then predict the test and large data

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
  cls_met(class, estimate = .pred_class, .pred_one) %>%
  mutate(method = "Test Set", n = 1, std_err = NA_real_) %>%
  mutate(seed = 998, model = "logistic")

big_res <-
  lr_spline_big_pred %>%
  cls_met(class, estimate = .pred_class, .pred_one) %>%
  mutate(method = "Large Sample", n = 1, std_err = NA_real_) %>%
  mutate(seed = 998, model = "logistic")

save(test_res, big_res, file = file.path(glue("holdout_logistic_{chr_seed}.RData")))

# ------------------------------------------------------------------------------
# V-Fold CV

rs_v_fold <- NULL

for (reps in 1:10) {
  for (v_val in c(5, 10, 15, 20)) {

    tmp_rs <- vfold_cv(sim_tr, repeats = reps, v = v_val, strata = class)
    tmp_res <-
      lr_spline_wflow %>%
      fit_resamples(
        resamples = tmp_rs,
        metrics = cls_met
      ) %>%
      collect_metrics()%>%
      mutate(repeats = reps, folds = v_val, estimator = "standard",
             seed = 998, model = "logistic")
    rs_v_fold <- bind_rows(rs_v_fold, tmp_res)

  }
}

save(rs_v_fold, file = file.path(glue("v_fold_logistic_{chr_seed}.RData")))

# ------------------------------------------------------------------------------
# Bootstrapping

B <- (1:10) * 10

rs_boot <- rs_boot_permute <- NULL
for (iters in B) {
  tmp_rs <- bootstraps(sim_tr, times = iters, apparent = TRUE)
  attr(tmp_rs, "estimator") <- "632+"

  tmp_boot <-
    lr_spline_wflow %>%
    fit_resamples(
      resamples = tmp_rs,
      metrics = cls_met
    )

  tmp_res <-
    bind_rows(
      collect_metrics(tmp_boot, estimator = "standard"),
      collect_metrics(tmp_boot, estimator = "632"),
      collect_metrics(tmp_boot, estimator = "632+")
    ) %>%
    mutate(times = iters, seed = 998, model = "logistic")

  tmp_rand <-
    tmp_boot %>%
    select(.metrics) %>%
    unnest(.metrics) %>%
    filter(.estimator == "randomized") %>%
    summarize(randomized = mean(.estimate), .by = c(.metric)) %>%
    mutate(times = iters, seed = 998, model = "knn")

  rs_boot <- bind_rows(rs_boot, tmp_res)
  rs_boot_permute <- bind_rows(rs_boot_permute, tmp_rand)
}

save(rs_boot, file = file.path(glue("boot_logistic_{chr_seed}.RData")))

# ------------------------------------------------------------------------------
# Monte-Carlo CV

retain_pct <- c(0.5, 0.6, 0.7, 0.8, 0.9)

rs_mc_cv <- NULL
for (iters in B) {
  for (pct in retain_pct) {

    tmp_rs <- mc_cv(sim_tr, times = iters, prop = pct, strata = class)

    tmp_res <-
      lr_spline_wflow %>%
      fit_resamples(
        resamples = tmp_rs,
        metrics = cls_met
      ) %>%
      collect_metrics()%>%
      mutate(retain = pct, times = iters, estimator = "standard",
             seed = 998, model = "logistic")

    rs_mc_cv <- bind_rows(rs_mc_cv, tmp_res)

  }
}

save(rs_mc_cv, file = file.path(glue("mc_cv_logistic_{chr_seed}.RData")))

# ------------------------------------------------------------------------------
# Validation Set

rs_val <- NULL
for (pct in retain_pct) {

  tmp_rs <- validation_split(sim_tr, prop = pct, strata = class)

  tmp_res <-
    lr_spline_wflow %>%
    fit_resamples(
      resamples = tmp_rs,
      metrics = cls_met
    ) %>%
    collect_metrics()%>%
    mutate(retain = pct, estimator = "standard", seed = 998, model = "logistic")

  rs_val <- bind_rows(rs_val, tmp_res)

}

save(rs_val, file = file.path(glue("val_logistic_{chr_seed}.RData")))

# ------------------------------------------------------------------------------

tmp_rs <- apparent(sim_tr)

resub <-
  lr_spline_wflow %>%
  fit_resamples(
    resamples = tmp_rs,
    metrics = cls_met
  ) %>%
  pluck(".metrics") %>%
  pluck(1) %>%
  mutate(estimator = "standard", seed = 998, model = "logistic")

save(resub, file = file.path(glue("resub_logistic_{chr_seed}.RData")))

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if(!interactive()) {
  q("no")
}
