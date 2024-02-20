
library(tidymodels)
library(patchwork)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)


# ------------------------------------------------------------------------------

cls_met <- metric_set(brier_class, roc_auc, accuracy)
cls_info <-
  as_tibble(cls_met) %>%
  mutate(multiplier = ifelse(direction == "minimize", 1, -1)) %>%
  select(.metric = metric, multiplier)

# ------------------------------------------------------------------------------

get_res <- function(x, nm = NULL) {
  load(x)
  if (is.null(nm)) {
    nm <- ls(pattern = "^rs_")
  }
  get(nm)
}

# ------------------------------------------------------------------------------

large_files <- list.files("files", pattern = "^holdout_", full.names = TRUE)
large_res <- map_dfr(large_files, get_res, nm = "big_res")

boot_files <- list.files("files", pattern = "^boot_", full.names = TRUE)
boot_res <- map_dfr(boot_files, get_res)

resub_files <- list.files("files", pattern = "^resub_", full.names = TRUE)
resub_res <- map_dfr(resub_files, get_res, nm = "resub")

mc_files <- list.files("files", pattern = "^mc_cv_", full.names = TRUE)
mc_res <- map_dfr(mc_files, get_res)

vfold_files <- list.files("files", pattern = "^v_fold_", full.names = TRUE)
vfold_res <- map_dfr(vfold_files, get_res)

# ------------------------------------------------------------------------------
# Compute the large sample estimates for each model

stats_large_sample <-
  large_res %>%
  summarize(
    large_sample = median(.estimate),
    .by = c(model, .metric)
  )

# ------------------------------------------------------------------------------
# Compute the resubstitution estimates for each model

stats_resub <-
  resub_res %>%
  summarize(
    large_sample = median(.estimate),
    .by = c(model, .metric)
  )

# ------------------------------------------------------------------------------
# MC CV

stats_mc <-
  mc_res %>%
  full_join(stats_large_sample, by = c("model", ".metric")) %>%
  full_join(cls_info, by = c(".metric")) %>%
  summarize(
    mean = mean(mean),
    std_err = mean(std_err),
    bias = mean( (mean - large_sample) / large_sample * multiplier),
    n = sum(!is.na(mean)),
    .by = c(model, .metric, seed, times, retain)
  ) %>%
  summarize(
    bias = mean(bias),
    std_err = mean(std_err),
    .by = c(model, .metric, times, retain)
  ) %>%
  mutate(retain = format(retain))

# ------------------------------------------------------------------------------
# V-fold

stats_v_fold_cv <-
  vfold_res %>%
  full_join(stats_large_sample, by = c("model", ".metric")) %>%
  full_join(cls_info, by = c(".metric")) %>%
  summarize(
    mean = mean(mean),
    std_err = mean(std_err),
    bias = mean( (mean - large_sample) / large_sample * multiplier),
    n = sum(!is.na(mean)),
    .by = c(model, .metric, seed, repeats, folds)
  ) %>%
  summarize(
    bias = mean(bias),
    std_err = mean(std_err),
    .by = c(model, .metric, repeats, folds)
  ) %>%
  mutate(
    times = folds * repeats,
    repeats = format(repeats),
    folds = format(folds)
  )

# ------------------------------------------------------------------------------
# Bootstraps

stats_bootstraps <-
  boot_res %>%
  full_join(stats_large_sample, by = c("model", ".metric")) %>%
  full_join(cls_info, by = c(".metric")) %>%
  summarize(
    mean = mean(mean),
    std_err = mean(std_err),
    bias = mean( (mean - large_sample) / large_sample * multiplier),
    n = sum(!is.na(mean)),
    .by = c(model, .metric, .estimator, seed, times)
  ) %>%
  summarize(
    bias = mean(bias),
    std_err = mean(std_err),
    .by = c(model, .metric, .estimator, times)
  )

# ------------------------------------------------------------------------------
# compare

boot_res %>%
  filter(model == "knn" & .estimator == "binary" & times == 100 & seed == 22)
stats_large_sample %>% filter(model == "knn")
stats_resub %>% filter(model == "knn")

# ------------------------------------------------------------------------------

# bias_vals <- c(vfold_stats$bias,    boot_stats$bias,    mc_stats$bias)
# bias_rng <- extendrange(bias_vals)
#
# prec_vals <- c(vfold_stats$std_err, boot_stats$std_err, mc_stats$std_err)
# prec_rng <- extendrange(prec_vals)
# prec_rng[1] <- 0

# ------------------------------------------------------------------------------

# save(list = ls(pattern = "(_stats$)|(_rng$)"), file = "resample_sim_res.RData")

# ------------------------------------------------------------------------------



