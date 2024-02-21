
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
  res <- try(get(nm), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(NULL)
  }
  res
}

# ------------------------------------------------------------------------------

large_files <- list.files("files", pattern = "^holdout_", full.names = TRUE)
large_res <- map_dfr(large_files, get_res, nm = "big_res")

boot_files <- list.files("files", pattern = "^boot_", full.names = TRUE)
boot_res <- map_dfr(boot_files, get_res, "rs_boot")
perm_res <- map_dfr(boot_files, get_res, "rs_boot_permute")

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
    resub = median(.estimate),
    .by = c(model, .metric)
  )

# ------------------------------------------------------------------------------
# Compute the no information rate estimates for each model

stats_perm <-
  perm_res %>%
  summarize(
    permuted = median(randomized),
    .by = c(model, .metric)
  )
# temp until fix
stats_perm <- bind_rows(stats_perm, stats_perm %>% mutate(model = "logistic"))


# ------------------------------------------------------------------------------
# MC CV

stats_mc_cv <-
  mc_res %>%
  full_join(stats_large_sample, by = c("model", ".metric")) %>%
  full_join(stats_perm, by = c("model", ".metric")) %>%
  full_join(stats_resub, by = c("model", ".metric")) %>%
  full_join(cls_info, by = c(".metric")) %>%
  mutate(bias = (mean - large_sample) / large_sample * multiplier) %>%
  summarize(
    bias = mean(bias),
    std_err = mean(std_err),
    permuted = mean(permuted),
    large_sample = mean(large_sample),
    resub = mean(resub),
    .by = c(model, .metric, repeats, folds)
  ) %>%
  mutate(retain = format(retain))

# ------------------------------------------------------------------------------
# V-fold

stats_v_fold_cv <-
  vfold_res %>%
  full_join(stats_large_sample, by = c("model", ".metric")) %>%
  full_join(stats_perm, by = c("model", ".metric")) %>%
  full_join(stats_resub, by = c("model", ".metric")) %>%
  full_join(cls_info, by = c(".metric")) %>%
  mutate(bias = (mean - large_sample) / large_sample * multiplier) %>%
  summarize(
    bias = mean(bias),
    std_err = mean(std_err),
    permuted = mean(permuted),
    large_sample = mean(large_sample),
    resub = mean(resub),
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
  full_join(stats_perm, by = c("model", ".metric")) %>%
  full_join(stats_resub, by = c("model", ".metric")) %>%
  full_join(cls_info, by = c(".metric")) %>%
  mutate(
    bias = (mean - large_sample) / large_sample * multiplier,
    .estimator = ifelse(.estimator == "binary", "standard", .estimator)
    ) %>%
  summarize(
    estimate = mean(mean),
    bias = mean(bias),
    std_err = mean(std_err),
    permuted = mean(permuted),
    large_sample = mean(large_sample),
    resub = mean(resub),
    .by = c(model, .metric, .estimator, times)
  )

c_632 <-  1 - exp(-1)
c_368 <- exp(-1)

stats_bootstraps %>%
  filter(.metric %in% c("accuracy", "brier_class") & times == 100 &
           .estimator == "standard") %>%
  select(model, .metric, mean = estimate,
         `no information rate` = permuted, truth = large_sample,
         resubstitution = resub) %>%
  mutate(
    ror = (mean - resubstitution) / (`no information rate` - resubstitution),
    ror = ifelse(ror < 0, 0, ror),
    weights = c_632 / (1 - c_368 * ror),
    `632`  =     c_632 * mean +    c_368 * resubstitution,
    `632+` =    weights * mean + (1 - weights) * resubstitution
  )  %>%
  rename(`simple mean` = mean, `relative overfitting rate` = ror) %>%
  pivot_longer(
    cols = c(-model,-.metric),
    names_to = "statistic",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = c(statistic),
    names_from = c(.metric, model),
    values_from = value
  ) %>%
  filter(statistic != "truth") %>%
  mutate(
    type = ifelse(grepl("632", statistic), "final estimates", "estimates"),
    type = ifelse(statistic %in% c("relative overfitting rate", "weights", "no information rate"),
                  "intermediates", type)
  ) %>%
  gt(groupname_col = "type") %>%
  # tab_spanner(
  #   label = "1 Nearest Neighbor",
  #   columns = c(accuracy_knn, brier_class_knn)
  # ) %>%
  # tab_spanner(
  #   label = "Logistic Reg",
  #   columns = c(accuracy_logistic, brier_class_logistic)
  # ) %>%
  # cols_label(
  #   accuracy_knn = "Accuracy",
  #   brier_class_knn = "Brier",
  #   accuracy_logistic = "Accuracy",
  #   brier_class_logistic = "Brier"
  # ) %>%
tab_spanner(
  label = "Accuracy",
  columns = c(accuracy_knn, accuracy_logistic)
) %>%
  tab_spanner(
    label = "Brier Score",
    columns = c(brier_class_knn, brier_class_logistic)
  ) %>%
  cols_label(
    statistic = " ",
    accuracy_knn = "1 NN",
    brier_class_knn = "1 NN",
    accuracy_logistic = "Logistic",
    brier_class_logistic = "Logistic"
  ) %>%
  fmt_number(columns = c(-statistic), decimals = 3)




# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# plots

sel_model <- "knn"
sel_metric <- "roc_auc"

metric_data <-
  bind_rows(
    stats_bootstraps %>% filter(.metric == sel_metric),
    stats_mc_cv %>% filter(.metric == sel_metric),
    stats_v_fold_cv %>% filter(.metric == sel_metric)
  )


rng_bias <- range(metric_data$bias)
rng_std_err <- range(metric_data$std_err, na.rm = TRUE)
rng_std_err[1] <- 0

###

vfold_bias <-
  stats_v_fold_cv %>%
  filter(model == sel_model & .metric == sel_metric) %>%
  ggplot(aes(times, bias, col = folds, pch = folds)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = rng_bias)  + #
  scale_color_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Bias", x = "Number of Resamples")

vfold_prec <-
  stats_v_fold_cv %>%
  filter(model == sel_model & .metric == sel_metric) %>%
  ggplot(aes(times, std_err, col = folds, pch = folds)) +
  geom_point() +
  geom_line() +
  lims(y = rng_std_err) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples")  +
  scale_color_brewer(palette = "Dark2")

vfold_bias + vfold_prec +
  plot_annotation(paste(sel_model, sel_metric)) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "top")

###

mc_bias <-
  stats_mc_cv %>%
  filter(model == sel_model & .metric == sel_metric) %>%
  ggplot(aes(times, bias, col = retain, pch = retain)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(labels = scales::percent, limits = rng_bias) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Bias", x = "Number of Resamples")

mc_prec <-
  stats_mc_cv %>%
  filter(model == sel_model & .metric == sel_metric) %>%
  ggplot(aes(times, std_err, col = retain, pch = retain)) +
  geom_point() +
  geom_line() +
  lims(y = rng_std_err) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples") +
  scale_color_brewer(palette = "Set1")

mc_bias + mc_prec +
  plot_annotation(paste(sel_model, sel_metric)) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "top")

###

boot_bias <-
  stats_bootstraps %>%
  filter(model == sel_model & .metric == sel_metric) %>%
  rename(estimator = .estimator) %>%
  ggplot(aes(times, bias, col = estimator, pch = estimator)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = rng_bias) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Bias", x = "Number of Resamples")

boot_prec <-
  stats_bootstraps %>%
  filter(model == sel_model & .metric == sel_metric) %>%
  filter(.estimator == "standard") %>%
  rename(estimator = .estimator) %>%
  ggplot(aes(times, std_err, col = estimator, pch = estimator)) +
  geom_point() +
  geom_line(show.legend = FALSE) +
  lims(y = rng_std_err) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples")

boot_bias + boot_prec +
  plot_annotation(paste(sel_model, sel_metric)) +
  plot_layout(guides = 'collect') &
  theme(legend.position = "top")
