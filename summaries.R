
library(tidymodels)
library(patchwork)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

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

large_stats <- median(large_res$brier)

# ------------------------------------------------------------------------------

boot_files <- list.files("files", pattern = "^boot_", full.names = TRUE)
boot_res <- map_dfr(boot_files, get_res)

boot_est <- 
  boot_res %>% 
  summarize(
    estimate = mean(.estimate),
    bias = mean( (.estimate - large_est) / large_est),
    n = sum(!is.na(.estimate)),
    std_err = sd(.estimate) / n,
    large_est = mean(large_est),
    .by = c(seed, replicate, times)
  ) 

boot_stats <- 
  boot_est %>% 
  summarize(
    estimate = mean(estimate),
    bias = mean(bias),
    std_err = mean(std_err),
    large_est = mean(large_est),
    .by = c(times)
  ) %>% 
  mutate(estimator = "average")

boot_stats %>% ggplot(aes(times, bias)) + geom_point()
boot_stats %>% ggplot(aes(times, std_err)) + geom_point()

# ------------------------------------------------------------------------------

resub_files <- list.files("files", pattern = "^resub_", full.names = TRUE)
resub_res <- 
  map_dfr(resub_files, get_res, nm = "resub") %>% 
  select(resub = .estimate, seed, large_est)

# ------------------------------------------------------------------------------

c1 <- 1 - exp(-1)
c2 <- 1 - c1

boot_632_stats <- 
  boot_est %>% 
  select(seed, replicate, times, estimate, large_est) %>% 
  full_join(resub_res %>% select(-large_est), by = "seed") %>% 
  mutate(
    mean = estimate,
    estimate = c1 * mean + c2 * resub,
    bias = mean( (estimate - large_est) / large_est),
  ) %>% 
  summarize(
    estimate = mean(estimate),
    bias = mean(bias),
    large_est = mean(large_est),
    .by = c(times)
  ) %>% 
  mutate(std_err = NA_real_, estimator = "632")

boot_632_plus_stats <- 
  boot_res %>% 
  full_join(resub_res %>% select(-large_est), by = "seed") %>% 
  summarize(
    basic = mean(.estimate),
    no_info = mean(no_info),
    resub = mean(resub),
    large_est = mean(large_est),
    .by = c(seed, replicate, times)
  ) %>% 
  mutate(
    ror = (basic - resub) / (no_info - resub),
    ror = ifelse(ror < 0, 0, ror),
    wt = c1 / (1 - c2 * ror),
    estimate = wt * basic + (1 - wt) * resub,
    bias = mean( (estimate - large_est) / large_est),
  ) %>% 
  summarize(
    estimate = mean(estimate),
    basic = mean(basic),
    resub = mean(resub),
    no_info = mean(no_info),
    bias = mean(bias),
    wt = mean(wt),
    ror = mean(ror),
    large_est = mean(large_est),
    .by = c(times)
  ) %>% 
  mutate(std_err = NA_real_, estimator = "632+")

boot_stats <- 
  bind_rows(boot_stats, boot_632_stats, boot_632_plus_stats) %>% 
  mutate(estimator = factor(estimator, levels = c("average", "632", "632+")))
rm(boot_632_stats, boot_632_plus_stats)

# ------------------------------------------------------------------------------

mc_files <- list.files("files", pattern = "^mc_cv_", full.names = TRUE)
mc_res <- map_dfr(mc_files, get_res)

mc_stats <- 
  mc_res %>% 
  summarize(
    mean = mean(.estimate),
    bias = mean( (.estimate - large_est) / large_est),
    n = sum(!is.na(.estimate)),
    std_err = sd(.estimate) / n,
    .by = c(seed, replicate, times, retain)
  ) %>% 
  summarize(
    bias = mean(bias),
    std_err = mean(std_err),
    .by = c(times, retain)
  ) %>% 
  mutate(retain = format(retain))

# ------------------------------------------------------------------------------

vfold_files <- list.files("files", pattern = "^v_fold_", full.names = TRUE)
vfold_res <- map_dfr(vfold_files, get_res)

vfold_stats <- 
  vfold_res %>% 
  summarize(
    mean = mean(.estimate),
    bias = mean( (.estimate - large_est) / large_est),
    n = sum(!is.na(.estimate)),
    std_err = sd(.estimate) / n,
    .by = c(seed, replicate, folds, repeats)
  ) %>% 
  summarize(
    bias = mean(bias),
    std_err = mean(std_err),
    .by = c(repeats, folds)
  ) %>% 
  mutate(
    times = folds * repeats,
    folds = format(folds)
  )

# ------------------------------------------------------------------------------

bias_vals <- c(vfold_stats$bias,    boot_stats$bias,    mc_stats$bias)
bias_rng <- extendrange(bias_vals)

prec_vals <- c(vfold_stats$std_err, boot_stats$std_err, mc_stats$std_err)
prec_rng <- extendrange(prec_vals)
prec_rng[1] <- 0

# ------------------------------------------------------------------------------

save(list = ls(pattern = "(_stats$)|(_rng$)"), file = "resample_sim_res.RData")

# ------------------------------------------------------------------------------

mc_bias <- 
  mc_stats %>% 
  ggplot(aes(times, bias, col = retain, pch = retain)) + 
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(labels = scales::percent, limits = bias_rng) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Bias", x = "Number of Resamples")

mc_prec <- 
  mc_stats %>% 
  ggplot(aes(times, std_err, col = retain, pch = retain)) + 
  geom_point() +
  geom_line() +
  lims(y = prec_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples") +
  scale_color_brewer(palette = "Set1")

mc_bias + mc_prec + plot_layout(guides = 'collect') & theme(legend.position = "top")

vfold_bias <- 
  vfold_stats %>% 
  ggplot(aes(times, bias, col = folds, pch = folds)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = bias_rng)  +
  scale_color_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Bias", x = "Number of Resamples")

vfold_prec <- 
  vfold_stats %>% 
  ggplot(aes(times, std_err, col = folds, pch = folds)) + 
  geom_point() +
  geom_line() +
  lims(y = prec_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples")  +
  scale_color_brewer(palette = "Dark2")

vfold_bias + vfold_prec + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "top")

boot_bias <- 
  boot_stats %>% 
  ggplot(aes(times, bias, col = estimator, pch = estimator)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = bias_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Bias", x = "Number of Resamples") 

boot_prec <- 
  boot_stats %>% 
  filter(estimator == "average") %>% 
  ggplot(aes(times, std_err, col = estimator, pch = estimator)) + 
  geom_point() +
  geom_line() +
  lims(y = prec_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples") 

boot_bias + boot_prec + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "top")



