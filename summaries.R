
library(tidymodels)
library(patchwork)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

get_res <- function(x) {
  load(x)
  nm <- ls(pattern = "^rs_")
  get(nm)
}

# ------------------------------------------------------------------------------

# also maybe pool all of the holdouts to get a grand large sample estimate

boot_files <- list.files("files", pattern = "^boot_", full.names = TRUE)
boot_res <- map_dfr(boot_files, get_res)

boot_stats <- 
  boot_res %>% 
  summarize(
    mean = mean(.estimate),
    bias = mean( (.estimate - large_est) / large_est),
    n = sum(!is.na(.estimate)),
    std_err = sd(.estimate) / n,
    .by = c(seed, replicate, times)
  ) %>% 
  summarize(
    bias = mean(bias),
    std_err = mean(std_err),
    .by = c(times)
  )

boot_stats %>% ggplot(aes(times, bias)) + geom_point()
boot_stats %>% ggplot(aes(times, std_err)) + geom_point()

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
  ggplot(aes(times, bias, col = retain)) + 
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, lty = 2) +
  scale_y_continuous(labels = scales::percent, limits = bias_rng) +
  labs(y = "Bias", x = "Number of Resamples")

mc_prec <- 
  mc_stats %>% 
  ggplot(aes(times, std_err, col = retain)) + 
  geom_point() +
  geom_line() +
  lims(y = prec_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples")

mc_bias + mc_prec + plot_layout(guides = 'collect') & theme(legend.position = "top")

vfold_bias <- 
  vfold_stats %>% 
  ggplot(aes(times, bias, col = folds)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = bias_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Bias", x = "Number of Resamples")

vfold_prec <- 
  vfold_stats %>% 
  ggplot(aes(times, std_err, col = folds)) + 
  geom_point() +
  geom_line() +
  lims(y = prec_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples")

vfold_bias + vfold_prec + plot_layout(guides = 'collect') & theme(legend.position = "top")

boot_bias <- 
  boot_stats %>% 
  ggplot(aes(times, bias)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = bias_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Bias", x = "Number of Resamples")

boot_prec <- 
  boot_stats %>% 
  ggplot(aes(times, std_err)) + 
  geom_point() +
  geom_line() +
  lims(y = prec_rng) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(y = "Std. Error", x = "Number of Resamples")

boot_bias + boot_prec


