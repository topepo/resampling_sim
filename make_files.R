library(tidymodels)
library(glue)
library(stringr)

# pak::pak(c("tidymodels/tune@no-information-rate"), ask = FALSE)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

new_file <- function(x, template) {
  template <- gsub("SEED", x$seed, template)
  cat(template, sep = "\n", file = x$file)
  invisible(NULL)
}

# ------------------------------------------------------------------------------

num_sim <- 100

# ------------------------------------------------------------------------------

lr_template <- readLines("new_logistic_template.R")
model <- "logistic"

set.seed(1)
combinations <-
  tibble(seed = sample.int(1000, num_sim)) %>%
  mutate(
    chr_seed = format(1:1000)[seed],
    chr_seed = gsub(" ", "0", chr_seed),
    file = glue("files/sim_{model}_{seed}.R")
  )

for (i in 1:nrow(combinations)) {
  new_file(combinations[i,], lr_template)
}

# ------------------------------------------------------------------------------

knn_template <- readLines("new_knn_template.R")
model <- "knn"

set.seed(1)
combinations <-
  tibble(seed = sample.int(1000, num_sim)) %>%
  mutate(
    chr_seed = format(1:1000)[seed],
    chr_seed = gsub(" ", "0", chr_seed),
    file = glue("files/sim_{model}_{seed}.R")
  )

for (i in 1:nrow(combinations)) {
  new_file(combinations[i,], knn_template)
}

# ------------------------------------------------------------------------------

r_files <- sample(list.files("files", pattern = "*.R$"))
cmds <- paste("R CMD BATCH --vanilla", r_files)

cat("#!/bin/sh\n", file = "files/run.sh")
cat(cmds, sep = "\n", file = "files/run.sh", append = TRUE)

# ------------------------------------------------------------------------------

if (FALSE) {
  library(fs)
  file_info <- fs::dir_info("files")
  num_r <- sum(grepl("\\.R$", file_info$path))
  difftime(max(file_info$modification_time), min(file_info$modification_time)) / 4
}
