library(tidymodels)
library(glue)
library(stringr)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

template <- readLines("template.R")

# ------------------------------------------------------------------------------

num_sim <- 75

set.seed(2)

combinations <- 
  tibble(seed = sample.int(1000, num_sim)) %>% 
  mutate(
    chr_seed = format(1:1000)[seed],
    chr_seed = gsub(" ", "0", chr_seed),
    file = glue("files/sim_{seed}.R")
  )

new_file <- function(x, template) {
  template <- gsub("SEED", x$seed, template)   
  cat(template, sep = "\n", file = x$file)
  invisible(NULL)
}

for (i in 1:nrow(combinations)) {
  new_file(combinations[i,], template)
}

# ------------------------------------------------------------------------------

r_files <- list.files("files", pattern = "*.R$")
cmds <- paste("R CMD BATCH --vanilla", r_files)

cat("#!/bin/sh\n", file = "files/run.sh")
cat(cmds, sep = "\n", file = "files/run.sh", append = TRUE)

# ------------------------------------------------------------------------------


