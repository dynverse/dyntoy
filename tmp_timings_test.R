library(tidyverse)
library(dyntoy)

num_cells <- 1000
num_features <- 200

tests <- crossing(num_cells = 10^seq(log10(10), log10(10000), length.out = 11), num_features = 100)

timings <- pmap_df(tests, function(num_cells, num_features) {
  cat("num cells: ", num_cells, ", num_features: ", num_features, "\n", sep = "")
  ds <- generate_toy_datasets(models = "diverging", num_replicates = 1, num_cells = num_cells, num_features = num_features)
  tim <- ds$timings[[1]]
  times <- map2_dbl(tim %>% tail(-1), tim %>% head(-1) %>% set_names(NULL), function(a, b) as.numeric(difftime(a, b), units = "secs"))
  data_frame(num_cells, num_features, step = names(times), time = times)
})

ggplot(timings) + geom_line(aes(num_cells, time, colour = step)) + scale_x_log10() + scale_y_log10()
