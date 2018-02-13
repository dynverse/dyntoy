library(dyntoy)

toy_tasks <- generate_toy_datasets(num_replicates = 3)

devtools::use_data(toy_tasks, overwrite = TRUE)
