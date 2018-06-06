library(dyntoy)
library(dynutils)
library(dynwrap)

set.seed(1)

toy_tasks <- generate_toy_datasets(num_replicates = 10)

# fix toys
# toy_tasks <- map(seq_len(nrow(toy_tasks)), function(i) {
#   task <- toy_tasks %>% extract_row_to_list(i) %>% dynwrap::add_cell_waypoints(25)
# }) %>% list_as_tibble()

devtools::use_data(toy_tasks, overwrite = TRUE)
