library(dyntoy)
library(dynutils)
library(dynwrap)
library(tidyverse)

set.seed(3)

toy_datasets <- generate_toy_datasets(
  num_replicates = 5
  # dropout_probability_factor = 0,
  # sample_dispersion_count = function(x) Inf
)

pdf(paste0("~/dyntoy.pdf"), 15, 10)
for (model in unique(toy_datasets$model)) {
  datasets <- toy_datasets %>% filter(model == !!model)

  print(cowplot::plot_grid(plotlist = map(seq_len(nrow(datasets)), function(i) {
    dataset <- datasets %>% extract_row_to_list(i)
    dynplot::plot_default(dataset, color_cells = "milestone") + ggplot2::ggtitle(dataset$id, paste0(dataset$trajectory_type, " - cells = ", length(dataset$cell_ids), " - genes = ", ncol(dataset$expression)))
  })))
}
dev.off()


devtools::use_data(toy_datasets, overwrite = TRUE)
