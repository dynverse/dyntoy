library(dyntoy)
library(dynutils)
library(dynwrap)

set.seed(3)

toy_tasks <- generate_toy_datasets(num_replicates = 10)


pdf(paste0("~/dyntoy.pdf"), 15, 10)
for (model in unique(toy_tasks$model)) {
  tasks <- toy_tasks %>% filter(model == !!model)

  print(cowplot::plot_grid(plotlist = map(seq_len(nrow(tasks)), function(i) {
    task <- tasks %>% extract_row_to_list(i)
    dynplot::plot_default(task, color_cells = "milestone") + ggplot2::ggtitle(task$id, paste0(task$trajectory_type, " - cells=", length(task$cell_ids), " - genes=", ncol(task$expression)))
  })))
}
dev.off()


devtools::use_data(toy_tasks, overwrite = TRUE)
