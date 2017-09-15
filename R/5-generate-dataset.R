generate_dataset <- function(ti_type="linear", num_cells=99, num_genes=101) {
  dataset <- list()

  dataset$milestone_network <- generate_toy_milestone_network(ti_type)
  dataset$milestone_ids <- unique(c(dataset$milestone_network$from, dataset$milestone_network$to))
  dataset$progressions <- random_progressions_tented(dataset$milestone_network, ncells = num_cells)
  dataset$cell_ids <- unique(dataset$progressions$cell_id)
  dataset$milestone_percentages <- dynutils::convert_progressions_to_milestone_percentages(dataset$cell_ids, dataset$milestone_ids, dataset$milestone_network, dataset$progressions)
  dataset$expression <- generate_expression(dataset$milestone_network, dataset$progressions, ngenes = num_genes)
  dataset$counts <- generate_counts(dataset$expression)

  dataset$type <- "ti_toy"

  # Prior information
  end_milestones <- dataset$milestone_ids[!(dataset$milestone_ids %in% dataset$milestone_network$from)]
  dataset$special_cells <- list(
    start_cell_id = dataset$progressions %>% arrange(from, to, percentage) %>% pull(cell_id) %>% first,
    end_cell_ids = dataset$progressions %>% filter(to %in% end_milestones) %>% group_by(to) %>% arrange(percentage) %>% summarise(cell_id=cell_id[which.max(percentage)]) %>% pull(cell_id)
  )

  dataset$cell_grouping <- dynutils::get_cell_grouping(dataset$milestone_percentages)

  dataset$geodesic_dist <- dynutils::compute_emlike_dist(dataset)

  dataset$id <- "toy"

  class(dataset) <- c("dyneval::ti_wrapper", "list")

  dataset
}
