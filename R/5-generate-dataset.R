generate_dataset <- function(unique_id, ti_type = "linear", num_cells = 99, num_genes = 101, noise_std = 0.05, use_tented_progressions = TRUE) {
  # generate milestone network
  milestone_network <- generate_toy_milestone_network(ti_type)

  # get milestone ids
  milestone_ids <- sort(unique(c(milestone_network$from, milestone_network$to)))

  # generate tented progressions
  if (use_tented_progressions) {
    progressions <- random_progressions_tented(milestone_network, ncells = num_cells)
  } else {
    progressions <- random_progressions(milestone_network, ncells = num_cells)
  }

  # get cell ids
  cell_ids <- unique(progressions$cell_id)

  # convert to percentages
  milestone_percentages <- dynutils::convert_progressions_to_milestone_percentages(cell_ids, milestone_ids, milestone_network, progressions)

  # generate expression
  expression <- generate_expression(milestone_network, progressions, ngenes = num_genes, noise_std = noise_std)

  # simulate counts
  counts <- generate_counts(expression)

  # collect prior informationn
  end_milestones <- milestone_ids[!(milestone_ids %in% milestone_network$from)]
  special_cells <- list(
    start_cell_id = progressions %>% arrange(from, to, percentage) %>% pull(cell_id) %>% first,
    end_cell_ids = progressions %>% filter(to %in% end_milestones) %>% group_by(to) %>% arrange(percentage) %>% summarise(cell_id=cell_id[which.max(percentage)]) %>% pull(cell_id)
  )

  cell_grouping <- dynutils::get_cell_grouping(milestone_percentages)

  # make a simple sample info
  sample_info <- data_frame(id = rownames(counts))
  feature_info <- data_frame(id = colnames(counts))

  # wrap dataset
  dataset <- dynutils::wrap_ti_task_data(
    ti_type = ti_type,
    id = unique_id,
    counts = counts,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    progressions = progressions,
    sample_info = sample_info,
    feature_info = feature_info,
    special_cells = special_cells,
    cell_grouping = cell_grouping
  )
  dataset$type <- "ti_toy"

  # add geodesic dist
  dataset$geodesic_dist <- dynutils::compute_emlike_dist(dataset)

  # return dataset
  dataset
}
