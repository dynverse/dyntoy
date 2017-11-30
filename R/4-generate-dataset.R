generate_dataset <- function(unique_id, trajectory_type = "linear", num_cells = 99, num_genes = 101, noise_nbinom_size = 20, use_tented_progressions = TRUE, expression_randomizer="modules") {
  # generate milestone network
  milestone_network <- generate_toy_milestone_network(trajectory_type)

  # get milestone ids
  milestone_ids <- sort(unique(c(milestone_network$from, milestone_network$to)))

  # generate (tented) progressions
  if (use_tented_progressions) {
    progressions <- random_progressions_tented(milestone_network, ncells = num_cells)
  } else {
    progressions <- random_progressions(milestone_network, ncells = num_cells)
  }

  # get cell ids
  cell_ids <- unique(progressions$cell_id)

  # generate expression
  expression <- generate_expression(milestone_network, progressions, ngenes = num_genes, expression_randomizer=expression_randomizer)

  # simulate counts
  original_counts <- generate_counts(expression, noise_nbinom_size=noise_nbinom_size)

  # normalize
  normalized <- dynutils::normalise_filter_counts(original_counts, filter_hvg=FALSE, nmads = 10)
  counts <- normalized$counts
  expression <- normalized$expression
  cell_ids <- rownames(counts)

  progressions <- progressions %>% filter(cell_id %in% cell_ids)

  # make a simple sample info
  cell_info <- tibble(cell_id = cell_ids)
  feature_info <- tibble(feature_id = colnames(counts), housekeeping=FALSE)

  # wrap dataset
  dataset <- dynutils::wrap_ti_task_data(
    trajectory_type = trajectory_type,
    ti_type = trajectory_type,
    id = unique_id,
    counts = counts,
    expression = expression,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    progressions = progressions,
    cell_info = cell_info,
    feature_info = feature_info
  )
  dataset$type <- "ti_toy"

  # add prior information
  dataset$prior_information <- with(dataset, dynutils::generate_prior_information(milestone_ids, milestone_network, progressions, milestone_percentages, counts, feature_info, cell_info))

  # add geodesic dist
  dataset$geodesic_dist <- dynutils::compute_emlike_dist(dataset)

  # return dataset
  dataset
}
