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
  counts <- generate_counts(expression, noise_nbinom_size=noise_nbinom_size)

  # make a simple sample info
  sample_info <- data_frame(id = rownames(counts))
  feature_info <- data_frame(id = colnames(counts))

  # wrap dataset
  dataset <- dynutils::wrap_ti_task_data(
    trajectory_type = trajectory_type,
    ti_type = trajectory_type,
    id = unique_id,
    counts = counts,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    progressions = progressions,
    sample_info = sample_info,
    feature_info = feature_info
  )
  dataset$type <- "ti_toy"

  # add prior information
  dataset$prior_information <- with(dataset, dynutils::generate_prior_information(milestone_ids, milestone_network, progressions, milestone_percentages))

  # add geodesic dist
  dataset$geodesic_dist <- dynutils::compute_emlike_dist(dataset)

  # return dataset
  dataset
}
