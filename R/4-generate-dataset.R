#' @importFrom dynwrap add_prior_information
generate_dataset <- function(
  unique_id,
  model = "linear",
  num_cells = 99,
  num_genes = 101,
  noise_nbinom_size = 20,
  allow_tented_progressions = TRUE,
  normalise = dynutils::check_packages("dynnormaliser")
) {
  # generate milestone network
  milestone_network <- generate_toy_milestone_network(model)

  # get milestone ids
  milestone_ids <- sort(unique(c(milestone_network$from, milestone_network$to)))

  # generate (tented) progressions
  progressions <- random_progressions(milestone_network, ncells = num_cells, allow_tented = allow_tented_progressions)

  # were any divergences created?
  divreg <- progressions %>% group_by(cell_id) %>% filter(n() > 1) %>% ungroup()
  if (nrow(divreg) > 0) {
    froms <- unique(divreg$from)
    divergence_regions <- froms %>% map_df(function(fr) {
      data_frame(
        divergence_id = paste0("divergence_", fr),
        milestone_id = unique(c(fr, divreg %>% filter(from == fr) %>% .$to)),
        is_start = milestone_id == fr
      )
    })
  } else {
    divergence_regions <- NULL
  }

  # get cell ids
  cell_ids <- unique(progressions$cell_id)

  # generate expression
  expression <- generate_expression(
    milestone_network = milestone_network,
    progressions = progressions,
    ngenes = num_genes
  )

  # simulate counts
  original_counts <- generate_counts(
    expression = expression,
    noise_nbinom_size = noise_nbinom_size
  )

  # normalize
  if (normalise) {
    normalised <- dynnormaliser::normalise_filter_counts(
      original_counts,
      filter_hvg = FALSE,
      nmads = 10
    )
    counts <- normalised$counts
    expression <- normalised$expression
    cell_ids <- intersect(rownames(counts), cell_ids)
    progressions <- progressions %>% filter(cell_id %in% cell_ids)
  } else {
    counts <- original_counts
    expression = expression
  }

  # make a simple sample info
  cell_info <- tibble(cell_id = cell_ids)
  feature_info <- tibble(feature_id = colnames(counts), housekeeping = FALSE)

  # wrap dataset
  wrap_data(
    id = unique_id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    task_source = "toy",
    model = model
  ) %>% add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = divergence_regions,
    progressions = progressions
  ) %>% add_cell_waypoints(
    num_cells_selected = 25
  ) %>% add_expression(
    counts = counts,
    expression = expression,
    feature_info = feature_info
  ) %>% dynwrap::add_prior_information()
}
