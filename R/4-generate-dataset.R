#' @rdname generate_dataset
#' @export
generate_trajectory <- function(
  unique_id = "",
  model = "linear",
  num_cells = 99,
  allow_tented_progressions = TRUE
) {
  # generate milestone network
  if (is.character(model)) {
    milestone_network <- generate_milestone_network(model = model)
  } else if (is.function(model)) {
    milestone_network <- model()
  } else if (is.data.frame(model)) {
    milestone_network <- model
  } else {
    stop("Unrecognised format for 'model'.")
  }

  # add columns if necessary
  if (!"length" %in% colnames(milestone_network)) {
    milestone_network$length <- runif(nrow(milestone_network))
  }
  if (!"directed" %in% colnames(milestone_network)) {
    milestone_network$directed <- TRUE
  }

  # get milestone ids
  milestone_ids <- sort(unique(c(milestone_network$from, milestone_network$to)))

  # generate (tented) progressions
  progressions <- random_progressions(milestone_network, ncells = num_cells, allow_tented_progressions = allow_tented_progressions)

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

  # make a simple cell info
  cell_ids <- unique(progressions$cell_id)
  cell_info <- tibble(cell_id = cell_ids)

  # create trajectory
  wrap_data(
    id = unique_id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    dataset_source = "synthetic/dyntoy",
    model = model
  ) %>% add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = divergence_regions,
    progressions = progressions
  )
}


#' Generate a toy dataset
#'
#' @param model A model for generating the milestone network. Must be one of:
#' \itemize{
#'   \item{a character vector (e.g. \code{"linear"}),}
#'   \item{a function (e.g. \code{model_linear}),}
#'   \item{a data frame (e.g. \code{model_linear()})}
#' }
#' @importFrom dynwrap add_prior_information
#'
#' @param unique_id An id for the dataset
#' @param num_cells The number of cells in each dataset
#' @param allow_tented_progressions Whether or not to be able to generate cells as
#'   part of a divergence.
#' @param normalise Whether or not to normalise the dataset
#' @param add_prior_information Whether to add prior information
#' @inheritParams generate_counts
#'
#' @export
generate_dataset <- function(
  unique_id = "",
  model = "linear",
  num_cells = 99,
  allow_tented_progressions = TRUE,
  num_features = 101,
  sample_mean_count = function() runif(1, 100, 1000),
  sample_dispersion_count = function(mean) map_dbl(mean, ~runif(1, ./10, ./4)),
  dropout_probability_factor = 100,
  normalise = dynutils::check_packages("dynnormaliser"),
  add_prior_information = TRUE
) {
  trajectory <- generate_trajectory(
    unique_id = unique_id,
    model = model,
    num_cells = num_cells,
    allow_tented_progressions = allow_tented_progressions
  )

  # generate expression
  counts <- generate_counts(
    trajectory,
    num_features = num_features,
    sample_mean_count = sample_mean_count,
    sample_dispersion_count = sample_dispersion_count,
    dropout_probability_factor = dropout_probability_factor
  )

  # normalise
  if (normalise) {
    normalised <- dynnormaliser::normalise_filter_counts(
      counts,
      filter_hvg = FALSE,
      filter_features = FALSE,
      filter_cells = FALSE
    )
    counts <- normalised$counts
    expression <- normalised$expression
    cell_ids <- intersect(rownames(counts), trajectory$cell_ids)
    progressions <- trajectory$progressions %>% filter(cell_id %in% cell_ids)
    cell_info <- trajectory$cell_info %>% filter(cell_id %in% cell_ids)

    # create trajectory
    trajectory <- wrap_data(
      id = unique_id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      dataset_source = "toy",
      model = model
    ) %>% add_trajectory(
      milestone_ids = trajectory$milestone_ids,
      milestone_network = trajectory$milestone_network,
      divergence_regions = trajectory$divergence_regions,
      progressions = progressions
    )
  } else {
    expression <- log2(counts + 1)
  }

  # make feature info
  feature_info <- tibble(feature_id = colnames(counts), housekeeping = FALSE)

  # wrap dataset
  dataset <- trajectory %>% add_cell_waypoints(
    num_cells_selected = 25
  ) %>% add_expression(
    counts = counts,
    expression = expression,
    feature_info = feature_info
  )

  if (add_prior_information) {
    dataset <- dataset %>% dynwrap::add_prior_information(
      verbose = FALSE
    )
  }

  dataset
}
