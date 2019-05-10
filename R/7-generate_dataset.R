#' Generate a toy dataset
#'
#' @inheritParams generate_trajectory
#' @inheritParams generate_counts
#' @param add_prior_information Whether to add prior information
#' @param add_velocity Whether to simulate RNA velocity
#' @param normalise Whether or not to normalise the dataset
#'
#' @importFrom dynwrap add_prior_information
#'
#' @export
generate_dataset <- dynutils::inherit_default_params(
  list(generate_trajectory, generate_counts),
  function(
    id,
    model,
    num_cells,
    num_features,
    allow_tented_progressions,
    sample_mean_count,
    sample_dispersion_count,
    dropout_probability_factor,
    differentially_expressed_rate,
    normalise = FALSE,
    add_prior_information = TRUE,
    add_velocity = TRUE
  ) {
    if (is.character(model) && length(model) > 1) {
      model <- model[[1]]
    }

    trajectory <- generate_trajectory(
      id = id,
      model = model,
      num_cells = num_cells,
      allow_tented_progressions = allow_tented_progressions
    )

    # generate expression
    count_generation_results <- generate_counts(
      trajectory,
      num_features = num_features,
      sample_mean_count = sample_mean_count,
      sample_dispersion_count = sample_dispersion_count,
      dropout_probability_factor = dropout_probability_factor,
      differentially_expressed_rate = differentially_expressed_rate
    )

    counts <- count_generation_results$counts
    tde_overall <- count_generation_results$tde_overall

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

      tde_overall <- tde_overall %>% filter(feature_id %in% colnames(counts))

      # create trajectory
      trajectory <- wrap_data(
        id = id,
        cell_ids = cell_ids,
        cell_info = cell_info,
        source = trajectory$source,
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

    # add tde
    dataset <- add_tde_overall(
      trajectory = dataset,
      tde_overall = tde_overall
    )

    # add velocity
    if (add_velocity) {
      dataset <- add_velocity(
        trajectory = dataset
      )
    }


    if (add_prior_information) {
      dataset <- dataset %>% dynwrap::add_prior_information(
        verbose = FALSE
      )
    }

    dataset
  }
)
