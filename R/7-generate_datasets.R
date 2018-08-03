#' Generate toy datasets with dyntoy
#'
#' @param models A list of network models to use to generate the milestone networks.
#' \itemize{
#'   \item{a character vector (e.g. \code{c("linear", "bifurcating")}),}
#'   \item{a list of functions (e.g. \code{list(model_linear, model_bifurcating)}),}
#'   \item{a list of data frames (e.g. \code{list(model_linear(), model_bifurcating())})}
#' }
#' @inheritParams generate_dataset
#' @param num_replicates How many replicates of each TI type to generate
#'
#' @export
generate_datasets <- dynutils::inherit_default_params(
  list(generate_dataset),
  function(
    models = names(topology_models),
    num_cells,
    num_features,
    allow_tented_progressions,
    sample_mean_count,
    sample_dispersion_count,
    dropout_probability_factor,
    normalise,
    add_prior_information,
    num_replicates = 3
  ) {
    crossing(
      model = models,
      replicate = seq_len(num_replicates)
    ) %>%
      rowwise() %>%
      do(with(., {
        generate_dataset(
          id = paste0("toy/", model, "_", replicate),
          model = model,
          num_cells = num_cells,
          num_features = num_features,
          allow_tented_progressions = allow_tented_progressions,
          sample_mean_count = sample_mean_count,
          sample_dispersion_count = sample_dispersion_count,
          dropout_probability_factor = dropout_probability_factor,
          normalise = normalise,
          add_prior_information = add_prior_information
        ) %>%
          list() %>%
          dynutils::list_as_tibble() %>%
          mutate(replicate = replicate)
      })) %>%
      ungroup()
  }
)

formals(generate_datasets)$models <- names(topology_models)
