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
generate_toy_datasets <- function(
  models,
  num_replicates = 3,
  num_cells = 200,
  num_genes = 100,
  noise_nbinom_size = 20,
  allow_tented_progressions = TRUE,
  normalise = dynutils::check_packages("dynnormaliser")
) {
  crossing(model = models, replicate = seq_len(num_replicates)) %>%
    rowwise() %>%
    do(with(., {
      generate_dataset(
        unique_id = paste0("toy/", model, "_", replicate),
        model = model,
        num_cells = num_cells,
        num_genes = num_genes,
        noise_nbinom_size = noise_nbinom_size,
        allow_tented_progressions = allow_tented_progressions,
        normalise = normalise
      ) %>%
        list() %>%
        dynutils::list_as_tibble() %>%
        mutate(replicate = replicate)
    })) %>%
    ungroup()
}

formals(generate_toy_datasets)$models <- formals(generate_milestone_network)$model
