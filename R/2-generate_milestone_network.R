#' Generate a toy milestone network
#'
#' @param model A model for generating the milestone network. Must be one of:
#' \itemize{
#'   \item{a character vector (e.g. \code{"linear"}),}
#'   \item{a function (e.g. \code{model_linear}),}
#'   \item{a data frame (e.g. \code{model_linear()})}
#' }
#' @param ... Parameters to pass to other models. Can be in the form of `linear = list(num_milestones = function() sample(2:8, 1)` or just `num_milestones = 10`.
#'
#' @export
#' @importFrom methods formalArgs
generate_milestone_network <- function(
  model = names(topology_models),
  ...
) {
  requireNamespace("igraph")

  if (is.character(model) && length(model) > 1) {
    model <- model[[1]]
  }

  # check params
  params <- list(...)
  network_model <- topology_models[[model]]

  relevant_params <- params
  specific_params <- params[[model]]
  relevant_params[names(specific_params)] <- specific_params

  relevant_paramnames <- intersect(names(relevant_params), formalArgs(network_model))

  # run model
  milnet <- do.call(network_model, relevant_params[relevant_paramnames]) %>%
    as_tibble()

  # check output, add columns if necessary
  if (!"length" %in% colnames(milnet)) {
    milnet$length <- stats::runif(nrow(milnet), min = .5, max = 1)
  }

  if (!"directed" %in% colnames(milnet)) {
    milnet$directed <- TRUE
  }

  # return result
  milnet
}

formals(generate_milestone_network)$model <- names(topology_models)
