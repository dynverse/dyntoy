#' Generate a toy milestone network
#'
#' @param model A model for generating the milestone network. Must be one of:
#' \itemize{
#'   \item{a character vector (e.g. \code{"linear"}),}
#'   \item{a function (e.g. \code{model_linear}),}
#'   \item{a data frame (e.g. \code{model_linear()})}
#' }
#' @inheritDotParams topology_models
#'
#' @export
generate_milestone_network <- function(
  model = names(topology_models),
  ...
) {
  requireNamespace("igraph")
  model <- match.arg(model)

  # check params
  params <- list(...)
  network_model <- topology_models[[model]]

  relevant_params <- params
  specific_params <- params[[model]]
  relevant_params[names(specific_params)] <- specific_params

  relevant_paramnames <- intersect(names(relevant_params), formalArgs(network_model))

  # run model
  milnet <- do.call(network_model, relevant_params[relevant_paramnames]) %>%
    as_data_frame()

  # check output, add columns if necessary
  if (!"length" %in% colnames(milnet)) {
    milnet$length <- runif(nrow(milnet))
  }

  if (!"directed" %in% colnames(milnet)) {
    milnet$directed <- TRUE
  }

  # return result
  milnet
}

formals(generate_milestone_network)$model <- names(topology_models)
