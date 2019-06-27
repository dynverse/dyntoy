general_graph_model_fun <- function(
  num_modifications = function() sample_discrete_uniform(1, 3, 6),
  max_degree = function() rbinom(1, size = 10, .25) + 1,
  allow_divergences = FALSE,
  allow_loops = FALSE,
  allow_convergences = FALSE
) {
  if (is.function(num_modifications)) num_modifications <- num_modifications()
  if (is.function(max_degree)) max_degree <- max_degree()

  assert_that(
    num_modifications >= 1,
    max_degree >= 3
  )
  testthat::expect_gte(max_degree, 3)

  milnet <- tribble(
    ~from, ~to,
    "M1", "M2"
  )

  num_nodes <- 2

  for (i in seq_len(num_modifications)) {
    j <- sample.int(nrow(milnet), 1)

    fr <- milnet$from[[j]]
    to <- milnet$to[[j]]

    available_types <- c()

    if (allow_divergences) available_types <- available_types %>% c("divergence")
    if (allow_loops) available_types <- available_types %>% c("loop")
    if (allow_convergences) available_types <- available_types %>% c("convergence")
    if (allow_divergences && allow_convergences) available_types <- available_types %>% c("divergence_convergence")

    type <- sample(available_types, 1)

    num_new_nodes <- rbinom(1, size = max_degree - 3, .25) + 2
    new_nodes <- paste0("M", num_nodes + seq_len(num_new_nodes))
    num_nodes <- num_nodes + num_new_nodes

    if (type == "divergence") {
      new_edges <- bind_rows(
        tibble(
          from = new_nodes[[1]],
          to = new_nodes[-1]
        ),
        tribble(
          ~from, ~to,
          fr, new_nodes[[1]],
          new_nodes[[1]], to
        )
      )
    } else if (type == "convergence") {
      new_edges <- bind_rows(
        tibble(
          from = new_nodes[-1],
          to = new_nodes[[1]]
        ),
        tribble(
          ~from, ~to,
          fr, new_nodes[[1]],
          new_nodes[[1]], to
        )
      )
    } else if (type == "loop") {
      new_edges <- tibble(
        from = c(fr, to, new_nodes),
        to = c(to, new_nodes, fr)
      )
    } else if (type == "divergence_convergence") {
      ix <- seq_len(length(new_nodes) / 2)
      nn1 <- new_nodes[ix]
      nn2 <- new_nodes[-ix]
      new_edges <- tibble(
        from = c(fr, nn1, fr, nn2),
        to = c(nn1, to, nn2, to)
      )
    } else {
      stop("Invalid modification type")
    }

    milnet <- milnet %>%
      slice(-j) %>%
      bind_rows(new_edges)
  }

  milnet
}

#' @param num_milestones The number of milestones in the trajectory (linear, cyclic)
#' @rdname topology_models
#' @importFrom utils head tail
#' @export
model_linear <- function(
  num_milestones = function() rbinom(1, size = 10, .25) + 2
) {
  if (is.function(num_milestones)) num_milestones <- num_milestones()

  assert_that(num_milestones >= 2)

  milestone_ids <- paste0("M", seq_len(num_milestones))
  tibble(
    from = milestone_ids %>% head(-1),
    to = milestone_ids %>% tail(-1)
  )
}

#' @rdname topology_models
#' @export
model_cyclic <- function(
  num_milestones = function() rbinom(1, size = 10, .25) + 3
) {
  if (is.function(num_milestones)) num_milestones <- num_milestones()

  assert_that(num_milestones >= 3)

  topology_models$linear(num_milestones) %>%
    add_row(from = paste0("M", num_milestones), to = "M1")
}

#' @param max_degree The maximum degree of a branch node, must be at least 3 (diverging, converging)
#' @rdname topology_models
#' @export
model_bifurcating <- function() {
  general_graph_model_fun(
    num_modifications = 1,
    max_degree = 3,
    allow_divergences = TRUE
  )
}

#' @rdname topology_models
#' @export
model_multifurcating <- function(
  num_branchpoints = function() rbinom(1, size = 10, .25) + 1,
  max_degree = function() sample_discrete_uniform(1, 3, 6)
) {
  general_graph_model_fun(
    num_modifications = num_branchpoints,
    max_degree = max_degree,
    allow_divergences = TRUE
  )
}

#' @param num_branchpoints The number of branchpoints in the trajectory (bifurcating, diverging, converging)
#' @rdname topology_models
#' @export
model_binary_tree <- function(
  num_branchpoints = function() sample_discrete_uniform(1, 3, 6)
) {
  general_graph_model_fun(
    num_modifications = num_branchpoints,
    max_degree = 3,
    allow_divergences = TRUE
  )
}

#' @rdname topology_models
#' @export
model_tree <- function(
  num_branchpoints = function() sample_discrete_uniform(1, 3, 6),
  max_degree = function() sample_discrete_uniform(1, 3, 6)
) {
  general_graph_model_fun(
    num_modifications = num_branchpoints,
    max_degree = max_degree,
    allow_divergences = TRUE
  )
}

#' @rdname topology_models
#' @export
model_converging <- function() {
  general_graph_model_fun(
    num_modifications = 1,
    max_degree = 3,
    allow_convergences = TRUE
  )
}

#' @param num_modifications Number of modifications made to the original trajectory (diverging_converging, diverging_with_loops, multiple_looping, connected)
#' @param nodes_per_modification The number of nodes to use per modification (diverging_converging, diverging_with_loops, multiple_looping, connected)
#' @rdname topology_models
#' @export
model_diverging_converging <- function(
  num_modifications = function() rbinom(1, size = 10, .25) + 1,
  nodes_per_modification = function() sample_discrete_uniform(1, 3, 6)
) {
  general_graph_model_fun(
    num_modifications = num_modifications,
    max_degree = nodes_per_modification,
    allow_divergences = TRUE,
    allow_convergences = TRUE
  )
}

#' @rdname topology_models
#' @export
model_diverging_with_loops <- function(
  num_modifications = function() rbinom(1, size = 10, .25) + 1,
  nodes_per_modification = function() sample_discrete_uniform(1, 3, 6)
) {
  general_graph_model_fun(
    num_modifications = num_modifications,
    max_degree = nodes_per_modification,
    allow_divergences = TRUE,
    allow_loops = TRUE
  )
}

#' @rdname topology_models
#' @export
model_multiple_looping <- function(
  num_modifications = function() rbinom(1, size = 10, .25) + 1,
  nodes_per_modification = function() sample_discrete_uniform(1, 3, 6)
) {
  general_graph_model_fun(
    num_modifications = num_modifications,
    max_degree = nodes_per_modification,
    allow_loops = TRUE
  )
}

#' @rdname topology_models
#' @export
model_connected <- function(
  num_modifications = function() rbinom(1, size = 10, .25) + 1,
  nodes_per_modification = function() sample_discrete_uniform(1, 3, 6)
) {
  general_graph_model_fun(
    num_modifications = num_modifications,
    max_degree = nodes_per_modification,
    allow_loops = TRUE,
    allow_divergences = TRUE,
    allow_convergences = TRUE
  )
}

#' @param num_trajectories The number of disconnected trajectories to generate
#' @param ... Parameters to pass to other models. Can be in the form of `linear = list(num_milestones = function() sample(2:8, 1)` or just `num_milestones = 10`.
#' @rdname topology_models
#' @export
model_disconnected <- function(
  num_trajectories = rbinom(1, size = 5, .25) + 2,
  ...
) {
  map_df(
    seq_len(num_trajectories),
    function(i) {
      models <- names(topology_models) %>% keep(~ . != "disconnected")
      generate_milestone_network(model = sample(models, 1), ...) %>%
        mutate(
          from = paste0("T", i, "_", from),
          to = paste0("T", i, "_", to)
        )
    }
  )
}

#' Milestone network models for generating toy examples
#'
#' @export
topology_models <- list(
  linear = model_linear,
  cyclic = model_cyclic,
  bifurcating = model_bifurcating,
  multifurcating = model_multifurcating,
  binary_tree = model_binary_tree,
  tree = model_tree,
  converging = model_converging,
  diverging_converging = model_diverging_converging,
  diverging_with_loops = model_diverging_with_loops,
  looping = model_multiple_looping,
  connected = model_connected,
  disconnected = model_disconnected
)
