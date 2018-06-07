general_graph_model_fun <- function(
  ...
) {
  fun <- function(
    num_modifications = NULL,
    max_degree = NULL,
    allow_divergences = FALSE,
    allow_loops = FALSE,
    allow_convergences = FALSE
  ) {
      if (is.null(max_degree)) {
        max_degree <- sample_discrete_uniform(1, 3, 6)
      }
      if (is.null(num_modifications)) {
        num_modifications <- rbinom(1, size = 10, .25) + 1
      }
      testthat::expect_gte(num_modifications, 1)
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
            data_frame(
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
            data_frame(
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
          new_edges <- data_frame(
            from = c(fr, to, new_nodes),
            to = c(to, new_nodes, fr)
          )
        } else if (type == "divergence_convergence") {
          ix <- seq_len(length(new_nodes) / 2)
          nn1 <- new_nodes[ix]
          nn2 <- new_nodes[-ix]
          new_edges <- data_frame(
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
  override <- list(...)
  form <- formals(fun)
  form[names(override)] <- override
  formals(fun) <- form
  fun
}

#' Milestone network models
network_models <- list(
  linear = function(num_milestones = rbinom(1, size = 10, .25) + 2) {
    testthat::expect_gte(num_milestones, 2)

    milestone_ids <- paste0("M", seq_len(num_milestones))
    data_frame(
      from = milestone_ids %>% head(-1),
      to = milestone_ids %>% tail(-1)
    )
  },

  cyclic = function(num_milestones = rbinom(1, size = 10, .25) + 3) {
    testthat::expect_gte(num_milestones, 3)

    network_models$linear(num_milestones) %>%
      add_row(from = paste0("M", num_milestones), to = "M1")
  },

  bifurcating = general_graph_model_fun(max_degree = 3, allow_divergences = TRUE),

  diverging = general_graph_model_fun(allow_divergences = TRUE),

  converging = general_graph_model_fun(allow_convergences = TRUE),

  diverging_converging = general_graph_model_fun(allow_divergences = TRUE, allow_convergences = TRUE),

  diverging_with_loops = general_graph_model_fun(allow_divergences = TRUE, allow_loops = TRUE),

  looping = general_graph_model_fun(allow_loops = TRUE),

  general_graph = general_graph_model_fun(allow_divergences = TRUE, allow_convergences = TRUE, allow_loops = TRUE),

  disconnected = function(
    num_trajectories = rbinom(1, size = 5, .25) + 2
  ) {
    map_df(
      seq_len(num_trajectories),
      function(i) {
        j <- sample(which(names(network_models) != "disconnected"), 1)
        network_models[[j]]() %>%
          mutate(
            from = paste0("T", i, "_", from),
            to = paste0("T", i, "_", to)
          )
      }
    )
  }
)


#' Generate a toy milestone network
#'
#' @param model Which model to use.
#' @param ... Extra params for each model. See \code{\link{network_models}}
generate_toy_milestone_network <- function(
  model = names(network_models),
  ...
) {
  requireNamespace("igraph")
  model <- match.arg(model)

  milnet <- network_models[[model]](...) %>%
    as_data_frame()

  if (!"length" %in% colnames(milnet)) {
    milnet$length <- runif(nrow(milnet))
  }

  if (!"directed" %in% colnames(milnet)) {
    milnet$directed <- TRUE
  }

  milnet
}

formals(generate_toy_milestone_network)$model <- names(network_models)
