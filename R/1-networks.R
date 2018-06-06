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

  multifurcating = function(
    num_multifurcations = rbinom(1, size = 10, .25) + 1,
    max_degree = sample_discrete_uniform(1, 3, 6)
  ) {
    testthat::expect_gte(num_multifurcations, 1)
    testthat::expect_gte(max_degree, 3)

    milnet <- tribble(
      ~from, ~to,
      "M1", "M2"
    )

    num_nodes <- 2

    for (i in seq_len(num_multifurcations)) {
      j <- sample.int(nrow(milnet), 1)

      fr <- milnet$from[[j]]
      to <- milnet$to[[j]]

      num_new_nodes <- rbinom(1, size = max_degree - 3, .25) + 2

      new_nodes <- paste0("M", num_nodes + seq_len(num_new_nodes))
      branchpoint <- new_nodes[[1]]
      num_nodes <- num_nodes + num_new_nodes

      milnet <- milnet %>%
        slice(-j) %>%
        bind_rows(tribble(
          ~from, ~to,
          fr, new_nodes[[1]],
          new_nodes[[1]], to
        )) %>%
        bind_rows(data_frame(
          from = new_nodes[[1]],
          to = new_nodes[-1]
        ))
    }

    milnet
  },

  converging = function(
    num_convergences = rbinom(1, size = 10, .25) + 1,
    max_degree = sample_discrete_uniform(1, 3, 6)
  ) {
    network_models$multifurcating(num_convergences, max_degree) %>%
      rename(from = to, to = from)
  },

  multifurcating_with_loops = function(
    num_multifurcations = rbinom(1, size = 10, .25) + 2,
    max_degree = sample_discrete_uniform(1, 3, 6),
    num_loops = rbinom(1, size = num_multifurcations, .25) + 1
  ) {
    testthat::expect_lte(num_loops, num_multifurcations + 1)

    milnet <- network_models$multifurcating(num_multifurcations, max_degree)

    mils <- unique(c(milnet$from, milnet$to))

    looped_mils <- sample(mils, num_loops * 2)

    milnet %>%
      bind_rows(data_frame(
        from = looped_mils %>% head(num_loops),
        to = looped_mils %>% tail(num_loops)
      ))
  },

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
