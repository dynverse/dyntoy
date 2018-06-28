#' @importFrom stats approxfun rnorm runif
generate_expression <- function(
  milestone_network,
  progressions,
  ngenes = 100
) {

  nedges <- nrow(milestone_network)
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  nnodes <- length(milestone_ids)

  milestone_expressions <- list()
  milestone_network <- milestone_network %>% mutate(splinefuns = map(seq_len(n()), ~NULL))

  nmodules <- max(6, nrow(milestone_network) * 10)

  for (edge_id in seq_len(nedges)) {
    edge <- extract_row_to_list(milestone_network, edge_id)

    # check whether the starting and ending milestones have already been visited, otherwise the start and end are random
    if (edge$from %in% names(milestone_expressions)) {
      start <- milestone_expressions[[edge$from]]
    } else {
      start <- sample(c(0, 1), nmodules, replace = TRUE)
    }

    if (edge$to %in% names(milestone_expressions)) {
      end <- milestone_expressions[[edge$to]]
    } else {
      end <- start
      n_changed_modules <- 6
      chosen_modules <- sample(seq_along(end), n_changed_modules)
      end[chosen_modules] <- 1 - end[chosen_modules]
    }

    milestone_expressions[[edge$from]] <- start
    milestone_expressions[[edge$to]] <- end

    start <- rep(start, each = ceiling(ngenes/nmodules))[seq_len(ngenes)]
    end <- rep(end, each = ceiling(ngenes/nmodules))[seq_len(ngenes)]

    xs <- map(seq_len(ngenes), ~c(0, 1))
    ys <- pmap(list(x = xs, start = start, end = end), function(x, start, end) c(start, end))

    milestone_network$splinefuns[edge_id] <- map2(xs, ys, function(x, y) {
      stats::approxfun(x, y)
    }) %>% list()
  }

  filtered_progression <- progressions %>% # a cell can only be in one edge (maximum in tents)
    group_by(cell_id) %>%
    arrange(-percentage) %>%
    filter(row_number() == 1)

  # extract expression for each edge
  expression <- filtered_progression %>%
    group_by(from, to) %>%
    summarise(percentages = list(percentage), cell_ids = list(cell_id)) %>%
    left_join(milestone_network, by = c("from", "to")) %>%
    rowwise() %>%
    do(
      expression = map(.$splinefuns, function(f) f(.$percentage)) %>% invoke(rbind, .),
      cell_id = .$cell_id
    ) %>% {
      set_colnames(invoke(cbind, .$expression), unlist(.$cell_id))
    } %>% t

  expression <- expression[unique(progressions$cell_id), ]

  colnames(expression) <- paste0("G", seq_len(ncol(expression)))

  expression
}

#' @importFrom stats rnbinom
generate_counts <- function(expression, noise_nbinom_size = 20) {
  count_mean <- 100
  counts <- stats::rnbinom(length(expression), mu = expression * count_mean, size = noise_nbinom_size) %>%
    matrix(nrow = nrow(expression), dimnames = dimnames(expression))
  counts[counts < 0] <- 0
  counts
}
