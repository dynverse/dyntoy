#' @importFrom stats approxfun rnorm runif
generate_expression <- function(milestone_network, progressions, ngenes=100, expression_randomizer = c("modules", "modulo", "shift")) {
  expression_randomizer <- match.arg(expression_randomizer)

  nedges <- nrow(milestone_network)
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  nnodes <- length(milestone_ids)

  milestone_expressions <- list()
  milestone_network <- milestone_network %>% mutate(splinefuns=map(seq_len(n()), ~NULL))

  nmodules <- max(6, nrow(milestone_network) * 10)

  for (edge_id in seq_len(nedges)) {
    edge <- extract_row_to_list(milestone_network, edge_id)

    # check whether the starting and ending milestones have already been visited, otherwise the start and end are random
    if (expression_randomizer == "shift") {
      start <- if (edge$from %in% names(milestone_expressions)) milestone_expressions[[edge$from]] else stats::runif(nmodules)
      end <- if (edge$to %in% names(milestone_expressions)) milestone_expressions[[edge$to]] else stats::runif(nmodules)
    } else if (expression_randomizer == "modulo") {
      start <- if (edge$from %in% names(milestone_expressions)) milestone_expressions[[edge$from]] else as.numeric(((seq_len(nmodules) %% nnodes) +1) == which(milestone_ids == edge$from))
      end <- if (edge$to %in% names(milestone_expressions)) milestone_expressions[[edge$to]] else as.numeric(((seq_len(nmodules) %% nnodes) + 1) == which(milestone_ids == edge$to))
    } else if (expression_randomizer == "modules") {
      start <- if (edge$from %in% names(milestone_expressions)) milestone_expressions[[edge$from]] else sample(c(0, 1), nmodules, replace=TRUE)
      if (edge$to %in% names(milestone_expressions)) {
        end <- milestone_expressions[[edge$to]]
      } else {
        end <- start
        n_changed_modules <- 6
        chosen_modules <- sample(seq_along(end), n_changed_modules)
        end[chosen_modules] <- 1 - end[chosen_modules]
      }
    }

    milestone_expressions[[edge$from]] <- start
    milestone_expressions[[edge$to]] <- end

    if (expression_randomizer == "modules") {
      start <- rep(start, each = ceiling(ngenes/nmodules))[1:ngenes]
      end <- rep(end, each = ceiling(ngenes/nmodules))[1:ngenes]
    }

    if (expression_randomizer == "shift") {
      xs <- map(seq_len(ngenes), ~seq(0, 1, length.out=sample(c(2, 2+edge$length), 1)))
      ys <- pmap(list(x=xs, start=start, end=end), function(x, start, end) c(start, sample(c(0, 1), length(x) - 2, replace=TRUE), end))
    } else if (expression_randomizer == "modulo") {
      xs <- map(seq_len(ngenes), ~c(0, 1))
      ys <- pmap(list(x=xs, start=start, end=end), function(x, start, end) c(start, end))
    } else if (expression_randomizer == "modules") {
      xs <- map(seq_len(ngenes), ~c(0, 1))
      ys <- pmap(list(x=xs, start=start, end=end), function(x, start, end) c(start, end))
    }

    milestone_network$splinefuns[edge_id] <- map2(xs, ys, function(x, y) {
      stats::approxfun(x, y)
    }) %>% list()
  }

  filtered_progression <- progressions %>% # a cell can only be in one edge (maximum in tents)
    group_by(cell_id) %>%
    arrange(-percentage) %>%
    #mutate(percentage = sum(percentage)) %>%
    filter(row_number() == 1)

  # extract expression for each edge
  expression <- filtered_progression %>%
    group_by(from, to) %>%
    summarise(percentages = list(percentage), cell_ids=list(cell_id)) %>%
    left_join(milestone_network, by=c("from", "to")) %>%
    rowwise() %>%
    do(
      expression=map(.$splinefuns, function(f) f(.$percentage)) %>% invoke(rbind, .),
      cell_id=.$cell_id
    ) %>% {
      set_colnames(invoke(cbind, .$expression), unlist(.$cell_id))
    } %>% t

  expression <- expression[unique(progressions$cell_id), ]

  colnames(expression) <- paste0("G", seq_len(ncol(expression)))

  expression
}

generate_counts <- function(expression, noise_nbinom_size=20) {
  count_mean <- 100
  counts <- rnbinom(length(expression), mu = expression * count_mean, size=noise_nbinom_size) %>%
    matrix(nrow=nrow(expression), dimnames=dimnames(expression))
  counts[counts < 0] <- 0
  counts
}


# library(tidyverse)
#
# milestone_network = dyngen::generate_toy_milestone_network("bifurcating")
# progressions = dyngen::random_progressions_tented(milestone_network, 500)
# expression <- generate_expression(milestone_network, progressions, 100)
#
# cell_order <- order_cells(milestone_network, progressions)
# expression[cell_order$order, ] %>% pheatmap::pheatmap(cluster_rows=F, gaps_row = cell_order$edge_id %>% diff() %>% {which(. != 0)})
#
# source("../dyneval/R_modular_TI/dimred_wrappers.R")
# space <- dimred_ica(expression, ndim=2)
# ggplot(space %>% as.data.frame()) + geom_point(aes(Comp1, Comp2))
#
# library(rgl)
# plot3d(space[, 1], space[, 2], space[, 3])
