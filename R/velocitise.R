#' Velocitise a toy dataset
#'
#' This will cut the number of cells in half.
#'
#' Uses a heuristic to try to pairwise match half of the cells with the other half of the cells.
#' After running the heuristic `num_heuristic_tries` times, the best heuristic will be chosen.
#'
#' @param num_heuristic_tries number of heuristical tries
#' @param wanted_geodesic_quantile the desired geodesic distance between a cells current state and future state
#'   is this quantile value of all geodesic distances.
#'
#' @export
velocitise <- function(toy, num_heuristic_tries = 100, wanted_geodesic_quantile = .05) {
  geo <- dynwrap::calculate_geodesic_distances(toy, waypoint_cells = toy$cell_ids)

  wanted_distance <- quantile(geo, wanted_geodesic_quantile)

  subset_cells <- sample.int(length(toy$cell_ids), length(toy$cell_ids) / 2)
  subset_geo <- geo[subset_cells, -subset_cells]

  heuristic <- function() {
    ix <- rep(NA, nrow(subset_geo))
    fil <- rep(TRUE, nrow(subset_geo))

    for (i in sample.int(nrow(subset_geo))) {
      j <- which(fil)[which.min(abs(subset_geo[i, fil] - wanted_distance))]
      ix[[i]] <- j
      fil[[j]] <- FALSE
    }

    ix
  }

  out <-
    map_df(
      seq_len(num_heuristic_tries),
      function(i) {
        ix <- heuristic()
        dists <- subset_geo[cbind(seq_along(ix), ix)]
        mean_sqr_dist <- mean((dists - wanted_distance)^2)

        tibble(
          i,
          ix = list(ix),
          mean_sqr_dist
        )
      }
    ) %>% arrange(mean_sqr_dist)

  start_dists <- dynwrap::calculate_geodesic_distances(
    toy,
    waypoint_cells = toy$prior_information$start_id
  )[1,]

  velocity_links <-
    tibble(
      cell1 = subset_cells,
      cell2 = seq_along(toy$cell_ids)[-subset_cells][out$ix[[1]]],
      dist1 = start_dists[cell1],
      dist2 = start_dists[cell2]
    ) %>%
    mutate(
      cell_id = toy$cell_ids[ifelse(dist1 < dist2, cell1, cell2)],
      future_id = toy$cell_ids[ifelse(dist1 < dist2, cell2, cell1)]
    )

  assertthat::assert_that(length(unique(c(velocity_links$cell1, velocity_links$cell2))) == length(toy$cell_ids))

  rna_velocity <- toy$expression[velocity_links$future_id, , drop = FALSE]
  rownames(rna_velocity) <- velocity_links$cell_id

  new_toy <-
    dynwrap::wrap_expression(
      id = toy$id,
      counts = toy$counts[velocity_links$cell_id, , drop = FALSE],
      expression = toy$expression[velocity_links$cell_id, , drop = FALSE],
      rna_velocity = rna_velocity
    ) %>%
    dynwrap::add_trajectory(
      milestone_ids = toy$milestone_ids,
      milestone_network = toy$milestone_network,
      divergence_regions = toy$divergence_regions,
      milestone_percentages = toy$milestone_percentages %>% filter(cell_id %in% velocity_links$cell_id)
    )

  new_toy
}
