#' Add projected expression to a directed trajectory
#'
#' @param trajectory The trajectory
#' @param expected_geodesic_difference How far in the future the velocity goes
#' @param velocity_rescale The length of the velocity vector relative to the geodesic distance
#'
#' @export
add_velocity <- function(
  trajectory,
  expected_geodesic_difference = 0.2,
  velocity_rescale = 0.5
) {
  assert_that(trajectory$directed)

  geodesic_distances <- dynwrap::calculate_geodesic_distances(
    trajectory,
    waypoint_cells = trajectory$cell_ids,
    directed = TRUE
  )

  cell_id <- first(rownames(trajectory$expression))
  expression_projected <- map(rownames(trajectory$expression), function(cell_id) {
    geodesic_distance <- geodesic_distances[cell_id, ]

    # find closest forwards
    reference_cell_id <- names(which.min(abs(expected_geodesic_difference - geodesic_distance)))

    reference_geodesic_difference <- geodesic_distance[reference_cell_id]
    cell_expression <- trajectory$expression[cell_id, ]
    reference_expression <- trajectory$expression[reference_cell_id, ]

    # reference_expression
    cell_expression + (reference_expression - cell_expression)
  }) %>% do.call(rbind, .)
  rownames(expression_projected) <- rownames(trajectory$expression)
  trajectory$expression_projected <- expression_projected

  trajectory
}
