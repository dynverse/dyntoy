
#' Generate cells and their progressions along a milestone network
#'
#' @param milestone_network The milestone network along which to generate cells.
#' @param num_cells The number of cells in each dataset
#' @param allow_tented_progressions Whether or not to be able to generate cells as
#'   part of a divergence.
#'
#' @importFrom stats runif
generate_progressions <- function(
  milestone_network,
  num_cells = 99,
  allow_tented_progressions = TRUE
) {
  from_probabilities <- milestone_network %>%
    group_by(from) %>%
    summarise(prob = sqrt(sum(length^2))) %>%
    mutate(use_tent = allow_tented_progressions & sample(c(TRUE, FALSE), n(), replace = TRUE))
  cell_ids <- paste0("C", seq_len(num_cells))

  from_ixs <- sample(
    seq_len(nrow(from_probabilities)),
    num_cells,
    prob = from_probabilities$prob,
    replace = TRUE
  )

  # TODO: this can probably be done much faster if we first split the cells based on a from...
  seq_len(num_cells) %>% map_df(function(i) {
    cell_id <- cell_ids[[i]]

    from_mid <- from_probabilities$from[[from_ixs[[i]]]]
    use_tent <- from_probabilities$use_tent[[from_ixs[[i]]]]

    poss_tos <- milestone_network %>% filter(from == from_mid) %>% .$to

    type <- sample(
      c("start", "end", "edge", "tent"),
      size = 1,
      prob = c(1, 1, 4, ifelse(use_tent, 4, 0))
    )

    switch(
      type,
      start = tibble(cell_id, from = from_mid, to = sample(poss_tos, 1), percentage = 0),
      end = tibble(cell_id, from = from_mid, to = sample(poss_tos, 1), percentage = 1),
      edge = tibble(cell_id, from = from_mid, to = sample(poss_tos, 1), percentage = stats::runif(1, 0, 1)),
      tent = {
        progression_pct <- stats::runif(1, 0, 1)
        to_percentage <- stats::runif(length(poss_tos))
        percentage <- to_percentage / sum(to_percentage) * (1 - progression_pct)
        tibble(cell_id, from = from_mid, to = poss_tos, percentage)
      }
    )
  })
}
