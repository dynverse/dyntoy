#' @importFrom stats runif
random_progressions <- function(milestone_network, ncells=100) {
  cell_ids <- paste0("C", seq_len(ncells))
  tibble(cell_id = cell_ids) %>%
    bind_cols(milestone_network[sample(seq_len(nrow(milestone_network)), length(cell_ids), replace=TRUE, prob=milestone_network$length), ]) %>%
    mutate(percentage = map_dbl(length, ~stats::runif(1, 0, .))) %>%
    select(cell_id, from, to, percentage)
}

#' @importFrom stats runif
random_progressions_tented <- function(milestone_network, ncells=100) {
  from_probabilities <- milestone_network %>% group_by(from) %>% summarise(prob=sqrt(sum(length^2)))
  cell_ids <- paste0("C", seq_len(ncells))

  data_frame(
    cell_id = cell_ids,
    from = sample(from_probabilities$from, length(cell_ids), replace = TRUE, prob = from_probabilities$prob)
  ) %>%
    left_join(milestone_network, by = "from") %>%
    group_by(cell_id) %>%
    mutate(
      progression_pct = stats::runif(1),
      to_percentage = stats::runif(n()),
      percentage = to_percentage / sum(to_percentage) * (1-progression_pct)
    ) %>%
    ungroup() %>%
    select(cell_id, from, to, percentage)
}
