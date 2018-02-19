#' @importFrom stats runif
random_progressions <- function(milestone_network, ncells = 100, use_tented = TRUE) {
  from_probabilities <- milestone_network %>% group_by(from) %>% summarise(prob = sqrt(sum(length^2)))
  cell_ids <- paste0("C", seq_len(ncells))

  cell_ids %>% map_df(function(cell_id) {
    from_mid <- sample(from_probabilities$from, 1, prob = from_probabilities$prob)

    poss_tos <- milestone_network %>% filter(from == from_mid) %>% .$to

    type <- sample(
      c("start", "end", "edge", "tent"),
      size = 1,
      prob = c(.1, .1, .4, ifelse(use_tented, .4, 0))
    )

    switch(
      type,
      start = data_frame(cell_id, from = from_mid, to = sample(poss_tos, 1), percentage = 0),
      end = data_frame(cell_id, from = from_mid, to = sample(poss_tos, 1), percentage = 1),
      edge = data_frame(cell_id, from = from_mid, to = sample(poss_tos, 1), percentage = stats::runif(1, 0, 1)),
      tent = {
        progression_pct <- stats::runif(1, 0, 1)
        to_percentage <- stats::runif(length(poss_tos))
        percentage <- to_percentage / sum(to_percentage) * (1 - progression_pct)
        data_frame(cell_id, from = from_mid, to = poss_tos, percentage)
      }
    )
  })
}
