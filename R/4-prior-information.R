generate_prior_information <- function(milestone_ids, milestone_network, progressions, milestone_percentages) {
  # start cells
  # check if there are one or more starting milestones
  start_milestones <- setdiff(milestone_ids, milestone_network$to)
  if (length(start_milestones) > 0) {
    start_cells <- progressions %>%
      filter(from == start_milestones) %>%
      group_by(from) %>%
      arrange(percentage) %>%
      filter(row_number() == 1) %>%
      pull(cell_id) %>%
      unique()
    if (length(start_cells) != length(start_milestones)) {warning("Not every start milestone has a cell")}
  } else {
    start_cells <- unique(progressions$cell_id)
  }

  # end cells
  end_milestones <- setdiff(milestone_ids, milestone_network$from)
  end_cells = progressions %>%
    filter(to %in% end_milestones) %>%
    group_by(to) %>%
    arrange(percentage) %>%
    summarise(cell_id=cell_id[which.max(percentage)]) %>%
    pull(cell_id) %>%
    unique()
  if (length(end_cells) != length(end_milestones)) {warning("Not every end milestone has a cell")}

  # cell grouping
  grouping_assignment <- dynutils::get_cell_grouping(milestone_percentages)
  grouping_network <- milestone_network %>% select(from, to)

  tibble::lst(start_milestones, start_cells, end_milstones, end_cells, grouping_assignment, grouping_network)
}
