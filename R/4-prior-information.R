generate_prior_information <- function(dataset) {
  if(!all(c("milestone_ids", "milestone_network", "progressions", "milestone_percentages", "cell_ids")) %in% dataset) {
    stop("Dataset is incomplete")
  }

  tibble::lst(special_cells, cell_grouping)
}
