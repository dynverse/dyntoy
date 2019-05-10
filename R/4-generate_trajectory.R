#' Generate a trajectory
#'
#' @param id An id for the dataset
#'
#' @inheritParams generate_milestone_network
#' @inheritParams generate_progressions
#'
#' @export
generate_trajectory <- dynutils::inherit_default_params(
  list(generate_milestone_network, generate_progressions),
  function(
    id = "",
    model,
    num_cells,
    allow_tented_progressions
  ) {
    if (is.null(id) || id == "") {
      # if you don't specify a unique id, you will be punished
      # by the dynutils random time string!
      id <- dynutils::random_time_string("toy")
    }

    # check model argument
    if (is.character(model) && length(model) > 1) {
      model <- model[[1]]
    }
    if (is.character(model)) {
      milestone_network <- generate_milestone_network(model = model)
    } else if (is.function(model)) {
      milestone_network <- model()
    } else if (is.data.frame(model)) {
      milestone_network <- model
    } else {
      stop("Unrecognised format for 'model'.")
    }

    # add columns if necessary
    if (!"length" %in% colnames(milestone_network)) {
      milestone_network$length <- runif(nrow(milestone_network))
    }
    if (!"directed" %in% colnames(milestone_network)) {
      milestone_network$directed <- TRUE
    }

    # get milestone ids
    milestone_ids <- sort(unique(c(milestone_network$from, milestone_network$to)))

    # generate (tented) progressions
    progressions <- generate_progressions(
      milestone_network = milestone_network,
      num_cells = num_cells,
      allow_tented_progressions = allow_tented_progressions
    )

    # were any divergences created?
    divreg <- progressions %>% group_by(cell_id) %>% filter(n() > 1) %>% ungroup()
    if (nrow(divreg) > 0) {
      froms <- unique(divreg$from)
      divergence_regions <- froms %>% map_df(function(fr) {
        tibble(
          divergence_id = paste0("divergence_", fr),
          milestone_id = unique(c(fr, divreg %>% filter(from == fr) %>% .$to)),
          is_start = milestone_id == fr
        )
      })
    } else {
      divergence_regions <- NULL
    }

    # make a simple cell info
    cell_ids <- unique(progressions$cell_id)
    cell_info <- tibble(cell_id = cell_ids)

    # create trajectory
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      source = "synthetic/dyntoy",
      model = model
    ) %>% add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = divergence_regions,
      progressions = progressions
    )
  }
)
