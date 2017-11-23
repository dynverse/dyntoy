#' Generate toy datasets with dyngen
#'
#' @param trajectory_types The types of trajectory to generate
#' @param num_replicates How many replicates of each TI type to generate
#' @param num_cells The number of cells in each dataset
#' @param num_genes The number of genes in each dataset
#' @param expression_randomizer How to randomize the expression
#'
#' @export
generate_toy_datasets <- function(trajectory_types, num_replicates = 3, num_cells = 200, num_genes = 100, expression_randomizer="modules", noise_std=0.25) {
  crossing(trajectory_type = trajectory_types, replicate = seq_len(num_replicates)) %>%
    rowwise() %>%
    do(with(., {
      generate_dataset(paste0("toy_", trajectory_type, "_", replicate), trajectory_type, num_cells, num_genes, expression_randomizer=expression_randomizer, noise_std = noise_std) %>%
        list() %>%
        dynutils::list_as_tibble() %>%
        mutate(replicate = replicate)
    })) %>%
    ungroup()
}

formals(generate_toy_datasets)$trajectory_types <- formals(generate_toy_milestone_network)$trajectory_type
