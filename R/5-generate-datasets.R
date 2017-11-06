#' Generate toy datasets with dyngen
#'
#' @param ti_types The types of TI to generate
#' @param num_replicates How many replicates of each TI type to generate
#' @param num_cells The number of cells in each dataset
#' @param num_genes The number of genes in each dataset
#'
#' @export
generate_toy_datasets <- function(ti_types, num_replicates = 3, num_cells = 99, num_genes = 101) {
  crossing(ti_type = ti_types, replicate = seq_len(num_replicates)) %>%
    rowwise() %>%
    do(with(., {
      generate_dataset(paste0("toy_", ti_type, "_", replicate), ti_type, num_cells, num_genes) %>%
        list() %>%
        dynutils::list_as_tibble() %>%
        mutate(replicate = replicate)
    })) %>%
    ungroup()
}

formals(generate_toy_datasets)$ti_types <- formals(generate_toy_milestone_network)$ti_type
