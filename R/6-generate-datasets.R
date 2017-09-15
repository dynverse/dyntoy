#' Generate toy datasets with dyngen
#'
#' @param ti_types The types of TI to generate
#' @param num_replicates How many replicates of each TI type to generate
#' @param num_cells The number of cells in each dataset
#' @param num_genes The number of genes in each dataset
#'
#' @export
generate_toy_datasets <- function(ti_types = c("linear", "bifurcating", "cycle"), num_replicates = 3, num_cells = 99, num_genes = 101) {
  settings <- expand.grid(ti_type = ti_types, replicate = seq_len(num_replicates), stringsAsFactors = FALSE)

  dynutils::list_as_tibble(lapply(seq_len(nrow(settings)), function(rowi) {
    list2env(dynutils::extract_row_to_list(settings, rowi), environment())

    generate_dataset(paste0("toy_", ti_type, "_", replicate), ti_type, num_cells, num_genes)
  }))
}
