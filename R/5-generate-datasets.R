#' Generate toy datasets with dyngen
#'
#' @param models The types of trajectory to generate
#' @param num_replicates How many replicates of each TI type to generate
#' @param num_cells The number of cells in each dataset
#' @param num_genes The number of genes in each dataset
#' @param expression_randomizer How to randomize the expression
#' @param noise_nbinom_size The size parameter of the nbinom distribution
#' @param use_tented_progressions Whether or not to be able to generate cells as
#'   part of a divergence
#'
#' @export
generate_toy_datasets <- function(
  models,
  num_replicates = 3,
  num_cells = 200,
  num_genes = 100,
  expression_randomizer = "modules",
  noise_nbinom_size = 20,
  use_tented_progressions = TRUE
) {
  crossing(model = models, replicate = seq_len(num_replicates)) %>%
    rowwise() %>%
    do(with(., {
      generate_dataset(
        unique_id = paste0("toy/", model, "_", replicate),
        model = model,
        num_cells = num_cells,
        num_genes = num_genes,
        expression_randomizer = expression_randomizer,
        noise_nbinom_size = noise_nbinom_size,
        use_tented_progressions = use_tented_progressions
      ) %>%
        list() %>%
        dynutils::list_as_tibble() %>%
        mutate(replicate = replicate)
    })) %>%
    ungroup()
}

formals(generate_toy_datasets)$models <- formals(generate_toy_milestone_network)$model
