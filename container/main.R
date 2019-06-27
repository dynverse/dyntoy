#!/usr/local/bin/Rscript

library(optparse)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(dyntoy)

parser <-
  OptionParser(usage = paste0("LOCAL=/path/to/folder; MOUNT=/ti; docker run -v $LOCAL:$MOUNT dynverse/dyneval")) %>%
  add_option("--model", type = "character", help = "The model, such as linear, bifurcating or tree") %>%
  add_option("--num_cells", type = "integer", help = "Number of cells", default = 100) %>%
  add_option("--num_features", type = "integer", help = "Number of features (genes)", default = 100) %>%
  add_option("--output", type = "character", help = "Filename of the scores, example: $MOUNT/dataset.(h5|loom). Will be a json file containing the scores.")

parsed_args <- parse_args(parser, args = commandArgs(trailingOnly = TRUE))

if (any(sapply(parsed_args[c("output")], is.null))) {
  stop("output arguments are mandatory")
}

# read dataset and model
dataset <- dyntoy::generate_dataset(
  model = parsed_args$model,
  num_cells = parsed_args$num_cells,
  num_features = parsed_args$num_features
)

# write output
dynutils::write_h5(dataset, parsed_args$output)
