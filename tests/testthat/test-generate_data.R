context("Generating datasets with dyntoy")

test_that("Creating toy datasets", {
  models <- c("linear", "cyclic")
  num_replicates <- 3
  num_cells <- 10
  num_features <- 1001
  datasets <- generate_datasets(models = models, num_replicates = num_replicates, num_cells = num_cells, num_features = num_features)

  expect_true( is_tibble(datasets) )

  required_cols <- c(
    "id", "cell_ids", "source", "model", "milestone_ids", "milestone_network", "divergence_regions", "milestone_percentages",
    "progressions", "counts", "expression", "prior_information"
  )
  for (rc in required_cols) {
    expect_true(rc %in% colnames(datasets), label = paste0(rc, " %in% colnames(datasets)"))
  }

  expect_equal( unique(datasets$source), "synthetic/dyntoy" )
  expect_true( all(datasets$model %in% models) )
  expect_equal( nrow(datasets), length(models) * num_replicates )

  for (tt in models) {
    expect_equal(sum(datasets$model == tt), num_replicates)
  }
})

test_that("Creating more toy datasets", {
  models <- eval(formals(generate_datasets)$models)
  num_replicates <- 1
  num_cells <- 99
  num_features <- 101
  datasets <- suppressWarnings({generate_datasets(models = models, num_replicates = num_replicates, num_cells = num_cells, num_features = num_features)})


  expect_true( is_tibble(datasets) )

  required_cols <- c(
    "id", "cell_ids", "source", "model", "milestone_ids", "milestone_network", "divergence_regions", "milestone_percentages",
    "progressions", "counts", "expression", "prior_information"
  )
  for (rc in required_cols) {
    expect_true(rc %in% colnames(datasets), label = paste0(rc, " %in% colnames(datasets)"))
  }

  expect_equal( unique(datasets$source), "synthetic/dyntoy" )
  expect_true( all(datasets$model %in% models) )
  expect_equal( nrow(datasets), length(models) * num_replicates )

  for (tt in models) {
    expect_equal(sum(datasets$model == tt), num_replicates)
  }
})

toy_datasets <- dyntoy::toy_datasets

test_that("Data object toy_datasets", {
  expect_that( is_tibble(toy_datasets), is_true() )

  required_cols <- c(
    "id", "cell_ids", "source", "model", "milestone_ids", "milestone_network", "divergence_regions", "milestone_percentages",
    "progressions", "counts", "expression", "prior_information"
  )
  for (rc in required_cols) {
    expect_true(rc %in% colnames(toy_datasets), label = paste0(rc, " %in% colnames(toy_datasets)"))
  }

  models <- eval(formals(generate_datasets)$models)
  expect_true( all(toy_datasets$model %in% models) )
})



for (dataseti in seq_len(nrow(toy_datasets))) {
  dataset <- extract_row_to_list(toy_datasets, dataseti)

  test_that(paste0("Evaluating with ", dataset$id), {
    expect_true( is.character(dataset$id) )

    cell_ids <- dataset$cell_ids
    milestone_ids <- dataset$milestone_ids

    expect_true( is.character(cell_ids) )
    expect_true( is.character(milestone_ids) )

    milestone_network <- dataset$milestone_network
    expect_true( all(milestone_network$from %in% milestone_ids) )
    expect_true( all(milestone_network$to %in% milestone_ids) )
    expect_true( all(milestone_network$length > 0) )
    expect_true( is.logical(milestone_network$directed) )

    milestone_percentages <- dataset$milestone_percentages
    expect_equal( sort(unique(milestone_percentages$cell_id)), sort(cell_ids) )
    expect_true( all(milestone_percentages$milestone_id %in% milestone_ids) )
    # todo: check whether a cell is in a tent

    mp_summ <- milestone_percentages %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
    expect_true( all(mp_summ$sum == 1) )

    progressions <- dataset$progressions
    expect_equal( sort(unique(progressions$cell_id)), sort(cell_ids) )
    expect_true( all(c(progressions$from, progressions$to) %in% milestone_ids) )

    pr_summ <- progressions %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
    expect_true( all(mp_summ$sum <= 1) )

    counts <- dataset$counts
    expect_true( is.numeric(counts) )
    expect_true( is.matrix(counts) )
    expect_true( all(rownames(counts) %in% cell_ids ))
    expect_false( any(duplicated(rownames(counts))) )
    expect_false( any(duplicated(colnames(counts))) )

    # TODO: add check for divergence regions and prior_information
    # I don't think this is necessary, as this is already checked in dynwrap
  })
}

test_that("Creating toy datasets with different models", {
  toy <- generate_dataset(model = "bifurcating")
  toy <- generate_dataset(model = model_bifurcating())
  toy <- generate_dataset(model = model_bifurcating)
})
