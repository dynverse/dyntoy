context("Generating datasets with dyntoy")

test_that("Creating toy datasets", {
  models <- c("linear", "cyclic")
  num_replicates <- 3
  num_cells <- 10
  num_genes <- 1001
  tasks <- generate_toy_datasets(models = models, num_replicates = num_replicates, num_cells = num_cells, num_genes = num_genes)

  expect_true( is_tibble(tasks) )

  required_cols <- c(
    "id", "cell_ids", "task_source", "model", "milestone_ids", "milestone_network", "divergence_regions", "milestone_percentages",
    "progressions", "counts", "expression", "prior_information"
  )
  for (rc in required_cols) {
    expect_true(rc %in% colnames(tasks), label = paste0(rc, " %in% colnames(tasks)"))
  }

  expect_equal( unique(tasks$task_source), "toy" )
  expect_true( all(tasks$model %in% models) )
  expect_equal( nrow(tasks), length(models) * num_replicates )

  for (tt in models) {
    expect_equal(sum(tasks$model == tt), num_replicates)
  }
})

test_that("Creating more toy datasets", {
  models <- eval(formals(generate_toy_datasets)$models)
  num_replicates <- 2
  num_cells <- 99
  num_genes <- 101
  tasks <- suppressWarnings({generate_toy_datasets(models = models, num_replicates = num_replicates, num_cells = num_cells, num_genes = num_genes)})

  expect_true( is_tibble(tasks) )

  required_cols <- c(
    "id", "cell_ids", "task_source", "model", "milestone_ids", "milestone_network", "divergence_regions", "milestone_percentages",
    "progressions", "counts", "expression", "prior_information"
  )
  for (rc in required_cols) {
    expect_true(rc %in% colnames(tasks), label = paste0(rc, " %in% colnames(tasks)"))
  }

  expect_equal( unique(tasks$task_source), "toy" )
  expect_true( all(tasks$model %in% models) )
  expect_equal( nrow(tasks), length(models) * num_replicates )

  for (tt in models) {
    expect_equal(sum(tasks$model == tt), num_replicates)
  }
})

toy_tasks <- dyntoy::toy_tasks

test_that("Data object toy_tasks", {
  expect_that( is_tibble(toy_tasks), is_true() )

  required_cols <- c(
    "id", "cell_ids", "task_source", "model", "milestone_ids", "milestone_network", "divergence_regions", "milestone_percentages",
    "progressions", "counts", "expression", "prior_information"
  )
  for (rc in required_cols) {
    expect_true(rc %in% colnames(toy_tasks), label = paste0(rc, " %in% colnames(toy_tasks)"))
  }

  models <- eval(formals(generate_toy_datasets)$models)
  expect_true( all(toy_tasks$model %in% models) )
})



for (taski in seq_len(nrow(toy_tasks))) {
  task <- extract_row_to_list(toy_tasks, taski)

  test_that(paste0("Evaluating with ", task$id), {
    expect_true( is.character(task$id) )

    cell_ids <- task$cell_ids
    milestone_ids <- task$milestone_ids

    expect_true( is.character(cell_ids) )
    expect_true( is.character(milestone_ids) )

    milestone_network <- task$milestone_network
    expect_true( all(milestone_network$from %in% milestone_ids) )
    expect_true( all(milestone_network$to %in% milestone_ids) )
    expect_true( all(milestone_network$length > 0) )
    expect_true( is.logical(milestone_network$directed) )

    milestone_percentages <- task$milestone_percentages
    expect_equal( sort(unique(milestone_percentages$cell_id)), sort(cell_ids) )
    expect_true( all(milestone_percentages$milestone_id %in% milestone_ids) )
    # todo: check whether a cell is in a tent

    mp_summ <- milestone_percentages %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
    expect_true( all(mp_summ$sum == 1) )

    progressions <- task$progressions
    expect_equal( sort(unique(progressions$cell_id)), sort(cell_ids) )
    expect_true( all(c(progressions$from, progressions$to) %in% milestone_ids) )

    pr_summ <- progressions %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
    expect_true( all(mp_summ$sum <= 1) )

    counts <- task$counts
    expect_true( is.numeric(counts) )
    expect_true( is.matrix(counts) )
    expect_true( all(rownames(counts) %in% cell_ids ))
    expect_false( any(duplicated(rownames(counts))) )
    expect_false( any(duplicated(colnames(counts))) )

    # TODO: add check for divergence regions and prior_information
  })
}
