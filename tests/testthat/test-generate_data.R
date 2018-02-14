context("Generating datasets with dyntoy")

test_that("Creating toy datasets", {
  models <- c("linear", "cycle")
  num_replicates <- 3
  num_cells <- 10
  num_genes <- 1001
  tasks <- generate_toy_datasets(models = models, num_replicates = num_replicates, num_cells = num_cells, num_genes = num_genes)

  tasks <- tasks %>% mutate(origin = gsub("toy/(.*)_[0-9]*", "\\1", id))

  expect_that( is_tibble(tasks), is_true() )

  required_cols <- c("id", "cell_ids", "milestone_ids", "milestone_network", "milestone_percentages", "progressions", "counts", "geodesic_dist", "prior_information")
  expect_that( all(required_cols %in% colnames(tasks)), is_true() )

  expect_equal( unique(tasks$type), "ti_task" )
  expect_equal( unique(tasks$task_source), "toy" )
  expect_true( all(tasks$origin %in% models) )
  expect_equal( nrow(tasks), length(models) * num_replicates )

  for (tt in models) {
    expect_equal(sum(tasks$origin == tt), num_replicates)
  }
})

test_that("Creating more toy datasets", {
  models <- eval(formals(generate_toy_datasets)$models)
  num_replicates <- 2
  num_cells <- 99
  num_genes <- 101
  tasks <- suppressWarnings({generate_toy_datasets(models = models, num_replicates = num_replicates, num_cells = num_cells, num_genes = num_genes)})

  tasks <- tasks %>% mutate(origin = gsub("toy/(.*)_[0-9]*", "\\1", id))

  expect_that( is_tibble(tasks), is_true() )

  required_cols <- c("id", "cell_ids", "milestone_ids", "milestone_network", "milestone_percentages", "progressions", "counts", "geodesic_dist", "prior_information")
  expect_that( all(required_cols %in% colnames(tasks)), is_true() )

  expect_equal( unique(tasks$type), "ti_task" )
  expect_true( all(tasks$origin %in% models) )
  expect_equal( nrow(tasks), length(models) * num_replicates )

  for (tt in models) {
    expect_equal(sum(tasks$origin == tt), num_replicates)
  }
})

toy_tasks <- dyntoy::toy_tasks

test_that("Data object toy_tasks", {
  expect_that( is_tibble(toy_tasks), is_true() )

  required_cols <- c("id", "cell_ids", "milestone_ids", "milestone_network", "milestone_percentages", "progressions", "counts", "geodesic_dist", "prior_information")
  expect_that( all(required_cols %in% colnames(toy_tasks)), is_true() )

  expect_equal( unique(toy_tasks$type), "ti_task" )
  models <- eval(formals(generate_toy_datasets)$models)
  expect_true( all(gsub("toy/(.*)_[0-9]*", "\\1", toy_tasks$id) %in% models) )
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
    expect_equal( rownames(counts), cell_ids )
    expect_false( any(duplicated(rownames(counts))) )
    expect_false( any(duplicated(colnames(counts))) )

    geodesic_dist <- task$geodesic_dist
    expect_true( is.matrix(geodesic_dist) )
    expect_true( is.numeric(geodesic_dist) )
    expect_equal( rownames(geodesic_dist), cell_ids )
    expect_equal( colnames(geodesic_dist), cell_ids )
    expect_true( all(is.finite(geodesic_dist)) )
    expect_true( all(geodesic_dist >= 0) )

    special_cells <- task$special_cells
    expect_true( all(sapply(special_cells, function(x) all(x %in% cell_ids))) )
  })
}
