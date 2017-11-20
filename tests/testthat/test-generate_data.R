context("Generating datasets with dyntoy")

test_that("Creating toy datasets", {
  ti_types <- c("linear", "cycle")
  num_replicates <- 3
  num_cells <- 10
  num_genes <- 1001
  tasks <- generate_toy_datasets(ti_types = ti_types, num_replicates = num_replicates, num_cells = num_cells, num_genes = num_genes)

  expect_that( is_tibble(tasks), is_true() )

  required_cols <- c("id", "cell_ids", "milestone_ids", "milestone_network", "milestone_percentages", "progressions", "counts", "geodesic_dist", "prior_information")
  expect_that( all(required_cols %in% colnames(tasks)), is_true() )

  expect_equal( unique(tasks$type), "ti_toy" )
  expect_true( all(tasks$ti_type %in% ti_types) )
  expect_true( all(tasks$counts %>% map_lgl(~ nrow(.) == num_cells)) )
  expect_true( all(tasks$counts %>% map_lgl(~ ncol(.) == num_genes)) )
  expect_equal( nrow(tasks), length(ti_types) * num_replicates )
  expect_true( all(tasks$cell_ids %>% map_lgl(~ length(.) == num_cells )) )
})

test_that("Creating more toy datasets", {
  ti_types <- eval(formals(generate_toy_datasets)$ti_types)
  num_replicates <- 2
  num_cells <- 99
  num_genes <- 101
  tasks <- suppressWarnings({generate_toy_datasets(ti_types = ti_types, num_replicates = num_replicates, num_cells = num_cells, num_genes = num_genes)})

  expect_that( is_tibble(tasks), is_true() )

  required_cols <- c("id", "cell_ids", "milestone_ids", "milestone_network", "milestone_percentages", "progressions", "counts", "geodesic_dist", "prior_information")
  expect_that( all(required_cols %in% colnames(tasks)), is_true() )

  expect_equal( unique(tasks$type), "ti_toy" )
  expect_true( all(tasks$ti_type %in% ti_types) )
  expect_true( all(tasks$counts %>% map_lgl(~ nrow(.) == num_cells)) )
  expect_true( all(tasks$counts %>% map_lgl(~ ncol(.) == num_genes)) )
  expect_equal( nrow(tasks), length(ti_types) * num_replicates )
  expect_true( all(tasks$cell_ids %>% map_lgl(~ length(.) == num_cells )) )
})

toy_tasks <- dyntoy::toy_tasks

test_that("Data object toy_tasks", {
  expect_that( is_tibble(toy_tasks), is_true() )

  required_cols <- c("id", "cell_ids", "milestone_ids", "milestone_network", "milestone_percentages", "progressions", "counts", "geodesic_dist", "prior_information")
  expect_that( all(required_cols %in% colnames(toy_tasks)), is_true() )

  expect_equal( unique(toy_tasks$type), "ti_toy" )
  ti_types <- eval(formals(generate_toy_datasets)$ti_types)
  expect_true( all(toy_tasks$ti_type %in% ti_types) )
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
    expect_equal( sort(unique(milestone_percentages$milestone_id)), sort(milestone_ids) )
    # todo: check whether a cell is in a tent

    mp_summ <- milestone_percentages %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
    expect_true( all(mp_summ$sum == 1) )

    progressions <- task$progressions
    expect_equal( sort(unique(progressions$cell_id)), sort(cell_ids) )
    expect_equal( sort(unique(c(progressions$from, progressions$to))), sort(milestone_ids) )

    pr_summ <- progressions %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
    expect_true( all(mp_summ$sum <= 1) )

    counts <- task$counts
    expect_true( is.numeric(counts) )
    expect_true( is.matrix(counts) )
    expect_equal( rownames(counts), cell_ids )
    expect_false( is.null(colnames(counts)) )
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
