#' Simulate counts which are distributed using a zero-inflated negative biniomal distribution
#'
#' @param trajectory The dynwrap trajectory
#' @param num_features Number of features
#' @param sample_mean_count Function used to sample the mean expression
#' @param sample_dispersion_count Function to sample the dispersion (size) of the negative biniomal given the expression. Higher dispersion values generate less noise
#' @param dropout_probability_factor Factor used to calculate the probabilities of dropouts, relative to expression. Higher values (> 10000) have a lot of dropouts, lower values (< 10) have almost none
#' @param dropout_rate Base rate of drop-outs
#' @param differentially_expressed_rate Percentage of differentially expressed genes
generate_counts <- function(
  trajectory,
  num_features = 101,
  sample_mean_count = function() runif(1, 100, 1000),
  sample_dispersion_count = function(mean) map_dbl(mean, ~runif(1, ./10, ./4)),
  dropout_probability_factor = 100,
  dropout_rate = 0.2,
  differentially_expressed_rate = 1
) {
  feature_ids <- paste0("G", seq_len(num_features))

  dimred_trajectory <- dynwrap::calculate_trajectory_dimred(trajectory)

  # generate counts
  count_generation_results <- map(feature_ids, function(feature_id) {
    # get density in multivariate normal distribution
    limits <- c(
      range(dimred_trajectory$milestone_positions$comp_1),
      range(dimred_trajectory$milestone_positions$comp_2)
    ) %>% matrix(nrow = 2)

    mean <- runif(2, limits[1, ], limits[2, ])
    sigma <- runif(2, apply(limits, 2, diff) / 10, apply(limits, 2, diff) / 8) %>% diag

    dimred_trajectory$cell_positions$density <- mvtnorm::dmvnorm(dimred_trajectory$cell_positions[, c("comp_1", "comp_2")], mean, sigma)

    # from density, get expression using a zero-inflated negative binomial
    calculate_pi <- function(x) dexp(x / dropout_probability_factor, rate = dropout_rate)
    mean_count <- sample_mean_count()
    dispersion_count <- sample_dispersion_count(mean_count)

    # if gene is differentially expressed, use densities, otherwise shuffle the densities
    x <- dimred_trajectory$cell_positions$density
    differentially_expressed <- runif(1) <= differentially_expressed_rate
    if (!differentially_expressed) x <- sample(x)

    dimred_trajectory$cell_positions$counts <-
      sample_zinbinom_expression(
        x,
        mu = mean_count,
        size = dispersion_count,
        calculate_pi = calculate_pi
      )

    lst(
      counts = dimred_trajectory$cell_positions$counts,
      tde_overall = tibble(feature_id = feature_id, differentially_expressed = !!differentially_expressed)
    )
  })

  counts <- map(count_generation_results, "counts") %>% do.call(cbind, .)
  tde_overall <- map_df(count_generation_results, "tde_overall")

  rownames(counts) <- trajectory$cell_ids
  colnames(counts) <- feature_ids

  lst(
    counts,
    tde_overall
  )
}

rzinbinom <- function(n, mu, size, pi) {
  rval <- rnbinom(n, mu = mu, size = size)
  rval[runif(n) < pi] <- 0
  as.integer(round(rval))
}

sample_zinbinom_expression <- function(
  x,
  mu = runif(1, 1, 1000),
  size = runif(1, mu/10, mu / 4),
  calculate_pi = function() 0.1
) {
  counts <- map_int(x, ~rzinbinom(1, mu * ., size=size, pi = calculate_pi(mu * .)))

  counts
}





