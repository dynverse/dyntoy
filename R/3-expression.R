rzinbinom <- function(n, mu, size, pi) {
  rval <- rnbinom(n, mu = mu, size = size)
  rval[runif(n) < pi] <- 0
  as.integer(round(rval))
}

sample_zinbinom_expression <- function(
  x,
  mu = runif(1, 1, 1000),
  size = runif(1, mu/10, mu / 4),
  calculate_pi = 0.01
) {
  counts <- map_int(x, ~rzinbinom(1, mu * ., size=size, pi = calculate_pi(mu * .)))

  counts
}

#' Simulate counts which are distributed using a zero-inflated negative biniomal distribution
#'
#' @param trajectory The dynwrap trajectory
#' @param num_features Number of features
#' @param sample_mean_count Function used to sample the mean expression
#' @param sample_dispersion_count Function to sample the dispersion (size) of the negative biniomal given the expression. Higher dispersion values generate less noise
#' @param dropout_probability_factor Factor used to calculate the probabilities of dropouts, relative to expression. Higher values (> 10000) have a lot of dropouts, lower values (< 10) have almost none
generate_counts <- function(
  trajectory,
  num_features,
  sample_mean_count = function() runif(1, 100, 1000),
  sample_dispersion_count = function(mean) map_dbl(mean, ~runif(1, ./10, ./4)),
  dropout_probability_factor = 100
) {
  feature_ids <- paste0("G", seq_len(num_features))

  dimred_trajectory <- dynwrap::dimred_trajectory(trajectory)

  # generate counts
  counts <- map(feature_ids, function(feature_id) {
    # get density in multivariate normal distribution
    limits <- c(
      range(dimred_trajectory$space_milestones$comp_1),
      range(dimred_trajectory$space_milestones$comp_2)
    ) %>% matrix(nrow = 2)

    mean <- runif(2, limits[1, ], limits[2, ])
    sigma <- runif(2, apply(limits, 2, diff) / 10, apply(limits, 2, diff) / 8) %>% diag

    dimred_trajectory$space_samples$density <- mvtnorm::dmvnorm(dimred_trajectory$space_samples[, c("comp_1", "comp_2")], mean, sigma)

    # from density, get expression using a zero-inflated negative binomial
    calculate_pi <- function(x) dexp(x / dropout_probability_factor)
    mean_count <- sample_mean_count()
    dispersion_count <- sample_dispersion_count(mean_count)

    dimred_trajectory$space_samples$counts <- sample_zinbinom_expression(
      dimred_trajectory$space_samples$density,
      mu = mean_count,
      size = dispersion_count,
      calculate_pi = calculate_pi
    )

    dimred_trajectory$space_samples$counts
  }) %>% do.call(cbind, .)
  rownames(counts) <- trajectory$cell_ids
  colnames(counts) <- feature_ids

  counts
}


