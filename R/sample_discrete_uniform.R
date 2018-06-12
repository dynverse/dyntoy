sample_discrete_uniform <- function(n, from, to) {
  floor(stats::runif(n, from, to + .9999))
}
