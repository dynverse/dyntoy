sample_discrete_uniform <- function(n, from, to) {
  round(stats::runif(n, from - .4999999, to + .4999999))
}
