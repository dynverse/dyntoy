#' @importFrom igraph ba.game
#' @importFrom stats runif
generate_toy_milestone_network <- function(ti_type = c("linear", "bifurcating", "cycle", "consecutive_bifurcating", "trifurcating", "converging", "BA")) {
  requireNamespace("igraph")
  ti_type <- match.arg(ti_type)

  milnet <- switch(
    ti_type,
    linear = {
      tribble(
        ~from, ~to,
        "M1", "M2"
      )
    },
    bifurcating = {
      tribble(
        ~from, ~to, ~length,
        "M1", "M2", 3,
        "M2", "M3", 2,
        "M2", "M4", 2,
        "M3", "M5", 1,
        "M4", "M6", 1
      )
    },
    trifurcating = {
      tribble(
        ~from, ~to, ~length,
        "M1", "M2", 3,
        "M2", "M3", 2,
        "M2", "M4", 2,
        "M2", "M5", 2,
        "M3", "M6", 1,
        "M4", "M7", 1,
        "M5", "M8", 1
      )
    },
    consecutive_bifurcating = tribble(
      ~from, ~to,
      "M1", "M2",
      "M2", "M3",
      "M3", "M4",
      "M3", "M5",
      "M2", "M6"
    ),
    converging = tribble(
      ~from, ~to,
      "M1", "M2",
      "M3", "M4",
      "M2", "M5",
      "M4", "M5",
      "M5", "M6"
    ),
    cycle = {
      tribble(
        ~from, ~to,
        "M1", "M2",
        "M2", "M3",
        "M3", "M1"
      )
    },
    BA = {
      num_milestones <- round(stats::runif(1, 5, 15))
      igraph::ba.game(num_milestones) %>%
        igraph::as_data_frame() %>%
        mutate_at(c("from", "to"), function(x) paste0("M", x)) %>%
        select(from = to, to = from)
    }
  )

  if (!"length" %in% colnames(milnet)) {
    milnet <- milnet %>% mutate(length = 1)
  }

  milnet %>%
    mutate(directed = TRUE) %>%
    as_data_frame()
}
