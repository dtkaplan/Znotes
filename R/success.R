#' Generates a random success or regret message
#'
#' Just for a bit of variety in multiple-choice feedback.
#'
#' @rdname success
#' @export
random_regret <- function() {
  sample(regrets, size=1)
}

regrets <- c(
  "Sorry!",
  "Not quite.",
  "Not this time.",
  "No.",
  "Wrong",
  "Unh-unh.",
  "False.",
  "Nope"
)

#' @rdname success
#' @export
random_success <- function() {
  sample(success, size=1)
}

success <- c(
  "Right!",
  "Excellent!",
  "Good.",
  "Correct.",
  "Nice!"

)
