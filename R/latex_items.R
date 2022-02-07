#' Construct latex markup for vectors
#'
#' @param n How many components
#' @param digits Number of digits for integer vector
#' @param v and alternative way to specify the numbers in the vectors
#' @export
latex_vec <- function(n=3, name="v", digits=3, seed=NULL, v=NULL) {
  if (!is.null(v)) vec <- v
  else {
    if (!is.null(seed))set.seed(seed)
    v <- runif(n, -1, 1)
    vec <- round(v * 10^digits)
  }
  pre <- ifelse(vec > 0, "\\ ", "")
  components <- paste(pre, vec, collapse="\\\\")
  paste0(r"(\vec{)",name,r"(} \equiv \left[\begin{array}{r})", components, r"(\end{array}\right])")
}
