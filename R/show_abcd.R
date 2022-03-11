#' Draw ABCD linear dynamics phase plane flow
#'
#' @param a, b, c, d: Four components of the dynamics matrix.
#' @param which Which variable(s) to show the positive side of the nullcline in color
#' @param show_sign When `TRUE`, show the flow vectors all the same length
#' @param pow Exponent to use for display length versus actual length of a flow vector
#'
#' @details `pow` can be used to make the very short flow vectors near the origin look
#' bigger. Default is 0.3.
#'
#' @examples
#' Znotes::show_abcd(-.5, -1, 1, .7)
#'
#' @export
show_abcd <- function(a, b, c, d, which=c("both", "x", "y"),
                      show_sign=FALSE, pow=0.3) {
  which <- match.arg(which)
  show_sign <- ifelse(show_sign, sign, I)
  dx <- makeFun(a*x + b*y ~ x & y)
  dy <- makeFun(c*x + d*y ~ x & y)

  P <- NULL
  if (which %in% c("both", "x")) {
    P <- P %>%
      inequality_constraint(dy(x,y) < 0 ~ x&y,
                            fill="blue", alpha=0.3,
                            domain(x=-1:1, y=-1:1))
  }
  if (which %in% c("both", "y")) {
    P <- P %>% inequality_constraint(dx(x,y) < 0 ~ x&y,
                                     fill="red", alpha=0.3,
                                     domain(x=-1:1, y=-1:1))
  }
  P %>% vectorfield_plot((show_sign(dx(x,y))) ~ x&y, show_sign(dy(x,y)) ~ x & y, transform=function(x) x^pow) %>%
    gf_refine(coord_fixed())
}
