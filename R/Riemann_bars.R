#' Create dataframe for graphing Riemann bars
#'
#' @param tilde tilde expression defining a function
#' @param h width of bars
#' @param domain as in slice_plot()
#' @param ... parameters for tilde expression
#'
#' @examples
#' Pts <- Riemann_bars(x*dnorm(x) ~ x, h=0.1, domain(x=c(-3,3)))
#' \dontrun{
#' gf_rect(ymin + ymax ~ xmin + xmax, data = Pts,
#' fill=~ color, alpha=0.3, color="black") %>%
#' gf_refine(scale_fill_identity())
#' }
#'
#' @export
Riemann_bars <- function(tilde, h, domain, ...) {
  f <- makeFun(tilde, ...)
  left <- min(domain[[1]])
  right <- max(domain[[1]])
  xpts <- seq(left, right - 0.01*h, by=h)
  ypts <- f(xpts)
  yright <- f(xpts + h)
  yleft <- pmin(0, ypts)
  yright <- pmax(0, ypts)
  Res <- tibble(xmin = xpts, xmax=xpts + h,
                ymin = yleft, ymax=yright,
                color = ifelse(ypts >= 0, "blue", "orange"))

  Res
}
