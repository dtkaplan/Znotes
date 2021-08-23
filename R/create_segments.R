#' Create a data frame for plotting piecewise-linear segments
#'
#' Functions can be approximated by a sequence of piecewise-linear
#' segments. This function generates the information needed for
#' plotting those segments, as you might if the piecewise linear approximation
#' were plotted on top of the slice_plot() of the function.
#'
#' @param ftilde a tilde expression, as in `slice_plot()`
#' @param domain specification of a domain, as in `slice_plot()`
#' @param nsegs how many segments to divide the domain into
#' @param h an alternative to `nsegs`, giving the length of each segment.
#' @return a data frame (see examples)
#'
#' @examples
#' \dontrun{
#' fun <- sin(x) ~ x
#' dom <- domain(x = c(-5,5))
#' segments <- create_segments(fun, domain=dom, nsegs=20)
#' slice_plot(fun, domain=dom) %>%
#'   # show the slope function as slopes
#'   gf_segment(y + yend ~ x + xend, data = segments, color=~slope) %>%
#'   gf_refine(scale_color_viridis_c())
#'
#' slice_plot(fun, domain=dom) %>%
#'   # piecewise linear approximations
#'   gf_segment(yf + yfend ~ x + xend, data = segments, color=~slope,
#'   size=2, alpha=0.5) %>%
#'   gf_refine(scale_color_viridis_c())
#'
#' slice_plot(fun, domain=dom) %>%
#'   # show the approximate slope function
#'   gf_line(slope ~ x , data = segments, color=~slope) %>%
#'   gf_refine(scale_color_viridis_c())
#'
#' }
#'
#' @export
create_segments <- function(ftilde, domain, h=NULL, nsegs=20) {
  f <- makeFun(ftilde)
  df <- D(ftilde)
  vname <- all.vars(ftilde[[3]])
  if (is.null(h)) h <- base::diff(domain[[1]])/nsegs
  start <- seq(domain[[1]][1] + h/2, domain[[1]][2], by=h )
  slopes <- df(start)
  offsets <- f(start)

  res <- tibble(x = start - h/2,
                xend = x + h,
                start = start,
                y = -slopes*h/2.2,
                yend = slopes*h/2.2,
                slope = slopes,
                offset = offsets,
                yf = y + offset,
                yfend = yend + offset,
                n = 1:length(x))

  res
}
