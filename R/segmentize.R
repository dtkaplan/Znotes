#' Break a function into segments, giving offset and relative values
#'
#'
#' @examples
#' Pts <- segmentize(x*dnorm(x) ~ x, .1, list(x=c(-3,3)))
#' \dontrun{
#' gf_segment(ymin + ymax ~ xmin + xmax, data = Pts)
#' }
#' @export
segmentize <- function(tilde, h, domain, npts=10, ...) {
  f <- makeFun(tilde, ...)
  xmin <- min(domain[[1]])
  xmax <- max(domain[[1]])
  nsegs <- ceiling((xmax-xmin)/h)
  xpts <- seq(xmin, xmax, length=nsegs*npts)
  ypts <- f(xpts)
  inds <- npts*((0:(length(xpts))) %/% npts) + 1
  segment_number <- (0:(length(xpts))) %/% npts
  startx <- xpts[inds[1:length(xpts)]]
  starty <- f(startx)[-length(ypts)]
  ymin <- ypts[-length(ypts)]
  ymax <- ypts[-1]
  xmin <- xpts[-length(xpts)]
  xmax <- xpts[-1]
  tibble(xmin = xmin, xmax=xmax,
         ymin = ymin - starty, ymax=ymax-starty,
         offset=starty, segnum = segment_number[1:length(xmin)] )
}
