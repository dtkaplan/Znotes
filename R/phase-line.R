#' Draw a one-dimensional phase space with arrows.
#'
#' @param tilde A tilde expression giving the dynamical function
#' @param domain The domain to show of the graphics, as in `slice_plot()`.
#' @param narrows How many arrows to place on the phase line
#' @param nix_dyn If `TRUE`, don't show the dynamical function.
#' @param transform A function to use to make the short arrows
#' long enough to stand out. Try `transform=sqrt`.
#'
#' @examples
#' phase_line(4*x*(1-x) ~ x, domain(x=0:1),
#'    nix_dyn=FALSE, narrows=20, transform=sqrt)
#'
#' @export
phase_line <- function(tilde, domain, narrows=15,
                       nix_dyn = FALSE, transform=I) {
  dyn <- makeFun(tilde)
  vname <- as.character(tilde[[3]])
  min_x <- min(domain[[1]])
  max_x <- max(domain[[1]])
  arrow_offset <- (max_x - min_x)/(3*narrows)
  arrow_x <- seq(min_x + arrow_offset, max_x - arrow_offset,
                 length=narrows)
  arrow_step <- dyn(arrow_x)
  max_len <- max(abs(arrow_step))
  arrow_step <- arrow_step/max_len
  arrow_step <- arrow_step * (max_x - min_x)/(1.0*narrows)
  ypts <- dyn(seq(min_x, max_x, length=200))
  yrange <- range(ypts) %>% extendrange()
  ymin <- pmin(0, min(yrange))
  ymax <- pmax(0, max(yrange))
  Arrows <- tibble(
    xstart = arrow_x,
    xend = arrow_x + sign(arrow_step)*transform(abs(arrow_step)),
    ystart = 0,
    yend = 0
  )

  if (nix_dyn) {
    P <- ggplot()
    the_theme <- theme_void()
  } else {
    P <- slice_plot(dyn(x) ~ x, domain)
    the_theme <- theme_minimal()
  }
  P %>%
    gf_lims(y = c(ymin, ymax)) %>%
    gf_hline(yintercept = ~ 0, color="gray", size=3, alpha=.5) %>%
    gf_segment(ystart + yend ~ xstart + xend, data = Arrows,
               color="blue", arrow = grid::arrow(end="last", length=unit(.6, "mm"), type="closed")) %>%
    gf_theme(the_theme) %>%
    gf_labs(y = paste0("d", vname, "/dt"))
}
