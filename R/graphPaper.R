#' Traditional graph-paper axes
#'
#' \code{graphPaper} plots out traditional graph-paper axes for making
#' mathematical plots.  Whereas standard R graphics put the axes at the
#' edge of the plotting window, \code{graphPaper} puts them through the zeros
#' and lets you set explicitly the location of tick marks and graph-paper rules.
#' You can then plot over the axes by using functions such as \code{lines},
#' \code{points}, or \code{funPlot} (with \code{add=TRUE}).
#'
#' @name graphPaper
#'
#' @param xticks numerical vector listing the position of x-axis ticks
#' @param yticks like \code{xticks} but for y-axis
#' @param xlabels as in \code{plot}, sets the y-axis label
#' @param ylabels similarly, set the y-axis label
#' @param xlab Label for horizontal axis
#' @param ylab Label for vertical axis
#'
#'
#' @examples
#' graph_paper(xticks=-5:5, yticks=seq(-1, 1, by=.25),
#'             ylabels = c(-1, 1)) %>%
#'   graphFun(sin((x^2) / 3)~x, x=range(-4, 4), color="blue")
#'
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' @return a ggplot2 object
#' @export
graph_paper = function(P = NULL, xticks=0:5, yticks=-0:5,
                       xlabels = xticks, ylabels = yticks,
                       extend = 0.10,
                       xlim=c(min(xticks), max(xticks)),
                       ylim=c(min(yticks), max(yticks)),
                       xlab="x",
                       ylab="y", maxxlabels=7, maxylabels=7, ...) {
  extendx <- extend * diff(range(xticks))
  extendy <- extend * diff(range(yticks))
  horiz <- tibble(
    y = yticks,
    y2 = yticks,
    x = rep(min(xticks), length(yticks)) - extendx/2,
    x2 = rep(max(xticks), length(yticks)) + extendx/2
  )
  vertical <- tibble(
    y = rep(min(yticks), length(xticks)) - extendy/2,
    y2 = rep(max(yticks), length(xticks)) + extendy/2,
    x = xticks,
    x2 = xticks
  )
  Xlabels <- tibble(
    x = -extendx/3, y = ylabels[ylabels != 0], label = as.character(y)
  )
  Ylabels <- tibble(
    x = xlabels[xlabels != 0], y = -extendy/3, label = as.character(x)
  )
  Labels <- bind_rows(Xlabels, Ylabels)

  Grid <- bind_rows(horiz, vertical)
  arrow_head <- arrow(ends="both",  type = "closed",
                      length = unit(0.1, "inches"))

  P %>% gf_segment(y + y2 ~ x + x2, data = Grid, alpha = 0.3) %>%
    gf_theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.title.y = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             panel.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border = element_blank(),
             aspect.ratio = 1,
    ) %>%
    gf_refine(scale_y_continuous(breaks = yticks),
              scale_x_continuous(breaks = xticks)) %>%
    gf_segment(min(yticks-extendy) + max(yticks+extendy) ~ 0 + 0,
               color = "black", arrow = arrow_head) %>%
    gf_segment(0 + 0 ~ min(xticks - extendx) + max(xticks + extendx),
               color = "black",
               arrow = arrow_head) %>%
    gf_point(y ~ x, data = Labels, size=5, alpha=.8, color="white", inherit=FALSE) %>%
    gf_text(y ~ x, label = ~ label, data = Labels, inherit=FALSE, alpha = 0.6)

}

#' @export
gvec <- function(P = NULL, from, to, color="blue", size=1.2, alpha=1) {
  df <- tibble(x = from[1], xend = to[1], y = from[2], yend = to[2])
  A <- arrow(ends="last", type="closed", length=unit(0.125, "inches"))
  if (inherits(P, "ggplot")) {
    P %>% gf_segment(y + yend ~ x + xend, data=df, color=color, arrow = A, size=size, alpha=alpha)
  } else {
    gf_segment(y + yend ~ x + xend, data=df, color=color, arrow = A, size=size, alpha=alpha)
  }
}
